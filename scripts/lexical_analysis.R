# ---+
# Analyse lexicale globale
# Axelle Penture
# Version 2.0 - 27.06.2025
# ---+

# Librairies 
if (!requireNamespace("textdata", quietly = TRUE)) {
  install.packages("textdata", repos = "https://cran.rstudio.com")
}
if (!requireNamespace("text2vec", quietly = TRUE)) {
  install.packages("text2vec", repos = "https://cran.rstudio.com")
}

library(xml2)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(textdata)      
library(textreuse)     
library(text2vec) 
library(readxl)       
library(stringr)
library(widyr)
library (purrr)

# Charger le script & les données
source("scripts/data_deities.R")
ovid_deities <- read_xml("https://raw.githubusercontent.com/pax3l/m2_ovid_deities_analysis_for_quarto/refs/heads/main/data/ovid_MIN_APO_IUP.xml")

# Préparation des données 

# 1. Extraction des contextes
#xml_name(xml_parent(pers_nodes[[1]]))  # donne le nom du parent immédiat
#xml_find_first(pers_nodes[[1]], "ancestor::*", ns = tei_ns) |> xml_name()
#table(xml_name(xml_parent(pers_nodes)))

pers_nodes
contextes <- vapply(pers_nodes, function(n) {
  l <- xml_find_first(n, "ancestor::tei:l", ns = tei_ns) # ou tei:p
  if (!is.na(l) && length(l) > 0) xml_text(l) else NA_character_
}, character(1))

milestones <- vapply(pers_nodes, function(n) {
  ms <- xml_find_first(n, "preceding::tei:milestone[1]", ns = tei_ns)
  if (!is.na(ms) && length(ms) > 0) xml_attr(ms, "n") else NA_character_
}, character(1))

# 2. Extraction des "ref"

refs <- xml_attr(pers_nodes, "ref")

# 3. Extraction des livres
livres <- vapply(pers_nodes, function(n) {
  div <- xml_find_first(n, "ancestor::tei:div[@type='textpart' and @subtype='book']", ns = tei_ns)
  if (!is.na(div) && length(div) > 0) xml_attr(div, "n") else NA_character_
}, character(1))

# 4. Création du tibble avec filtrage
pers_contexts <- tibble(
  context = contextes,
  ref = xml_attr(pers_nodes, "ref"),
  livre = vapply(pers_nodes, function(n) {
    div <- xml_find_first(n, "ancestor::tei:div[@type='textpart' and @subtype='book']", ns = tei_ns)
    if (!is.na(div)) xml_attr(div, "n") else NA_character_
  }, character(1)),
  section = milestones
) %>%
  filter(!is.na(context), !is.na(ref), !is.na(livre), context != "") %>%
  mutate(livre = as.integer(livre))

it <- itoken(
  pers_contexts$context,
  preprocessor = tolower,
  tokenizer = word_tokenizer,
  progressbar = TRUE
)

# Analyse lexicale globale
corpus <- pers_contexts$context
tokens <- word_tokenizer(tolower(corpus)) # Tokenization 

vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = 5) # filtre

vectorizer <- vocab_vectorizer(vocab) # vectorisation

tcm <- create_tcm(it, vectorizer, skip_grams_window = 10L) # matrice terme-cooccurrence

glove <- GlobalVectors$new(rank = 50, x_max = 10) # modèle Word2Vec via `GlobalVectors` (proche du skip-gram par structure)
w2v_model <- glove$fit_transform(tcm, n_iter = 20)

context_vectors <- glove$components # combinaison matrice pour vecteurs finaux
word_vectors <- w2v_model + t(context_vectors)

"apolline" %in% rownames(word_vectors)
"phoebe" %in% vocab$term

## Application aux divinités
### Fonction de distance cosinus
cos_sim <- sim2(x = word_vectors, y = word_vectors["apolline", , drop = FALSE], method = "cosine", norm = "l2")

# Voir les 10 plus proches voisins
nearest <- sort(cos_sim[,1], decreasing = TRUE)[2:11] # [1] est "apolline" lui-même
print(nearest)

# Collocation 

bigrams <- pers_contexts %>%              # Tokenisation en bigrammes
  unnest_tokens(bigram, context, token = "ngrams", n = 2)

bigram_counts <- bigrams %>%       # Fréquences des bigrammes
  count(bigram, sort = TRUE) %>%
  filter(n > 5)

print(head(bigram_counts, 20)) 

## Filtrage sur la PMI (Pointwise Mutual Information)

bigram_pmi <- bigrams %>% 
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  bind_tf_idf(word1, word2, n) %>%
  arrange(desc(tf_idf))

print(head(bigram_pmi, 20))


## Co-occurrences du vers prcédent au vers suivant 

get_parent_l <- function(node) {
  xml_find_first(node, "ancestor::tei:l", ns = tei_ns)
}

get_siblings_l <- function(l_node) {
  prev_l <- xml_find_first(l_node, "preceding-sibling::tei:l[1]", ns = tei_ns)
  next_l <- xml_find_first(l_node, "following-sibling::tei:l[1]", ns = tei_ns)
  list(prev = prev_l, current = l_node, following = next_l)
}

contexts_l <- map_chr(pers_nodes, function(n) { # extraction ctxt avant/pendant/après
  l_node <- get_parent_l(n)
  if (is.na(l_node)) return(NA_character_)
  siblings <- get_siblings_l(l_node)
  
  # Extraire texte en concaténant les trois balises (si elles existent)
  texts <- c(
    if (!is.na(siblings$prev)) xml_text(siblings$prev) else NULL,
    xml_text(siblings$current),
    if (!is.na(siblings$following)) xml_text(siblings$following) else NULL
  )
  
  paste(texts, collapse = " ")
})

pers_contexts_l <- tibble(
  context = contexts_l,
  ref = xml_attr(pers_nodes, "ref"),
  livre = vapply(pers_nodes, function(n) {
    div <- xml_find_first(n, "ancestor::tei:div[@type='textpart' and @subtype='book']", ns = tei_ns)
    if (!is.na(div)) xml_attr(div, "n") else NA_character_
  }, character(1))
) %>%
  filter(!is.na(context), !is.na(ref), !is.na(livre)) %>%
  mutate(livre = as.integer(livre))


tokens_window <- pers_contexts_l %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, context)

cooccurrence <- tokens_window %>%
  pairwise_count(word, id, sort = TRUE)

print(head(cooccurrence, 20))

# Visualisation en réseau 
plot_word_network <- function(ref_value, min_cooc = 2, node_color = "steelblue") {
  subset_tokens <- pers_contexts %>%
    filter(ref == ref_value) %>%
    mutate(id = row_number()) %>%
    unnest_tokens(word, context)
  
  cooc <- subset_tokens %>%
    pairwise_count(word, id, sort = TRUE) %>%
    filter(n >= min_cooc)
  
  graph <- igraph::graph_from_data_frame(cooc)
  
  set.seed(123)
  layout <- igraph::layout_with_fr(graph, niter = 1500)
  ggraph::ggraph(graph, layout = layout) +
    ggraph::geom_edge_link(aes(width = n, color = n), alpha = 0.8, color = "grey", edge_curvature = 0.2) +
    ggraph::geom_node_point(size = 2, color = node_color, alpha = 0.8) +
    ggraph::geom_node_text(aes(label = name), repel = TRUE, size = 3, force = 5, max.overlaps = 10) +
    scale_edge_color_gradientn(colors = c("#0000FF", "#FFA500", "#FF0000")) +
    theme_void() +
    #ggtitle(paste("Réseau de co-occurrences pour", ref_value)) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),  
      legend.title = element_text(face = "bold"),
      panel.border = element_rect(colour = "black", fill = NA, size = 1)
    )
}

# Exemples de plots
plot_word_network("APO", node_color = "#FFB3D9")
plot_word_network("IUP", node_color = "#A3BFFF" )
plot_word_network("MIN", node_color = "#C8B9F7")













