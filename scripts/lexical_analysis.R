# ---+
# Analyse lexicale globale
# Axelle Penture
# Version 2.0 - 102.06.2025
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

dtm <- create_dtm(it, vectorizer) # matrice terme-doc

# Entraînement Word2Vec (skip-gram) # to change on est avec text2vec
set.seed(123)
w2v_model <- word2vec(x = tokens, type = "skip-gram", window = 10, dim = 50, iter = 20)

# Trouver mots proches de "Minerva"
nearest <- predict(w2v_model, newdata = "minerva", type = "nearest", top_n = 10)
print(nearest)

# Collocation 

bigrams <- pers_contexts %>%              # Tokenisation en bigrammes
  unnest_tokens(bigram, context, token = "ngrams", n = 2)

bigram_counts <- bigrams %>%       # Fréquences des bigrammes
  count(bigram, sort = TRUE) %>%
  filter(n > 5)

print(head(bigram_counts, 20))

## filtrage sur la PMI (Pointwise Mutual Information)

bigram_pmi <- bigrams %>% 
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  bind_tf_idf(word1, word2, n) %>%
  arrange(desc(tf_idf))

print(head(bigram_pmi, 20))

## Co-occurrences de 20 mots 
tokens_window <- pers_contexts %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, context)

cooccurrence <- tokens_window %>%
  pairwise_count(word, id, window = 20, sort = TRUE)

print(head(cooccurrence, 20))

# Visualisation en réseau 
plot_word_network <- function(ref_name, min_cooc = 5) {
  subset_tokens <- pers_contexts %>%
    filter(ref == ref_name) %>%
    mutate(id = row_number()) %>%
    unnest_tokens(word, context)
  
  cooc <- subset_tokens %>%
    pairwise_count(word, id, window = 20, sort = TRUE) %>%
    filter(n >= min_cooc)
  
  graph <- graph_from_data_frame(cooc)
  
  set.seed(123)
  ggraph(graph, layout = "fr") +
    geom_edge_link(aes(width = n), alpha = 0.8, color = "darkgrey") +
    geom_node_point(size = 5, color = "steelblue") +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void() +
    ggtitle(paste("Réseau de co-occurrences pour", ref_name))
}

# Exemples de plots
plot_word_network("MIN")
plot_word_network("APO")
plot_word_network("IUP")












