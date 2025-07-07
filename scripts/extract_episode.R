#---+
# Pour une recherche ciblée dans les données 'data'
# Axelle Penture
# Version 1.0 - 05.07.2025
# ---+

# Chargement des données
library(xml2)
library(dplyr)
library(stringr)
library(purrr)

source("scripts/data_deities.R")
ovid_deities <- read_xml("https://raw.githubusercontent.com/pax3l/m2_ovid_deities_analysis_for_quarto/refs/heads/main/data/ovid_MIN_APO_IUP.xml")

# Pour garder le fil de l'écriture et créer un docuement propre à part entière, nous reprenons ici des éléments déjà présents dans le script 'data_deities.R'

ns <- c(tei = "http://www.tei-c.org/ns/1.0")
# Fonction principale d'extraction d'informations contextuelles à partir des balises <persName>
extract_episode <- function(xml_doc, ns = c(tei = "http://www.tei-c.org/ns/1.0")) {
  
  pers_nodes <- xml_find_all(xml_doc, ".//tei:persName", ns = ns)
  
  data_plus <- map_df(pers_nodes, function(node) {
    
  # Extraction des éléments de contexte
    livre_node   <- xml_find_first(node, "ancestor::tei:div[@type='textpart' and @subtype='book']", ns)
    episode_node <- xml_find_first(node, ".//preceding::tei:milestone[@unit='tale'][1]", ns)
    verse_node   <- xml_find_first(node, "ancestor::tei:l", ns)
    
  # Attributs TEI
    livre   <- xml_attr(livre_node, "n")
    episode <- xml_attr(episode_node, "n")
    vers    <- xml_attr(verse_node, "n")
    
    ref     <- xml_attr(node, "ref")
    type    <- xml_attr(node, "type")
    ana     <- xml_attr(node, "ana")
    mention <- xml_text(node)
    
    # Construction de la ligne de données
    tibble(
      livre = as.integer(livre),
      episode = episode,
      vers = as.integer(vers),
      ref = ref,
      type = type,
      ana = ana,
      mention = mention
    )
  })
  
  return(data_plus)
}

# Extraction et fusion avec le data.frame existant
data_plus_ext <- extract_episode(ovid_deities)
data_plus_ext
# print(data_plus_ext, n=200)
# data_plus_ext %>% filter(episode == "THE NINE MUSES AND PYRENAEUS")

# Tableau des épisodes, nouvelle données extraite dans ce document
## Extraction des épisodes avec leurs bornes de vers
extract_episodes_range <- function(xml_doc, ns = c(tei = "http://www.tei-c.org/ns/1.0")) {
  
  milestones <- xml_find_all(xml_doc, ".//tei:milestone[@unit='tale']", ns)
  
  map_df(milestones, function(milestone) {
    
    episode <- xml_attr(milestone, "n")
    livre_node <- xml_find_first(milestone, "ancestor::tei:div[@type='textpart' and @subtype='book']", ns)
    livre <- xml_attr(livre_node, "n") %>% as.integer()
    
    # Début = premier vers après milestone
    vers_debut_node <- xml_find_first(milestone, ".//following::tei:l[1]", ns)
    vers_debut <- xml_attr(vers_debut_node, "n") %>% as.integer()
    
    # Fin = dernier vers avant le milestone suivant
    next_milestone <- xml_find_first(milestone, ".//following::tei:milestone[@unit='tale'][1]", ns)
    if (!is.na(next_milestone)) {
      vers_fin_node <- xml_find_first(next_milestone, ".//preceding::tei:l[1]", ns)
    } else {
      vers_fin_node <- xml_find_first(milestone, ".//following::tei:l[last()]", ns)
    }
    vers_fin <- xml_attr(vers_fin_node, "n") %>% as.integer()
    
    tibble(
      livre = livre,
      episode = episode,
      vers_debut = vers_debut,
      vers_fin = vers_fin
    )
  }) %>%
    arrange(livre, vers_debut) %>%
    mutate(
      episode_id = paste0("L", livre, "_E", row_number())
    )
}

episodes_table <- extract_episodes_range(ovid_deities)
episodes_table <- episodes_table %>%
  mutate(episode = str_to_sentence(episode))

print(episodes_table, n=127)
# episodes_table %>%
  # filter(episode == "Peleus apud Ceycem.")

# episodes_table %>%
  # filter(episode == "Callisto.")

episodes_table %>%
 filter(episode == "Io. Argus. Syrinx.")

# COMMENTAIRES ET EXEMPLES D’UTILISATION 
# Le data.frame `data` contient une ligne par mention de divinité, avec :
# - livre   : numéro du livre (int)
# - episode : identifiant d'épisode narratif (string ou NA)
# - vers    : numéro de vers (string ou NA)
# - ref     : identifiant de divinité ("MIN", "APO", "IUP")
# - type    : "narrative" ou "metamorphosis"
# - ana     : rôle dans l'épisode ("act", "obj", "auto", "NA")
# - mention : contenu textuel de la balise <persName>

# Exemples de requêtes

## Mentions de MIN (Athéna) dans le Livre 2
data_plus_ext %>% 
  filter(livre == 2, ref == "MIN")

## Toutes les métamorphoses dans l’épisode 'Lycaon'
data_plus_ext %>% 
  filter(episode == "Lycaon", type == "metamorphosis")

## Mentions agissantes (ana == "act") de toutes les divinités
data_plus_ext %>% 
  filter(ana == "act")

## Compter les mentions par épisode, ou n est le nomrbe de mentions 
data_plus_ext %>% 
  count(episode, sort = TRUE)

## Toutes les mentions de type "narrative" du livre 1
narratives_livre1 <- data_plus_ext %>% 
  filter(type == "narrative", livre == 1)

# Manque la fonction
# export_query(narratives_livre1, "exports/narratives_livre1.csv")

# Fonction pour requêtes dynamiques !
#V1, ne renvoit que le data.frame entier peu importe la demande
# query_data_plus <- function(ref = NULL, type = NULL, ana = NULL, livre = NULL, vers = NULL, episode = NULL, mention_regex = NULL) {
  
 # result <- data_plus_ext
  
 # if (!is.null(ref)) {
 #   result <- filter(result, .data$ref %in% ref)
 # }
 # if (!is.null(type)) {
 #   result <- filter(result, .data$type %in% type)
#  }
 # if (!is.null(ana)) {
 #   result <- filter(result, .data$ana %in% ana)
 # }
#  if (!is.null(livre)) {
 #   result <- filter(result, .data$livre %in% livre)
 # }
#  if (!is.null(vers)) {
#    result <- filter(result, .data$vers %in% vers)
#  }
#  if (!is.null(episode)) {
#    result <- filter(result, .data$episode %in% episode)
#  }
#  if (!is.null(mention_regex)) {
#    result <- filter(result, grepl(mention_regex, .data$mention, ignore.case = TRUE))
# }
  
#  return(result)
#} 

#query_data_plus(ref = "APO", ana = "act", livre = 11)

# Exemples 
# 1.Toutes les mentions de type narrative avec ref = "MIN"
#query_data_plus(ref = "MIN", type = "narrative")

# 2.Toutes les mentions dans le livre 5
#query_data_plus(livre = 5)

# 3.Mentions dans l’épisode “THE NINE MUSES AND PYRENAEUS”
#query_data_plus(episode = "THE NINE MUSES AND PYRENAEUS")

# 4.Mentions où le texte contient "Athena"
#query_data_plus(mention_regex = "Athena")

# 5.Exporter les métamorphoses avec ana = "obj"
# export_query_to_csv(query_data_plus(type = "metamorphosis", ana = "obj"), "metamorphose_obj.csv")

## Même fonction mais dynamique ! (qui fonctionne!)
query_data_plus_interactive <- function() {
  
  if (!exists("data_plus_ext")) {
    stop("La table 'data_plus_ext' n'existe pas dans l'environnement global.")
  }
  
  result <- data_plus_ext
  
  ask_filter <- function(name, col_class) {
    prompt <- paste0("Entrer une valeur pour ", name, " (ou NULL pour ne pas filtrer) : ")
    input <- readline(prompt)
    
    if (tolower(input) == "null" || input == "") {
      return(NULL)
    }
    
    values <- unlist(strsplit(input, ","))
    values <- trimws(values) # enlève les espaces
    
    if (col_class %in% c("integer", "numeric")) {
      values <- as.integer(values)
    }

    return(values)
  }
  
  filters <- list(
    ref = class(result$ref),
    type = class(result$type),
    ana = class(result$ana),
    livre = class(result$livre),
    vers = class(result$vers),
    episode = class(result$episode)
  )
  
  for (name in names(filters)) {
    vals <- ask_filter(name, filters[[name]])
    if (!is.null(vals)) {
      if (name == "episode") {
        result <- filter(result, .data[[name]] %in% vals)
      } else if (name == "mention_regex") {
      } else {
        result <- filter(result, .data[[name]] %in% vals)
      }
    }
  }
  
  mention_regex <- readline("Entrer un regex pour filtrer la colonne 'mention' (ou NULL pour ne pas filtrer) : ")
  if (!(tolower(mention_regex) %in% c("null", ""))) {
    result <- filter(result, grepl(mention_regex, mention, ignore.case = TRUE))
  }
  
  # Afficher résultats
  cat(nrow(result), " lignes correspondent à votre requête.\n")
  
  print(result)
  
  return(result)
}

resultat <- query_data_plus_interactive()

# Exports ?
## Fonction d’export CSV
#export_query_to_csv <- function(query_result, filename = "extraction_episode.csv") {
 # write.csv(query_result, file = filename, row.names = FALSE, fileEncoding = "UTF-8")
#}
## Fonction export des requêtes ou du tableau enrichi
#export_query <- function(df, filename = "query_export.csv") {
#  write.csv(df, filename, row.names = FALSE, fileEncoding = "UTF-8")
#}

# ---------------------------------------------
