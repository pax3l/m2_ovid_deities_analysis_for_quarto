# ---+
# Distribution des mentions de MIN, APO et IUP dans les Métamorphoses
# Axelle Penture
# Version 1.0 - 14.06.2025
# ---+

#Si nécessaire, installation des paquets
install.packages("xml2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")

# Chargement des librairies
library(xml2)
library(dplyr)
library(tidyr)
library(ggplot2)

# Chargement du texte d'Ovide depuis le dépôt en ligne 
ovid_deities <- read_xml(
  "https://raw.githubusercontent.com/pax3l/m2_ovid_deities_analysis_for_quarto/refs/heads/main/data/ovid_MIN_APO_IUP.xml"
)
ovid_deities

# Vérifier l'espace de noms
# Définir l'espace de noms TEI
tei_ns <- c(tei = "http://www.tei-c.org/ns/1.0")

# Vérifier la structure XML avec l'espace de noms TEI, et le nombre de 
print(xml_find_all(ovid_deities, ".//tei:persName", ns = tei_ns) %>% length())  
# print(xml_find_all(ovid_deities, ".//tei:*", ns = tei_ns) %>% length())

# Afficher les résultats
afficher_resultats <- function(repartition, titre = "RÉPARTITION") {
  cat("\n=== ", titre, " ===\n")
  
  # Nombre total de mentions
  total_mentions <- sum(repartition$count)
  cat("Total mentions:", total_mentions, "\n")
  
  # Afficher la répartition par livre
  cat("\nRépartition par livre:\n")
  print(repartition)
  
  # Afficher la répartition avec pourcentage
  print(repartition %>%
    mutate(pourcentage = round(count/total_mentions*100, 1)) %>%
    select(livre, attribut, count, pourcentage) %>%
    rename(Attribut = !!sym(attribut))
  )
}

# Fonction pour extraire les données persName avec leurs attributs
extrait_persname <- function(ovid_deities) {
  # Recherche de tous les éléments persName avec l'espace de noms TEI
  pers_nodes <- xml_find_all(ovid_deities, ".//tei:persName", ns = tei_ns)
  
  # Extraction des attributs et du texte
  data <- data.frame(
    texte = xml_text(pers_nodes),
    ref = xml_attr(pers_nodes, "ref"),
    role = xml_attr(pers_nodes, "role"),
    ana = xml_attr(pers_nodes, "ana"),
    stringsAsFactors = FALSE
  )
  
  # Ajout des informations de livre pour chaque persName
  livres <- c()
  for (node in pers_nodes) {
    # Recherche de l'élément parent avec type="textpart" et subtype="book" en utilisant l'espace de noms TEI
    livre_parent <- xml_find_first(node, "ancestor::tei:div[@type='textpart' and @subtype='book']", ns = tei_ns)
    if (!is.na(livre_parent)) {
      numero_livre <- as.numeric(xml_attr(livre_parent, "n"))
      livres <- c(livres, numero_livre)
    } else {
      livres <- c(livres, NA)
    }
  }
  
  data$livre <- livres
  data$livre <- as.numeric(data$livre)
  return(data)
}
resultats1 <- extrait_persname(ovid_deities)
head(resultats1) 

# Analyser la répartition générale pour tous les attributs
repartition_generale <- ana_repartition(resultats1, attribut = "ref")
afficher_resultats(repartition_generale, "RÉPARTITION GÉNÉRALE")

# Extraire les résultats pour MIN
resultats_min <- extraire_resultats_attribut(resultats1, "ref", "MIN")
afficher_resultats(resultats_min, "RÉPARTITION DE MIN")

# Extraire les résultats pour APO
resultats_apo <- extraire_resultats_attribut(resultats1, "ref", "APO")
afficher_resultats(resultats_apo, "RÉPARTITION DE APO")

# Extraire les résultats pour IUP
resultats_iup <- extraire_resultats_attribut(resultats1, "ref", "IUP")
afficher_resultats(resultats_iup, "RÉPARTITION DE IUP")

# Fonction pour analyser la répartition selon l'attribut ref
ana_repartition <- function(data, attribut = "ref", inclure_na = FALSE, filtrer = NULL) {
  if (!attribut %in% c("ref", "role", "ana")) {
    stop("L'attribut doit être 'ref', 'role' ou 'ana'")
  }
  
  # Filtrage des NA si demandé
  if (!inclure_na) {
    data <- data[!is.na(data[[attribut]]), ]
  }
  
  # Création du tableau de répartition
  repartition <- data %>%
    group_by({{ livre }}, {{ attribut }}) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(livre, !!sym(attribut))
  
  # Filtrer les résultats si un attribut spécifique est demandé
  if (!is.null(filtrer)) {
    repartition <- repartition %>%
      filter(!!sym(attribut) == filtrer)
  }
  
  return(repartition)
}

# Extraire les résultats pour un attribut spécifique
extraire_resultats_attribut <- function(data, attribut, valeur) {
  # Analyser la répartition avec filtrer
  resultats <- ana_repartition(data, attribut = attribut, filtrer = valeur)
  
  # Afficher les résultats
  cat("\n=== RÉSULTATS POUR", toupper(valeur), "===\n")
  print(resultats)
  
  return(resultats)
}

# Résumé général
resume_general <- function(data) {
  cat("=== RÉSUMÉ GÉNÉRAL ===\n")
  cat("Nombre total de persName:", nrow(data), "\n")
  cat("Nombre de livres:", length(unique(data$livre[!is.na(data$livre)])), "\n")
  cat("Répartition par livre:\n")
  print(table(data$livre, useNA = "ifany"))
  
  cat("\n=== ATTRIBUTS ===\n")
  cat("Attribut 'ref' - valeurs uniques:", length(unique(data$ref[!is.na(data$ref)])), "\n")
  cat("Attribut 'role' - valeurs uniques:", length(unique(data$role[!is.na(data$role)])), "\n")
  cat("Attribut 'ana' - valeurs uniques:", length(unique(data$ana[!is.na(data$ana)])), "\n")
}

# Visualiser la répartition
vis_repartition <- function(repartition, attribut) {
  p <- ggplot(repartition, aes(x = livre, y = count, fill = !!sym(attribut))) +
    geom_col(position = "dodge") +
    labs(title = paste("Répartition des persName par livre selon l'attribut", attribut),
         x = "Livre",
         y = "Nombre d'occurrences",
         fill = attribut) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

# Fonction principale d'analyse
analyser_tei <- function(ovid_deities, attribut_analyse = "ref", 
                         afficher_resume = TRUE, creer_graphique = TRUE) {
  
  # Fonction pour charger le document TEI
  charger_tei <- function(chemin) {
    if (grepl("^http", chemin)) {
      # Si c'est un URL, on lit directement
      doc <- read_xml(chemin)
    } else {
      # Sinon, on lit depuis le fichier local
      doc <- read_xml(chemin)
    }
    return(doc)
  }

  # Chargement du document (local ou distant)
  doc_ovid_deities <- charger_tei(ovid_deities)

  # Extraction des données
  data <- extrait_persname(doc_ovid_deities)

  # Affichage du résumé si demandé
  if (afficher_resume) {
    resume_general(data)
  }

  # Analyse de la répartition
  repartition <- ana_repartition(data, attribut_analyse)

  cat("\n=== RÉPARTITION PAR", toupper(attribut_analyse), "===\n")
  print(repartition)

  # Création du graphique si demandé
  if (creer_graphique) {
    graphique <- vis_repartition(repartition, attribut_analyse)
    print(graphique)
  }

  # Retour des résultats
  return(list(
    donnees_brutes = data,
    repartition = repartition,
    graphique = if(creer_graphique) graphique else NULL
  ))
}