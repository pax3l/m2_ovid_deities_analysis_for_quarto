# ---+
# Distribution des mentions de MIN, APO et IUP dans les Métamorphoses
# Axelle Penture
# Version 1.0 - 14.06.2025
# ---+

#Installation des paquets
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
# Correspond au nombre d'occurrences total en recherche manuelle

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
  
  # Extraire les informations de livre pour chaque persName
  livres <- sapply(pers_nodes, function(node) {
    livre_parent <- xml_find_first(node, "ancestor::tei:div[@type='textpart' and @subtype='book']", ns = tei_ns)
    if (!is.na(livre_parent)) {
      as.numeric(xml_attr(livre_parent, "n"))
    } else {
      NA
    }
  })
  
  # Créer le dataframe avec toutes les informations
  data <- data.frame(
    texte = xml_text(pers_nodes),
    ref = xml_attr(pers_nodes, "ref"),
    role = xml_attr(pers_nodes, "role"),
    ana = xml_attr(pers_nodes, "ana"),
    livre = livres,
    stringsAsFactors = FALSE
  )
  
  return(data)
}
resultats1 <- extrait_persname(ovid_deities)
head(resultats1) 

# Analyser la répartition générale pour tous les attributs
repartition_generale <- ana_repartition(resultats1, attribut = "ref")
repartition_generale_role <- ana_repartition(resultats1, attribut = "role") #objet non reconnu 
repartition_generale_ana <- ana_repartition(resultats1, attribut = "ana") #objet non reconnu 
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
ana_repartition(resultats1, attribut="ref")

# Extraire les résultats pour un attribut spécifique
extraire_resultats_attribut <- function(data, attribut, valeur) {
  if (is.null(data)) {
    stop("Le dataframe de données est NULL. Vérifiez que les données ont été chargées correctement.")
  }
  
  if (!attribut %in% names(data)) {
    stop(paste("L'attribut", attribut, "n'existe pas dans les données.", 
              "Les attributs disponibles sont:", paste(names(data), collapse = ", ")))
  }
  
  # Analyser la répartition avec filtrer
  resultats <- ana_repartition(data, attribut = attribut, filtrer = valeur)
  
  # Vérifier si des résultats ont été trouvés
  if (nrow(resultats) == 0) {
    warning(paste("Aucun résultat trouvé pour l'attribut", valeur))
  }
  
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
resume_general(resultats1)

# Visualiser la répartition pour les divinités par livres
vis_repartition <- function(repartition, attribut) {
  couleurs <- c("#FF1493", "#9370DB", "#0000FF")   
  p <- ggplot(repartition, aes(x = livre, y = count, fill = !!sym(attribut))) +
    geom_col(position = "dodge", color = "black", size = 0.2) +  
    labs(
      title = paste("Répartition des divinités par livre selon", attribut),
      x = "Livre",
      y = "Nombre d'occurrences"
    ) +
    scale_x_continuous(breaks = seq(1, 15, by = 1)) +  
    scale_y_continuous(breaks = seq(0, max(repartition$count), by = 5)) +  
    scale_fill_manual(values = couleurs) + 
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.y = element_line(color = "grey80"),
      legend.position = "bottom",  
      legend.title = element_blank(), 
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
      plot.title.position = "plot", 
      plot.background = element_rect(color = "black", size = 0.5) 
    )
  
  return(p)
}
vis_repartition(repartition_generale, "ref")

# Visualiser la répartition des rôles par livres
vis_repartition_role <- function(repartition, attribut) {
  couleurs <- c("#FF9A5C", "#CC7A93") 
  p <- ggplot(repartition, aes(x = livre, y = count, fill = !!sym(attribut))) +
    geom_col(position = "fill") +
    labs(
      title = "Répartition des rôles par livre",
      x = "Livre",
      y = "Proportion"
    ) +
    scale_x_continuous(breaks = seq(1, 15, by = 1)) +
    scale_fill_manual(values = couleurs) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.y = element_line(color = "grey80"),
      legend.position = "right",
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.title.position = "plot",
      plot.background = element_rect(color = "black", size = 0.5)
    )
  
  return(p)
}
vis_repartition_role(repartition_generale, "role")
# Error in `geom_col()`:
# ! Problem while computing aesthetics.
# ℹ Error occurred in the 1st layer.
#Caused by error:
 # ! object 'role' not found

# Visualiser la répartition des analyses
vis_repartition_ana <- function(repartition) {
  couleurs <- c("#93003A", "#FF7EB6", "#0000FF", "#FFED00")
  p <- ggplot(repartition, aes(x = ana, y = count, fill = livre)) +
    geom_col(position = "dodge") +
    labs(
      title = "Répartition des analyses par livre",
      x = "Type d'analyse",
      y = "Nombre d'occurrences"
    ) +
    coord_flip() +  
    scale_y_continuous(breaks = seq(0, max(repartition$count), by = 5)) +
    scale_fill_manual(values = couleurs) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.y = element_line(color = "grey80"),
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.title.position = "plot",
      plot.background = element_rect(color = "black", size = 0.5)
    )
  
  return(p)
}
vis_repartition_ana(repartition_generale, "ana") 
# Error in vis_repartition_ana(repartition_generale, "ana") : 
# unused argument ("ana")

# Autres essais 
# vis_repartition_role(repartition_generale_role)  # ne trouve pas l'objet
# vis_repartition_ana(repartition_generale_ana)  # idem
