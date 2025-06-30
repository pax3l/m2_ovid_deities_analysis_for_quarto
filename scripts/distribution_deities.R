# ---+
# Distribution des mentions de MIN, APO et IUP dans les Métamorphoses
# Axelle Penture
# Version 2.0 - 26.06.2025
# ---+

#Pré-instalalltion 
if (!requireNamespace("xml2", quietly = TRUE)) {
  install.packages("xml2", repos = "https://cran.rstudio.com")
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr", repos = "https://cran.rstudio.com")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr", repos = "https://cran.rstudio.com")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cran.rstudio.com")
}

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

# Toute la vérification pour essayer de trouver 'livre'
# Vérifier la structure XML avec l'espace de noms TEI
print("=== Structure du document XML ===")
cat("Nombre total de persName:", xml_find_all(ovid_deities, ".//tei:persName", ns = tei_ns) %>% length(), "\n")

# Vérifier la structure des livres
print("\n=== Structure des livres ===")
print("Livres trouvés avec premier XPath:")
print(xml_find_all(ovid_deities, "//tei:div[@type='textpart' and @subtype='book']", ns = tei_ns))
print("Livres trouvés avec second XPath:")
print(xml_find_all(ovid_deities, "//tei:div[@type='book']", ns = tei_ns))

# Vérifier les attributs des livres
print("\n=== Vérification des attributs ===")
livres_nodes <- xml_find_all(ovid_deities, "//tei:div[@type='textpart' and @subtype='book']", ns = tei_ns)
if (length(livres_nodes) > 0) {
  print("Attributs du premier livre:")
  print(xml_attrs(livres_nodes[[1]]))
} else {
  print("Aucun livre trouvé avec le premier XPath")
}

# Vérifier la hiérarchie XML
print("\n=== Hiérarchie XML ===")
print("Structure du premier persName:")
first_pers <- xml_find_first(ovid_deities, ".//tei:persName", ns = tei_ns)
if (!is.null(first_pers)) {
  print("Recherche du livre parent avec le premier XPath:")
  print(xml_find_first(first_pers, ".//ancestor::tei:div[@type='textpart' and @subtype='book']", ns = tei_ns))
  print("Recherche du livre parent avec le second XPath:")
  print(xml_find_first(first_pers, ".//ancestor::tei:div[@type='book']", ns = tei_ns))
} else {
  print("Aucun persName trouvé")
}
# Fin vérification...

# Afficher les résultats
afficher_resultats <- function(repartition, titre = "RÉPARTITION", attribut = "ref") {
  cat("\n=== ", titre, " ===\n")
  
  # Nombre total de mentions
  total_mentions <- sum(repartition$count)
  cat("Total mentions:", total_mentions, "\n")
  
  # Afficher la répartition avec pourcentage
  repartition_avec_pourcentage <- repartition %>%
    mutate(pourcentage = round(count/total_mentions*100, 1)) %>%
    rename(Attribut = !!sym(attribut))
  
  # Afficher en format markdown pour le .qmd
  cat("\n| Livre | Attribut | Nombre | Pourcentage |\n")
  cat("|-------|----------|--------|------------|\n")
  
  for (i in 1:nrow(repartition_avec_pourcentage)) {
    row <- repartition_avec_pourcentage[i, ]
    cat(sprintf("| %s | %s | %d | %.1f%% |\n", 
               row[["livre"]], 
               row$Attribut, 
               row$count, 
               row$pourcentage))
  }
  
  # Afficher la répartition brute
  cat("\nRépartition brute:\n")
  print(repartition)
  
  return(repartition_avec_pourcentage)
}

# Fonctions principales
# Fonction pour extraire les données persName avec leurs attributs
extrait_persname <- function(ovid_deities) {
  # Recherche de tous les éléments persName avec l'espace de noms TEI
  pers_nodes <- xml_find_all(ovid_deities, ".//tei:persName", ns = tei_ns)
  
  # Extraire les informations de livre pour chaque persName
  livres <- sapply(pers_nodes, function(node) {
    # Encore : recherche du livre parent avec plusieurs options XPath
    livre_parent <- xml_find_first(node, "ancestor::tei:div[@type='textpart' and @subtype='book']", ns = tei_ns)
    if (!is.null(livre_parent)) {
      # Vérifier si l'attribut n existe
      livre_num <- xml_attr(livre_parent, "n")
      if (!is.null(livre_num)) {
        as.numeric(livre_num)
      } else {
        NA
      }
    } else {
      # Autre chemin possible 
      livre_parent2 <- xml_find_first(node, "ancestor::tei:div[@type='book']", ns = tei_ns)
      if (!is.null(livre_parent2)) {
        as.numeric(xml_attr(livre_parent2, "n"))
      } else {
        NA
      }
    }
  })
  
  # Créer le dataframe avec toutes les informations
  data <- data.frame(
    texte = xml_text(pers_nodes),
    ref = xml_attr(pers_nodes, "ref"),
    type = xml_attr(pers_nodes, "type"),
    ana = xml_attr(pers_nodes, "ana"),
    livre = livres,
    stringsAsFactors = FALSE
  )
  
  # Afficher les colonnes du dataframe
  print("\n=== Vérification des colonnes du dataframe ===")
  print(colnames(data))
  print("\nNombre de lignes avec livre NA:")
  print(sum(is.na(data$livre)))
  
  # Vérifier les valeurs uniques de livre
  print("\nValeurs uniques de livre:")
  print(unique(data$livre))
  
  return(data)
}

resultats1 <- extrait_persname(ovid_deities)
head(resultats1)

# Fonction pour analyser la répartition selon l'attribut ref
ana_repartition <- function(data, attribut = "ref", inclure_na = FALSE, filtrer = NULL) {
  if (!attribut %in% c("ref", "type", "ana")) {
    stop("L'attribut doit être 'ref', 'type' ou 'ana'")
  }
  
  # Vérifier que le dataframe contient les colonnes nécessaires
  if (!"livre" %in% colnames(data)) {
    stop("Le dataframe ne contient pas la colonne 'livre'")
  }
  
  # Filtrage des NA si demandé
  if (!inclure_na) {
    data <- data[!is.na(data[[attribut]]), ]
  }
  
  # Création du tableau de répartition
  repartition <- data %>%
    group_by(across(all_of("livre")), across(all_of(attribut))) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(livre)
  
  # Vérifier les colonnes du résultat
  print("\n=== Vérification des colonnes du résultat ===")
  print(colnames(repartition))
  
  # Filtrer les résultats si un attribut spécifique est demandé
  if (!is.null(filtrer)) {
    repartition <- repartition %>%
      filter(!!sym(attribut) == filtrer)
  }
  
  return(repartition)
}

# Fonction pour afficher les résultats
afficher_resultats <- function(repartition, titre) {
  cat("\n===", titre, "===\n")
  print(repartition)
}

# Fonction pour visualiser la répartition
ggplot2::theme_set(ggplot2::theme_minimal())

vis_repartition_type <- function(repartition, attribut) {
  couleurs <- c("#FF9A5C", "#CC7A93") 
  p <- ggplot(repartition, aes(x = livre, y = count, fill = {{ attribut }})) +
    geom_col(position = "fill") +
    scale_fill_manual(values = couleurs) +
    labs(title = paste("Répartition des", attribut, "par livres"),
         x = "Livre",
         y = "Proportion",
         fill = attribut) +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 12)
    )
  
  # Sauvegarder le graphique en format PNG pour le .qmd
  png_filename <- paste0("repartition_", attribut, ".png")
  ggsave(png_filename, p, width = 10, height = 6, units = "in", path = "images", create.dir = TRUE)
  
  return(p)
}

# Fonction pour résumer les données
resume_general <- function(data) {
  cat("\n=== RÉSUMÉ GÉNÉRAL ===\n")
  cat("Nombre total d'occurrences:", nrow(data), "\n")
  cat("\n=== DIVINITÉS ===\n")
  cat("\nOccurrences des valeurs de 'ref':\n")
  print(table(data$ref))
}
resume_general(resultats1)

# Fonction pour extraire les résultats d'un attribut spécifique
extraire_resultats_attribut <- function(data, attribut, valeur) {
  return(data[data[[attribut]] == valeur, ])
}

# Analyser la répartition générale pour tous les attributs
if (exists("resultats1") && is.data.frame(resultats1)) {
  print("Analyse de la répartition...")
  repartition_generale <- ana_repartition(resultats1, attribut = "ref")
  repartition_generale_type <- ana_repartition(resultats1, attribut = "type")
  repartition_generale_ana <- ana_repartition(resultats1, attribut = "ana")
  afficher_resultats(repartition_generale, "RÉPARTITION GÉNÉRALE")
} else {
  stop("Le dataframe resultats1 n'existe pas ou n'est pas un dataframe")
}
afficher_resultats(repartition_generale_type, "RÉPARTITION PAR 'TYPE'")
afficher_resultats(repartition_generale_ana, "RÉPARTITION PAR 'ANA'")

# Visualiser les répartitions 
print(vis_repartition_type(repartition_generale_type, "type"))

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
  if (!attribut %in% c("ref", "type", "ana")) {
    stop("L'attribut doit être 'ref', 'type' ou 'ana'")
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
ana_repartition(resultats1, attribut="ref") # Problème de l'objet 'livre' non trouvé...

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
  cat("Répartition des divinités par livre:\n")
  print(table(data$livre, useNA = "ifany"))
  
}
resume_general(resultats1)

# Visualiser la répartition pour les divinités par livres
vis_repartition <- function(repartition, attribut) {
  couleurs <- c("#FF1493", "#0000FF", "#9370DB")   
  p <- ggplot(repartition, aes(x = livre, y = count, fill = !!sym(attribut))) +
    geom_col(position = "dodge", color = "black", size = 0.2) +  
    labs(
      title = paste("Répartition des divinités par livre selon l'attribut :", attribut),
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
print(vis_repartition(repartition_generale, "ref"))
plot_par_type <- vis_repartition(repartition_generale, "ref")
ggsave("plots/rep_ref_par_livre.png", plot = plot_par_type, width = 10, height = 6)


# Visualiser la répartition des rôles par livres
vis_repartition_type <- function(repartition, attribut) {
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
vis_repartition_type(repartition_generale, "type")
# Error in `geom_col()`:
# ! Problem while computing aesthetics.
# ℹ Error occurred in the 1st layer.
#Caused by error:
 # ! object 'type' not found
# ça non plus je ne comprends pas pourquoi pas. j'ai fait une erreur initiale en mettant 'role' à la place de 'type', mais j'ai tout remplacé en chercher/remplacer. 

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
# vis_repartition_type(repartition_generale_type)  # ne trouve pas l'objet
# vis_repartition_ana(repartition_generale_ana)  # idem
