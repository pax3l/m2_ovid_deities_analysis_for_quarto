# ---
# co_occurrence_analysis.R
# Analyse des relations croisées entre ana et type
# Utilisation de geom_tile et treemap pour visualiser les co-occurrences
# ---

# Chargement des bibliothèques
library(ggplot2)
library(dplyr)
library(treemap)
library(viridis)
library(xml2)
library(purrr)
library(tidyr)


# Chargement du texte
ovid_deities <- read_xml("data/ovid_MIN_APO_IUP.xml")

# Vérifier l'espace de noms
# Définir l'espace de noms TEI
tei_ns <- c(tei = "http://www.tei-c.org/ns/1.0")

# Extraction des données du XML
# Utiliser l'espace de noms TEI pour trouver les éléments w
words <- xml2::xml_find_all(ovid_deities, "//tei:w", ns = tei_ns)
words_list <- xml2::as_list(words)
co_occurrence_data <- map_df(words_list, function(x) {
  ana <- x$ana
  type <- x$type
  data.frame(ana = ana, type = type)
})

# Fonction pour créer la heatmap de co-occurrence
create_co_occurrence_heatmap <- function(data) {
  # Création de la matrice de co-occurrence
  co_occurrence_matrix <- data %>%
    group_by(ana, type) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = type, values_from = count, values_fill = 0)
  
  # Création de la heatmap
  heatmap_plot <- ggplot(co_occurrence_matrix, aes(x = ana, y = type, fill = count)) +
    geom_tile() +
    scale_fill_viridis(option = "B", direction = -1) +
    labs(
      title = "Matrice de co-occurrence entre ana et type",
      x = "Analyse",
      y = "Type de divinité",
      fill = "Nombre d'occurrences"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "right"
    )
  
  # Sauvegarde et affichage du graphique
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  ggsave(
    filename = "plots/co_occurrence_heatmap.png",
    plot = heatmap_plot,
    width = 12, height = 8
  )
  print(heatmap_plot)
}

# Fonction pour analyser la distribution des divinités par livre
create_deity_book_distribution <- function(data) {
  # Préparation des données
  # On veut ces colonnes :
  # - ref (divinité)
  # - livre
  
  # Création de la matrice de distribution
  distribution_matrix <- data %>%
    group_by(ref, livre) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = livre, values_from = count, values_fill = 0)
  
  # Création de la heatmap
  distribution_plot <- ggplot(distribution_matrix, aes(x = ref, y = livre, fill = count)) +
    geom_tile() +
    scale_fill_viridis(option = "B", direction = -1) +
    labs(
      title = "Distribution des divinités par livre",
      x = "Divinité",
      y = "Livre",
      fill = "Nombre d'occurrences"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "right"
    )
  
  # Sauvegarde et affichage du graphique
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  ggsave(
    filename = "plots/deity_book_distribution.png",
    plot = distribution_plot,
    width = 12, height = 8
  )
  print(distribution_plot)
}

# Fonction pour analyser les rôles par divinité
create_role_by_deity <- function(data) {
  # Préparation des données
  # On veut ces colonnes :
  # - ref (divinité)
  # - ana (rôle)
  
  # Création de la matrice de distribution
  role_matrix <- data %>%
    group_by(ref, ana) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = ana, values_from = count, values_fill = 0)
  
  # Création de la heatmap
  role_plot <- ggplot(role_matrix, aes(x = ref, y = ana, fill = count)) +
    geom_tile() +
    scale_fill_viridis(option = "B", direction = -1) +
    labs(
      title = "Rôles par divinité",
      x = "Divinité",
      y = "Rôle",
      fill = "Nombre d'occurrences"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "right"
    )
  
  # Sauvegarde et affichage du graphique
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  ggsave(
    filename = "plots/role_by_deity.png",
    plot = role_plot,
    width = 12, height = 8
  )
  print(role_plot)
}

# Fonction pour analyser l'évolution des rôles
create_role_evolution <- function(data) {
  # Préparation des données
  # On veut ces colonnes :
  # - ana (rôle)
  # - livre
  
  # Création de la matrice d'évolution
  evolution_matrix <- data %>%
    group_by(ana, livre) %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = livre, values_from = count, values_fill = 0)
  
  # Création de la heatmap
  evolution_plot <- ggplot(evolution_matrix, aes(x = ana, y = livre, fill = count)) +
    geom_tile() +
    scale_fill_viridis(option = "B", direction = -1) +
    labs(
      title = "Évolution des rôles au fil des livres",
      x = "Rôle",
      y = "Livre",
      fill = "Nombre d'occurrences"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "right"
    )
  
  # Sauvegarde du graphique
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  ggsave(
    filename = "plots/role_evolution.png",
    plot = evolution_plot,
    width = 12, height = 8
  )
}



# Fonction pour l'analyse tridimensionnelle divinité x rôle x livre
create_tridimensional_analysis <- function(data) {
  # Préparation des données
  # On veut ces colonnes :
  # - ref (divinité)
  # - ana (rôle)
  # - livre
  
  # Création de la matrice tridimensionnelle
  tridim_matrix <- data %>%
    group_by(ref, ana, livre) %>%
    summarise(count = n())
  
  # Création de la heatmap pour chaque divinité
  deity_plots <- tridim_matrix %>%
    group_by(ref) %>%
    do({
      deity_data <- .
      ggplot(deity_data, aes(x = ana, y = livre, fill = count)) +
        geom_tile() +
        scale_fill_viridis(option = "B", direction = -1) +
        labs(
          title = paste("Évolution des rôles de", unique(.$ref)),
          x = "Rôle",
          y = "Livre",
          fill = "Nombre d'occurrences"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "right"
        )
    })
  
  # Sauvegarde des graphiques
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  for (i in seq_along(deity_plots)) {
    deity_name <- names(deity_plots)[i]
    ggsave(
      filename = paste0("plots/", deity_name, "_role_evolution.png"),
      plot = deity_plots[[i]],
      width = 12, height = 8
    )
  }
}

# Essai/exemple suivi
# Création des graphiques
create_role_evolution(resultats1)
create_tridimensional_analysis(resultats1)

# Fonction pour créer le treemap
create_treemap <- function(data) {
  # Vérifier la structure des données
  print("Structure des données pour le treemap:")
  print(str(data))
  
  # Préparation des données pour le treemap
  treemap_data <- data %>%
    group_by(ref, type) %>%  # Utiliser ref et type qui existent dans les données
    summarise(count = n()) %>%
    ungroup()
  
  # Vérifier les données préparées
  print("Données préparées pour le treemap:")
  print(head(treemap_data))
  
  # Création du treemap
  if (nrow(treemap_data) > 0) {
    treemap(
      treemap_data,
      index = c("ref", "type"),
      vSize = "count",
      type = "index",
      palette = "Set3",
      title = "Distribution des divinités par type",
      fontsize.title = 14,
      fontsize.labels = 10,
      fontcolor.labels = "white"
    )
  } else {
    print("Aucune donnée disponible pour le treemap")
  }
    title = "Arborescence des co-occurrences ana/type",
    palette = "viridis",
    fontsize.title = 15,
    fontsize.labels = 10,
    border.col = "white",
    border.lwds = c(1, 0.5)
  )
  
  # Sauvegarde du graphique
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  ggsave(
    filename = "plots/co_occurrence_treemap.png",
    width = 12, height = 8
  )
}

# Fonction pour analyser les motifs dominants
analyze_dominant_patterns <- function(data) {
  # Calcul des motifs dominants
  patterns <- data %>%
    group_by(ana, type) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(
      percentage = count / sum(count) * 100,
      cumulative_percentage = cumsum(percentage)
    )
  
  # Création du graphique des motifs dominants
  pattern_plot <- ggplot(patterns, aes(x = reorder(paste(ana, type), -count), y = percentage)) +
    geom_bar(stat = "identity", fill = "#66C2A5") +
    labs(
      title = "Motifs dominants de co-occurrence",
      x = "Combinaison ana/type",
      y = "Pourcentage d'occurrences"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
  
  # Sauvegarde du graphique
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  ggsave(
    filename = "plots/dominant_patterns.png",
    plot = pattern_plot,
    width = 12, height = 8
  )
  
  # Affichage du graphique
  print(pattern_plot)
  
  return(patterns)
}
 # Reprise de l'extraction des données 
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
    type = xml_attr(pers_nodes, "type"),
    ana = xml_attr(pers_nodes, "ana"),
    livre = livres,
    stringsAsFactors = FALSE
  )
  
  return(data)
}

# Extraire les données
resultats1 <- extrait_persname(ovid_deities)
print(head(resultats1))
print(str(resultats1))

# Fonction pour créer la heatmap de co-occurrence
create_co_occurrence_heatmap <- function(resultats1) {
  # Vérifier les colonnes disponibles
  colonnes <- colnames(resultats1)
  print("Colonnes disponibles:")
  print(colonnes)
  
  # Vérifier les valeurs uniques
  print("Nombre de valeurs uniques par colonne:")
  print(sapply(resultats1, function(x) length(unique(x))))
  
  # Créer une heatmap simple avec les colonnes disponibles
  if ("ref" %in% colonnes) {
    # Si ref existe, utiliser ref comme axe x
    heatmap_plot <- ggplot(resultats1, aes(x = ref)) +
      geom_bar() +
      labs(
        title = "Distribution des références",
        x = "Référence",
        y = "Nombre d'occurrences"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1)
      )
  } else {
    # Si ref n'existe pas, créer un message
    heatmap_plot <- ggplot() +
      labs(title = "Aucune colonne appropriée trouvée")
  }
  
  return(heatmap_plot)
}

# Vérifier les colonnes disponibles
print("Colonnes disponibles:")
print(colnames(resultats1))

# Vérifier les valeurs uniques pour chaque colonne
print("Valeurs uniques pour chaque colonne:")
print(sapply(resultats1, function(x) length(unique(x))))

    )
  return(heatmap_plot)
}

# Créer la heatmap avec les données
heatmap <- create_co_occurrence_heatmap(resultats1)
print(heatmap) # je suis perplexe sur la forme de la heatmap...
create_treemap(resultats1) # et là, ça ne marche 
patterns <- analyze_dominant_patterns(resultats1) # le seul à se montrer pour le moment. pour analyse
