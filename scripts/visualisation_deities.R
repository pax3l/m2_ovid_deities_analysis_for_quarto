# ---+
# Visualisations de la distribution des divinités dans les Métamorphoses
# Axelle Penture
# Version 1.0 - 14.06.2025
# ---+

library(ggplot2)
library(dplyr)
library(tidyr)

# Chargement des données (à adapter selon votre source)
source("https://raw.githubusercontent.com/pax3l/m2_ovid_deities_analysis_for_quarto/refs/heads/main/scripts/distribution_deities.R")

# Fonction pour préparer les données pour les graphiques
prepare_data_for_visualization <- function(data) {
  # Ajouter une colonne pour le nom complet de la divinité
  data <- data %>%
    mutate(divinite = case_when(
      ref == "MIN" ~ "Minerve",
      ref == "APO" ~ "Apollon",
      ref == "IUP" ~ "Jupiter",
      TRUE ~ NA_character_
    ))
  
  return(data)
}

# Fonction pour créer un graphique de répartition par livre et divinité
plot_distribution_by_book <- function(data) {
  data_prepared <- prepare_data_for_visualization(data)
  
  ggplot(data_prepared, aes(x = livre, fill = divinité)) +
    geom_bar(position = "dodge", alpha = 0.8) +
    labs(
      title = "Répartition des mentions par livre et divinité",
      x = "Livre des Métamorphoses",
      y = "Nombre de mentions",
      fill = "Divinité"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold")
    )
}

# Fonction pour créer un graphique de répartition par rôle
plot_distribution_by_role <- function(data) {
  data_prepared <- prepare_data_for_visualization(data)
  
  ggplot(data_prepared, aes(x = role, fill = divinite)) +
    geom_bar(position = "dodge", alpha = 0.8) +
    labs(
      title = "Répartition des mentions par rôle et divinité",
      x = "Rôle",
      y = "Nombre de mentions",
      fill = "Divinité"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold")
    )
}

# Fonction pour créer un graphique de répartition par livre et rôle
plot_distribution_by_book_and_role <- function(data) {
  data_prepared <- prepare_data_for_visualization(data)
  
  ggplot(data_prepared, aes(x = livre, y = ..count.., fill = role)) +
    geom_bar(position = "fill", alpha = 0.8) +
    facet_wrap(~divinite, ncol = 1) +
    labs(
      title = "Répartition des rôles par livre (par divinité)",
      x = "Livre des Métamorphoses",
      y = "Proportion",
      fill = "Rôle"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold")
    )
}

# Exécuter les visualisations
if (interactive()) {
  # Créer les graphiques
  p1 <- plot_distribution_by_book(resultats1)
  p2 <- plot_distribution_by_role(resultats1)
  p3 <- plot_distribution_by_book_and_role(resultats1)
  
  # Sauvegarder les graphiques
  ggsave("distribution_par_livre.png", p1, width = 12, height = 8)
  ggsave("distribution_par_role.png", p2, width = 12, height = 8)
  ggsave("distribution_par_livre_role.png", p3, width = 12, height = 12)
  
  # Afficher les graphiques
  print(p1)
  print(p2)
  print(p3)
}
