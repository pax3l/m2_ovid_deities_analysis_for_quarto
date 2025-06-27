# density_distribution.R
# Analyse des distributions linéaires des divinités dans les livres
# Utilisation de geom_density() pour visualiser les pics d'apparition

# Chargement des bibliothèques
library(ggplot2)
library(dplyr)

# Chargement des données XML
source("https://raw.githubusercontent.com/pax3l/m2_ovid_deities_analysis_for_quarto/refs/heads/main/scripts/distribution_deities.R")

# Fonction principale pour créer les graphiques de densité
create_density_plots <- function(data) {
  # Préparation des données
  # On veut ces colonnes :
  # - livre (numéro du livre)
  # - ref (nom de la divinité)
  # - position (position dans le livre)
  
  # Calcul de la densité pour chaque divinité
  density_plots <- data %>%
    group_by(ref) %>%
    do({
      ggplot(., aes(x = position)) +
        geom_density(alpha = 0.5, fill = "#66C2A5") +
        labs(
          title = paste("Distribution de", unique(.$ref)),
          x = "Position dans le livre",
          y = "Densité"
        ) +
        theme_minimal()
    })
  
  # Sauvegarde des graphiques
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  for (i in seq_along(density_plots)) {
    deity_name <- names(density_plots)[i]
    ggsave(
      filename = paste0("plots/density_", deity_name, ".png"),
      plot = density_plots[[i]],
      width = 10, height = 6
    )
  }
}

# Fonction pour visualiser les zones de silence
create_silence_zones <- function(data) {
  # Création d'un graphique montrant les zones de silence
  silence_plot <- ggplot(data, aes(x = position)) +
    geom_density(alpha = 0.3, fill = "#66C2A5") +
    geom_vline(xintercept = data$position, color = "red", linetype = "dashed", alpha = 0.5) +
    labs(
      title = "Zones de silence dans les apparitions des divinités",
      x = "Position dans le livre",
      y = "Densité"
    ) +
    theme_minimal()
  
  # Sauvegarde du graphique
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  ggsave(
    filename = "plots/silence_zones.png",
    plot = silence_plot,
    width = 12, height = 8
  )
}
create_density_plots(resultats1)
create_silence_zones(resultats1)
