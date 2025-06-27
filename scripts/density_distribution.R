# density_distribution.R
# Analyse des distributions linéaires des divinités dans les livres
# Utilisation de geom_density() pour visualiser les pics d'apparition

# Configuration du miroir CRAN
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# installation si nécessaire
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("magrittr", quietly = TRUE)) {
  install.packages("magrittr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("xml2", quietly = TRUE)) {
  install.packages("xml2")
}

# Bibliothèques
library(ggplot2)
library(dplyr)
library(magrittr)
library(xml2)

# Chargement des données XML
source("scripts/distribution_deities_local.R")

# Définir l'espace de noms TEI
tei_ns <- c(tei = "http://www.tei-c.org/ns/1.0")

# Chargement du texte d'Ovide depuis GitHub avec authentification
options(GITHUB_PAT = Sys.getenv("GITHUB_PAT"))
ovid_deities <- read_xml("https://raw.githubusercontent.com/pax3l/m2_ovid_deities_analysis_for_quarto/main/data/ovid_MIN_APO_IUP.xml")

# Extraire les données
resultats1 <- extrait_persname(ovid_deities)

# Calculer la position dans le livre pour chaque mention
calcul_position <- function(data) {
  data <- data %>%
    group_by(livre) %>%
    mutate(position = row_number()) %>%
    ungroup()
  return(data)
}

# Préparer les données
resultats1 <- calcul_position(resultats1)

# Vérifier la structure des données
print(head(resultats1))
print(summary(resultats1))

# Fonction principale pour créer les graphiques de densité
create_density_plots <- function(data) {
  # Cibler les diivnités choisies
  data <- data %>% 
    filter(ref %in% c("MIN", "IUP", "APO")) %>%
    select(livre, ref) %>%  
    mutate(ref = as.character(ref))
  
  # Créer les graphiques pour chaque divinité
  density_plots <- data %>%
    group_by(ref) %>%
    group_map(~ {
      deity_name <- case_when(
        unique(.x$ref)[1] == "MIN" ~ "Minerve",
        unique(.x$ref)[1] == "IUP" ~ "Jupiter",
        unique(.x$ref)[1] == "APO" ~ "Apollon"
      )
      p <- ggplot(.x, aes(x = livre)) +
        geom_density(alpha = 0.5) +
        scale_x_continuous(breaks = 1:15, 
                          labels = paste("Livre", 1:15),
                          limits = c(0.5, 15.5)) +
        scale_fill_manual(values = c("#66C2A5", "#FF6961"),
                         guide = "none") +
        scale_color_manual(values = c("#66C2A5", "#FF6961"),
                         guide = "none") +
        labs(
          title = paste("Densité de", deity_name),
          x = "Livre",
          y = "Densité"
        ) +
        theme_minimal()
      print(p)
      return(p)
    })
  
  # Sauvegarder les graphiques pour l'impression
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  invisible(lapply(seq_along(density_plots), function(i) {
    deity_name <- unique(data$ref)[i]
    # Nettoyer le nom pour éviter les caractères problématiques
    filename <- paste0("plots/density_", gsub("[[:punct:][:space:]]", "_", deity_name), ".pdf")
    
    # Vérifier si le fichier peut être créé
    if (file.exists(filename)) {
      file.remove(filename)
    }
    
    # Sauvegarder le graphique pour l'impression
    ggsave(
      filename = filename,
      plot = density_plots[[i]],
      width = 10, height = 6,
      device = "pdf",
      dpi = 300  
    )
  }))
}

# Fonction pour visualiser les zones de silence
create_silence_zones <- function(data) {
  # Filtrer les divinités spécifiques
  data <- data %>% 
    filter(ref %in% c("MIN", "IUP", "APO"))
  
  silence_plot <- ggplot(data, aes(x = livre)) +
    geom_density(alpha = 0.3, fill = "#66C2A5") +
    geom_vline(xintercept = data$livre, color = "red", linetype = "dashed", alpha = 0.5) +
    scale_x_continuous(breaks = 1:15, 
                      labels = paste("Livre", 1:15),
                      limits = c(0.5, 15.5)) +
    labs(
      title = "Zones de silence dans les apparitions des divinités",
      x = "Livre",
      y = "Densité"
    ) +
    theme_minimal()
  print(silence_plot)  # Afficher le graphique
  
  # Sauvegarde
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  ggsave(
    filename = "plots/silence_zones.png",
    plot = silence_plot,
    width = 12, height = 8
  )
}

# Chargement des données XML
source("scripts/distribution_deities_local.R")

# Définir l'espace de noms TEI
tei_ns <- c(tei = "http://www.tei-c.org/ns/1.0")

# Chargement du texte d'Ovide depuis GitHub avec authentification
options(GITHUB_PAT = Sys.getenv("GITHUB_PAT"))
ovid_deities <- read_xml("https://raw.githubusercontent.com/pax3l/m2_ovid_deities_analysis_for_quarto/main/data/ovid_MIN_APO_IUP.xml")

# Extraire les données
resultats1 <- extrait_persname(ovid_deities)

# Créer les graphiques
create_density_plots(resultats1)
create_silence_zones(resultats1)

# Fonction pour créer les graphiques de densité avec position dans le livre
create_density_plots_position <- function(data) {
  # Cibler les diivnités choisies
  data <- data %>% 
    filter(ref %in% c("MIN", "IUP", "APO")) %>%
    select(livre, ref, position) %>%
    filter(!is.na(position)) %>%
    mutate(ref = as.character(ref))
    
  # Créer les graphiques pour chaque divinité
  density_plots <- data %>%
    group_by(ref) %>%
    group_map(~ {
      p <- ggplot(.x, aes(x = position)) +
        geom_density(alpha = 0.5) +
        scale_fill_manual(values = c("#66C2A5", "#FF6961"),
                         guide = "none") +
        scale_color_manual(values = c("#66C2A5", "#FF6961"),
                         guide = "none") +
        labs(
          title = paste("Distribution de", unique(.x$ref)),
          x = "Position dans le livre",
          y = "Densité"
        ) +
        theme_minimal()
      return(p)
    })
  
  # Afficher les graphiques
  invisible(lapply(density_plots, print))
  
  # Sauvegarde des graphiques
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  invisible(lapply(seq_along(density_plots), function(i) {
    deity_name <- unique(data$ref)[i]
    # Nettoyer le nom 
    filename <- paste0("plots/density_", gsub("[[:punct:][:space:]]", "_", deity_name), ".pdf")
    
    # Vérifier si le fichier peut être créé
    if (file.exists(filename)) {
      file.remove(filename)
    }
    
    # Sauvegarder le graphique
    ggsave(
      filename = filename,
      plot = density_plots[[i]],
      width = 10, height = 6,
      device = "pdf"
    )
  }))
}

# Créer les graphiques de densité
create_density_plots(resultats1)

# Fonction pour visualiser les zones de silence
create_silence_zones <- function(data) {
  # Filtrer les divinités spécifiques
  data <- data %>% 
    filter(ref %in% c("MIN", "IUP", "APO"))
  silence_plot <- ggplot(data, aes(x = livre)) +
    geom_density(alpha = 0.3, fill = "#66C2A5") +
    geom_vline(xintercept = data$livre, color = "red", linetype = "dashed", alpha = 0.5) +
    scale_x_continuous(breaks = 1:15, 
                      labels = paste("Livre", 1:15),
                      limits = c(0.5, 15.5)) +
    labs(
      title = "Zones de silence dans les apparitions des divinités",
      x = "Livre",
      y = "Densité"
    ) +
    theme_minimal()
  print(silence_plot) 
  
  # Sauvegarde
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  ggsave(
    filename = "plots/silence_zones.png",
    plot = silence_plot,
    width = 12, height = 8
  )
}

# Créer les graphiques de silence
create_silence_zones(resultats1)
