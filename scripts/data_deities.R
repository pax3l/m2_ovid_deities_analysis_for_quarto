# ---+
# Traitement des données de répartition des mentions de MIN, APO et IUP dans les Métamorphoses
# Axelle Penture
# Version 3.0 - 01.07.2025 (anciennement distribution_deities.R)
# ---+

# Pré-installation & chargement des librairies
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
if (!requireNamespace("scales", quietly = TRUE)) {
  install.packages("sclaes", repos = "https://cran.rstudio.com")
}

library(xml2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Charger du texte encodé d'Ovide depuis le dépôt en ligne 
ovid_deities <- read_xml(
  "https://raw.githubusercontent.com/pax3l/m2_ovid_deities_analysis_for_quarto/refs/heads/main/data/ovid_MIN_APO_IUP.xml"
)
ovid_deities # vérification 

# Créer l'espace de noms
tei_ns <- c(tei = "http://www.tei-c.org/ns/1.0") # Définir l'espace de noms TEI

# Chercher tous les éléments <persName> avec l'espace de noms TEI
pers_nodes <- xml_find_all(ovid_deities, ".//tei:persName", ns = tei_ns)

# Créer les objets nécessaire 
# Objet livre, extrait pour chaque <persName>
livre <- vapply(pers_nodes, function(node) {
  livre_parent <- xml_find_first(node, "ancestor::tei:div[@type='textpart' and @subtype='book']", ns = tei_ns)
  if (!is.na(livre_parent)) {
    livre_num <- xml_attr(livre_parent, "n")
    if (!is.na(livre_num)) {
      return(as.numeric(livre_num))
    }
  }
  return(NA_real_)
}, numeric(1))

# Créer le dataframe avec toutes les informations attendues
data <- data.frame(
  texte = xml_text(pers_nodes),
  ref = xml_attr(pers_nodes, "ref"),
  type = xml_attr(pers_nodes, "type"),
  ana = xml_attr(pers_nodes, "ana"),
  livre = livre,
  stringsAsFactors = FALSE
)

ref_value <- c("MIN", "APO", "IUP")

# Fonctions principales

## Présenter les données issues de l'encodage

### Comment afficher les résultats
afficher_resultats <- function(repartition, titre) {
  cat("\n===", titre, "===\n")
  print(repartition)
}

### Extraire les données <persName> avec attributs
extrait_persname <- function(ovid_deities) {
  pers_nodes <- xml_find_all(ovid_deities, "//tei:persName", ns = tei_ns)
  livres <- vapply(pers_nodes, function(node) {
    livre_parent <- xml_find_first(node, "ancestor::tei:div[@type='textpart' and @subtype='book']", ns = tei_ns)
    if (!is.na(livre_parent)) {
      livre_num <- xml_attr(livre_parent, "n")
      if (!is.na(livre_num)) {
        return(as.numeric(livre_num))
      }
    }
    return(NA_real_)
  }, numeric(1))
  return(data)
}
resultats1 <- extrait_persname(ovid_deities)
print(head(resultats1)) # Vérification et impression 

## Présenter les résultats généraux
### Résumé général des occurrences totales des divinités par "ref"

resume_general_divinite <- function(data) {
  cat("\n=== RÉSUMÉ GÉNÉRAL ===\n")
  cat("Nombre total d'occurrences:", nrow(data), "\n")
  cat("\n=== DIVINITÉS ===\n")
  cat("\nOccurrences des valeurs de 'ref':\n")
  print(table(data$ref))
}
resume_general_divinite(resultats1)
print(resume_general_divinite(resultats1)) # en double mais imprimé

### Résumé général des divinités par livres

resume_general_par_livre <- function(data) {
  cat("=== RÉSUMÉ GÉNÉRAL ===\n")
  cat("Nombre total de persName:", nrow(data), "\n")
  cat("Nombre de livres:", length(unique(data$livre[!is.na(data$livre)])), "\n")
  cat("Répartition des divinités par livre:\n")
  print(table(data$livre, useNA = "ifany"))
}
resume_general_par_livre(resultats1)
print(resume_general_par_livre(resultats1)) # idem

## Analyser la répartition des divinités dans les livres (attribut "ref")

attribut_ref <- "ref"
repartition_ref <- data %>%
  group_by(livre, !!sym(attribut_ref)) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(livre, !!sym(attribut_ref), fill = list(count = 0)) %>%
  arrange(livre, !!sym(attribut_ref))

vis_repartition <- function(repartition, attribut_ref) {
  couleurs <- c("#FF1493", "#0000FF", "#9370DB")   
  p <- ggplot(repartition, aes(x = livre, y = count, fill = !!sym(attribut_ref))) +
    geom_col(position = "dodge", color = "black", size = 0.2) +  
    labs(
      title = paste("Répartition des divinités par livre\nselon l'attribut :", attribut_ref),
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
vis_repartition(repartition_ref, "ref")
print(vis_repartition(repartition_ref, "ref"))

### Sauvegarde de l'image
plot_par_type <- vis_repartition(repartition_ref, "ref")
ggsave("plots/rep_ref_par_livre.png", plot = plot_par_type, width = 10, height = 6)

## Analyser les contextes narratifs des divinités dans les livres (attribut "type")

attribut_type <- "type"
repartition_type <- data %>%
  group_by(livre, !!sym(attribut_type)) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(livre, !!sym(attribut_type), fill = list(count = 0)) %>%
  arrange(livre, !!sym(attribut_type))

vis_repartition_type <- function(repartition, attribut_type) {
  couleurs <- c("#FF9A5C", "#CC7A93") 
  p <- ggplot(repartition, aes(x = livre, y = count, fill = !!sym(attribut_type))) +
    geom_col(position = "fill", color = "black", size = 0.2) +
    labs(
      title = "Proportion des contextes narratifs par livre", 
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
vis_repartition_type(repartition_type, "type")
print(vis_repartition_type(repartition_type, "type"))

## Analyse des contextes narratifs pour chaque divinités ("type" selon "ref")

### APO

attribut_type_apo <- "type"
repartition_type_apo <- data %>%
  filter(ref == "APO") %>%
  group_by(livre, !!sym(attribut_type_apo)) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(livre, !!sym(attribut_type_apo), fill = list(count = 0)) %>%
  arrange(livre, !!sym(attribut_type_apo))

vis_repartition_type_apo <- function(repartition, attribut_type_apo) {
  couleurs <- c("#FF9A5C", "#CC7A93") 
  p <- ggplot(repartition, aes(x = livre, y = count, fill = !!sym(attribut_type_apo))) +
    geom_col(position = "fill", color = "black", size = 0.2) +
    labs(
      title = "Proportion des contextes narratifs par livre pour APO", 
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
vis_repartition_type_apo(repartition_type_apo, "type")
print(vis_repartition_type_apo(repartition_type_apo, "type"))

### IUP

attribut_type_iup <- "type"
repartition_type_iup <- data %>%
  filter(ref == "IUP") %>%
  group_by(livre, !!sym(attribut_type_iup)) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(livre, !!sym(attribut_type_iup), fill = list(count = 0)) %>%
  arrange(livre, !!sym(attribut_type_iup))

vis_repartition_type_iup <- function(repartition, attribut_type_iup) {
  couleurs <- c("#FF9A5C", "#CC7A93") 
  p <- ggplot(repartition, aes(x = livre, y = count, fill = !!sym(attribut_type_iup))) +
    geom_col(position = "fill", color = "black", size = 0.2) +
    labs(
      title = "Proportion des contextes narratifs par livre pour IUP", 
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
vis_repartition_type_iup(repartition_type_iup, "type")
print(vis_repartition_type_iup(repartition_type_iup, "type"))

### MIN 

attribut_type_min <- "type"
repartition_type_min <- data %>%
  filter(ref == "MIN") %>%
  group_by(livre, !!sym(attribut_type_min)) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(livre, !!sym(attribut_type_min), fill = list(count = 0)) %>%
  arrange(livre, !!sym(attribut_type_min))

vis_repartition_type_min <- function(repartition, attribut_type_min) {
  couleurs <- c("#FF9A5C", "#CC7A93") 
  p <- ggplot(repartition, aes(x = livre, y = count, fill = !!sym(attribut_type_min))) +
    geom_col(position = "fill", color = "black", size = 0.2) +
    labs(
      title = "Proportion des contextes narratifs par livre pour MIN", 
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
vis_repartition_type_min(repartition_type_min, "type")
print(vis_repartition_type_min(repartition_type_min, "type"))

## Analyser les répartitions des rôles des divinités dans l'ensemble du corpus ("ana" selon livre)
### Point de vue global 

attribut_ana_na <- "ana"
repartition_ana_na <- data %>%
  group_by(livre, !!sym(attribut_ana_na)) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(livre, !!sym(attribut_ana_na), fill = list(count = 0)) %>%
  arrange(livre, !!sym(attribut_ana_na))

vis_repartition_ana_na <- function(repartition, attribut_ana_na) {
  n_livres <- length(unique(repartition$livre))
  couleurs <- c("act"="#93003A", "obj"="#FFED00", "auto"="#FF7EB6", "NA"="#0000FF")
  
  p <- ggplot(repartition_ana_na, aes(x = factor(livre), y = count, fill = !!sym(attribut_ana_na))) +
    geom_col(position = "fill", color = "black", size = 0.2) +
    labs(
      title = paste("Répartition des valeurs de ana par livre"),
      x = "Livre",
      y = "Proportion",
      fill = "Livre"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    scale_fill_manual(values = couleurs) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.title.position = "plot",
      plot.background = element_rect(color = "black", size = 0.5)
    )
  
  print(p)
}

vis_repartition_ana_na(repartition_ana_na, "ana")
print(vis_repartition_ana_na(repartition_ana_na, "ana"))

#### Sauvegarder le graphique
plot_par_ana <- vis_repartition_ana_na(repartition_ana_na, "ana")
ggsave(paste0("plots/rep_ana_par_livre.png"), plot = plot_par_ana, width = 10, height = 6)

### Point de vue par divinités
#### Fonction et plot
vis_repartition_ana_par_ref_na <- function(df, ref_value) {
  attribut_ana_na <- "ana"
  
  repartition_na <- df %>%
    filter(ref == ref_value) %>%
    group_by(livre, !!sym(attribut_ana_na)) %>%
    summarise(count = n(), .groups = "drop") %>%
    complete(livre, !!sym(attribut_ana_na), fill = list(count = 0)) %>%
    arrange(livre, !!sym(attribut_ana_na))
  
  couleurs <- c("act" = "#93003A", "obj" = "#FFED00", "auto" = "#FF7EB6", "NA" = "#0000FF")
  
  p <- ggplot(repartition_na, aes(x = factor(livre), y = count, fill = !!sym(attribut_ana_na))) +
    geom_col(position = "fill", color = "black", size = 0.2) +
    labs(
      title = paste0("Répartition de 'ana' pour la référence : ", ref_value),
      x = "Livre",
      y = "Proportion"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = couleurs) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.title.position = "plot",
      plot.background = element_rect(color = "black", size = 0.5)
    )
  
  print(p)
  
  return(p)
}

#### APO
plot_ana_APO_na <- vis_repartition_ana_par_ref_na(data, "APO")
ggsave("plots/repartition_ana_APO.png", plot = plot_ana_APO_na, width = 10, height = 6)

#### IUP
plot_ana_IUP_na <- vis_repartition_ana_par_ref_na(data, "IUP")
ggsave("plots/repartition_ana_IUP.png", plot = plot_ana_IUP_na, width = 10, height = 6)

#### MIN
plot_ana_MIN_na <- vis_repartition_ana_par_ref_na(data, "MIN")
ggsave("plots/repartition_ana_MIN.png", plot = plot_ana_MIN_na, width = 10, height = 6)

### Représentations sans les valeurs NA 
#### Général

attribut_ana <- "ana"
repartition_ana <- data %>%
  filter(!!sym(attribut_ana) != "NA") %>%
  group_by(livre, !!sym(attribut_ana)) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(livre, !!sym(attribut_ana), fill = list(count = 0)) %>%
  arrange(livre, !!sym(attribut_ana))

vis_repartition_ana <- function(repartition, attribut_ana) {
  n_livres <- length(unique(repartition$livre))
  couleurs <- c("act"="#93003A", "obj"="#FFED00", "auto"="#FF7EB6")
  
  p <- ggplot(repartition_ana, aes(x = factor(livre), y = count, fill = !!sym(attribut_ana))) +
    geom_col(position = "fill", color = "black", size = 0.2) +
    labs(
      title = paste("Répartition des valeurs de 'ana' par livre"),
      x = "Livre",
      y = "Proportion",
      fill = "Livre"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_minimal() +
    scale_fill_manual(values = couleurs) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.title.position = "plot",
      plot.background = element_rect(color = "black", size = 0.5)
    )
  
  print(p)
}

vis_repartition_ana(repartition_ana, "ana")
print(vis_repartition_ana(repartition_ana, "ana"))

#### Par divinités  

attribut_ana <<- "ana"
df_ana_ref <- data %>%
  select(ref, ana, livre) %>%
  as_tibble()

##### APO
repartition_ana_ref_apo <- df_ana_ref %>%
  filter(ref == "APO", !!sym(attribut_ana) != "NA") %>%
  group_by(livre, !!sym(attribut_ana)) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(
    livre = unique(data$livre),
    !!sym(attribut_ana),
    fill = list(count = 0)
  ) %>%
  arrange(livre, !!sym(attribut_ana))

vis_repartition_ana_par_ref_apo <- function(data, ref) {
  
  couleurs <- c("act" = "#93003A", "obj" = "#FFED00", "auto" = "#FF7EB6") 
  
  p <- ggplot(repartition_ana_ref_apo, aes(x = factor(livre), y = count, fill = !!sym(attribut_ana))) +
    geom_col(position = "fill", color = "black", size = 0.2) +
    labs(
      title = paste0("Répartition des valeurs de 'ana' \npar livre pour ", ref, " (sans 'NA')"),
      x = "Livre",
      y = "Proportion"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = couleurs) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.title.position = "plot",
      plot.background = element_rect(color = "black", size = 0.5)
    )
  
  print(p)
  return(p)
}

vis_repartition_ana_par_ref_apo(repartition_ana_ref_apo, "APO")
print(vis_repartition_ana_par_ref_apo(repartition_ana_ref_apo, "APO"))

#### IUP 
repartition_ana_ref_iup <- df_ana_ref %>%
  filter(ref == "IUP", !!sym(attribut_ana) != "NA") %>%
  group_by(livre, !!sym(attribut_ana)) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(
    livre = unique(data$livre),
    !!sym(attribut_ana),
    fill = list(count = 0)
  ) %>%
  arrange(livre, !!sym(attribut_ana))

vis_repartition_ana_par_ref_iup <- function(data, ref) {
  
  couleurs <- c("act" = "#93003A", "obj" = "#FFED00", "auto" = "#FF7EB6") 
  
  p <- ggplot(repartition_ana_ref_iup, aes(x = factor(livre), y = count, fill = !!sym(attribut_ana))) +
    geom_col(position = "fill", color = "black", size = 0.2) +
    labs(
      title = paste0("Répartition des valeurs de 'ana' \npar livre pour ", ref, " (sans 'NA')"),
      x = "Livre",
      y = "Proportion"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = couleurs) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.title.position = "plot",
      plot.background = element_rect(color = "black", size = 0.5)
    )
  
  print(p)
  return(p)
}

vis_repartition_ana_par_ref_iup(repartition_ana_ref_iup, "IUP")
print(vis_repartition_ana_par_ref_iup(repartition_ana_ref_iup, "IUP"))

#### MIN 

repartition_ana_ref_min <- df_ana_ref %>%
  filter(ref == "MIN", !!sym(attribut_ana) != "NA") %>%
  group_by(livre, !!sym(attribut_ana)) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(
    livre = unique(data$livre),
    !!sym(attribut_ana),
    fill = list(count = 0)
  ) %>%
  arrange(livre, !!sym(attribut_ana))

vis_repartition_ana_par_ref_min <- function(data, ref) {
  
  couleurs <- c("act" = "#93003A", "obj" = "#FFED00", "auto" = "#FF7EB6") 
  
  p <- ggplot(repartition_ana_ref_min, aes(x = factor(livre), y = count, fill = !!sym(attribut_ana))) +
    geom_col(position = "fill", color = "black", size = 0.2) +
    labs(
      title = paste0("Répartition des valeurs de 'ana' \npar livre pour ", ref, " (sans 'NA')"),
      x = "Livre",
      y = "Proportion"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = couleurs) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.title.position = "plot",
      plot.background = element_rect(color = "black", size = 0.5)
    )
  
  print(p)
  return(p)
}

vis_repartition_ana_par_ref_min(repartition_ana_ref_min, "MIN")
print(vis_repartition_ana_par_ref_min(repartition_ana_ref_min, "MIN"))


## Analyse croisée de la distribution des éléments "ana" et "type" # bloque ici aussi 

vis_ana_par_type_et_livre <- function(df, ref_value = "MIN", attribut_ana = "ana", save = FALSE, path = "plots/") {
  
  df_counts <- df %>%
    filter(ref == ref_value, !is.na(!!sym(attribut_ana)), !!sym(attribut_ana) != "NA") %>%
    group_by(livre, type, !!sym(attribut_ana)) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(livre, type) %>%
    mutate(prop = count / sum(count)) %>%
    ungroup()
class(df)
  couleurs <- c("act" = "#93003A", "obj" = "#FFED00", "auto" = "#FF7EB6")
  
  p <- ggplot(df_counts, aes(x = type, y = prop, fill = !!sym(attribut_ana))) +
    geom_col(position = "stack", color = "black", size = 0.2) +
    facet_wrap(~ livre, ncol = 5) +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = couleurs) +
    labs(
      title = paste("Proportions de l'attribut", attribut_ana, "par type et livre (ref =", ref_value, ")"),
      x = "Type",
      y = "Proportion",
      fill = attribut_ana
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
  
  print(p)
  
  if (save) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    filename <- paste0(path, "ana_type_par_livre_", ref_value, ".png")
    ggsave(filename, plot = p, width = 10, height = 6)
    message("Graphique sauvegardé : ", filename)
  }
}


refs <- c("MIN", "APO", "IUP")
for (ref_val in refs) {
  vis_ana_par_type_et_livre(data, ref_value = ref_val, save = TRUE) ## seems absolutely useless and not what I look for
}
print(vis_ana_par_type_et_livre(data, ref_value = "MIN"))

# Heatmaps croisant "type" et "ana"
## V1
heatmap_type_ana_generale <- function(data, show_prop = TRUE) {
  df_heat <- data %>%
    group_by(type, ana) %>%
    summarise(count = n(), .groups = "drop")
  
  if (show_prop) {
    df_heat <- df_heat %>%
      group_by(type) %>%
      mutate(prop = count / sum(count)) %>%
      ungroup()
  }
  
  p <- ggplot(df_heat, aes(x = ana, y = type, fill = if (show_prop) prop else count)) +
    geom_tile(color = "white") +
    geom_text(aes(label = if (show_prop) percent(prop, accuracy = 1) else count), size = 3.5) +
    scale_fill_gradient(low = "#FFEDC0", high = "#93003A", name = if (show_prop) "Proportion" else "Fréquence") +
    labs(
      title = "Heatmap globale ana × type",
      x = "ana",
      y = "type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  return(p)
}

heatmap_type_ana_globale <- heatmap_type_ana_generale(data) #
print(heatmap_globale) # pas assez précis pour en sortir une analyse 

## V2
heatmap_type_ana_par_livre <- function(df, show_prop = TRUE) {
  df_heat <- df %>%
    #filter(!is.na(ana), ana != "NA") %>% # à ajouter ou pas en fonction de ce qu'on veut voir
    group_by(livre, type, ana) %>%
    summarise(count = n(), .groups = "drop")
  
  if (show_prop) {
    df_heat <- df_heat %>%
      group_by(livre, type) %>%
      mutate(prop = count / sum(count)) %>%
      ungroup()
  }
  
  p <- ggplot(df_heat, aes(x = ana, y = type, fill = if (show_prop) prop else count)) +
    geom_tile(color = "white") +
    geom_text(aes(label = if (show_prop) percent(prop, accuracy = 1) else count), size = 3) +
    scale_fill_gradient(low = "#FFEDC0", high = "#93003A", name = if (show_prop) "Proportion" else "Fréquence") +
    labs(
      title = "Heatmap 'ana' × 'type' par livre",
      x = "ana",
      y = "type"
    ) +
    facet_wrap(~ livre, ncol = 5) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
  
  return(p)
}
heatmap_par_livre <- heatmap_type_ana_par_livre(data)
print(heatmap_par_livre)
ggsave("plots/heatmap_par_livre.png", plot = heatmap_par_livre, width = 10, height = 6)


# Présence narrative et discursive (persName et sp who)

## Comparaison narration vs discours direct (barplot groupé) ## not ding what I want yet 
### persName x ref (?) # crossing persName and sp-corresp
narration_df <- data %>%
  filter(!is.na(ref)) %>%
  group_by(ref) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(source = "narration")

### sp x corresp 
sp_nodes <- xml_find_all(ovid_deities, ".//sp")
sp_data <- tibble(
  who = xml_attr(sp_nodes, "who"),
  corresp = xml_attr(sp_nodes, "corresp"),
  ana = xml_attr(sp_nodes, "ana")
) %>%
  filter(!is.na(corresp), !is.na(who))

discours_df <- sp_data %>%
  filter(!is.na(corresp)) %>%
  group_by(corresp) %>%
  summarise(count = n(), .groups = "drop") %>%
  rename(ref = corresp) %>%
  mutate(source = "parole_directe")

comparaison_df <- bind_rows(narration_df, discours_df)

ggplot(comparaison_df, aes(x = ref, y = count, fill = source)) + # not what I want no
  geom_col(position = "dodge") +
  labs(title = "Présence des divinités : narration vs parole directe", x = "Divinité", y = "Nombre de mentions", fill = "Source") +
  theme_minimal() +
  scale_fill_manual(values = c("narration" = "#FF7EB6", "parole_directe" = "#93003A")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# --- Sauvegarde possible ---
ggsave("plots/comparaison_narration_discours.png", width = 8, height = 5)

      