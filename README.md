# Le rôle des divinités dans les *Métamorphoses* d’Ovide : étude descas de Minerve, Apollon et Jupiter — Mémoire M2 Humanités classiques et humanités numériques

Bienvenue dans ce dépôt dédié au mémoire de Master 2 consacré à l’analyse des interventions divines dans les *Métamorphoses* d’Ovide, dans une perspective d’encodage XML-TEI et d’analyse quantitative avec R.

## 🎓 Sujet du mémoire

Ce travail interroge les rôles actanciels et les modalités d’intervention de trois divinités, Jupiter, Minerve et Apollon, dans le récit des *Métamorphoses*. À travers l'encodage structuré du texte d’Ovide et de l'exploration numérique (TEI, R), il vise à croiser les niveaux narratifs avec les profils d’action divins.

## 📁 Contenu du dépôt

Le dépôt contient :

- le code (commenté) d’analyse et de traitement de données en R,
- le texte encodé des *Métamorphoses* au format XML-TEI,
- les figures et résultats exploratoires produits à partir des données,
- toutes les parties du mémoire, ainsi que la bibliographie. 

### 📌 Fichiers importants

- `index.qmd` : fichier de démarrage principal du projet Quarto (au format `book`)contenant une partie des métadonnées du mémoire.
- `data/ovid_MIN_APO_IUP.xml` : dossier contenant les fichiers XML-TEI encodés à partir des *Métamorphoses*.
- `scripts/data_deities.R` : script de traitement et mise en forme des données encodées sur les divinités, à partir des fichiers XML.
- `scripts/lexical_analysis.R` : script d’analyse lexicale du corpus, incluant bigrams, co-occurrences et nuages de mots.

## 📖 Visualisation du mémoire (HTML)

Si vous souhaitez parcourir le mémoire dans sa version **navigable en HTML**, vous pouvez tlécharger l'ensemble du dossier et l'ouvrir localement, ou bien consulter directement la version publiée en ligne ici 🔗: [à venir]

## ✨ Édition numérique des Métamorphoses
Une édition enrichie des livres encodés des _Métamorphoses_ est disponible dans un format interactif publié grâce à [EVT 1](https://github.com/evt-project/evt-viewer-angular) (Edition Visualization Technology), intégrant les annotations réalisées pour ce devoir, les balises de discours de Nadine Rakofsky, et les profils d’intervention divine.
🔗 [à venir]

Merci de votre intérêt pour ce travail.
N’hésitez pas à contribuer ou à poser des questions via les issues du dépôt.

_The base of this repo is a template repo for generating a manuscript from Quarto that accompanies the tutorial at: [Quarto Manuscripts: RStudio](https://quarto.org/docs/manuscripts/authoring/rstudio.html)_

