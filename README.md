# Déterminants comportementaux de l’aération des logements

**Étude économétrique — Master 1 Économie numérique**  
Réalisée par **Soufiane Manouach et Richard Larri**

## Présentation

Ce projet étudie les facteurs associés à l’aération quotidienne des logements. Il s’intéresse notamment aux arbitrages des ménages entre qualité de l’air intérieur, sensibilité au froid, nuisances sonores et contraintes du logement.

L’analyse s’appuie sur l’enquête Habitat-Santé 2024 et cherche à identifier les facteurs liés à la probabilité qu’un ménage aère son logement quotidiennement.

## Données

La base initiale comprend 200 observations. Après le traitement des valeurs manquantes et aberrantes, l’échantillon d’analyse comporte **193 ménages**.

La variable expliquée est binaire : le ménage aère ou n’aère pas quotidiennement. Les variables étudiées comprennent notamment la présence d’enfants, la sensibilité au froid, la connaissance des COV, le type de logement, le revenu du foyer et les nuisances sonores.

## Méthode

Le script R réalise les étapes suivantes :

- vérification et nettoyage des données ;
- traitement des valeurs manquantes et d’une valeur hors échelle pour la sensibilité au froid ;
- statistiques descriptives, comparaisons par groupes et corrélations ;
- visualisations des relations entre variables ;
- estimation d’un modèle Logit complet, puis d’un modèle réduit ;
- interprétation à l’aide des odds ratios et des effets marginaux ;
- examen de la qualité d’ajustement et de la capacité de classification du modèle.

## Résultats principaux

Dans l’échantillon analysé, **52,8 % des ménages déclarent aérer quotidiennement**.

Quatre facteurs ressortent comme statistiquement associés à l’aération dans le modèle présenté :

- **Présence d’un enfant de moins de 10 ans** : association positive, avec un odds ratio de 6,9.
- **Connaissance des COV** : association positive, avec un odds ratio de 8,2.
- **Sensibilité au froid** : association négative, avec un odds ratio de 0,37 par point supplémentaire sur l’échelle.
- **Type de logement** : vivre en maison est associé à une probabilité d’aération plus élevée que vivre en appartement, avec un odds ratio de 25,8.

Le rapport présente également un pseudo-R² de McFadden de 0,68, une AUC de 0,955 et un taux de classification de 93,3 %. Ces indicateurs sont rapportés pour l’échantillon utilisé dans l’étude ; aucune validation sur un échantillon externe n’est présentée.

## Pistes d’action discutées

La note opérationnelle propose trois leviers : mieux informer sur les polluants de l’air intérieur, tenir compte des contraintes liées à la précarité énergétique et intégrer la ventilation aux projets de rénovation thermique.

## Limites

L’analyse porte sur 193 ménages et repose sur les variables et mesures disponibles dans l’enquête. Les résultats montrent des associations dans cet échantillon ; ils ne démontrent pas à eux seuls des relations de causalité.

## Outils

- R
- `tidyverse`
- `ggplot2`
- `stargazer`
- `pROC`
- `margins`
- `lmtest`
- `car`

## Fichiers du projet

- Script R d’analyse économétrique
- Note opérationnelle au format PDF

Le script attend aussi un fichier `data_air.csv`. Celui-ci n’est pas inclus pour le moment. Le chemin d’accès défini dans le script est propre à un ordinateur : il devra être adapté pour permettre à une autre personne de l’exécuter.
