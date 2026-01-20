# Analyse des catastrophes naturelles (1960–2018)

Projet d’analyse et de nettoyage d’une base de données mondiale de catastrophes naturelles,
avec validation des données et visualisation cartographique.

La base de données contient environ 40 000 événements entre 1960 et 2018.

## Objectifs du projet
- Nettoyer et uniformiser une base de données volumineuse
- Corriger les incohérences géographiques (pays, régions, villes)
- Compléter les coordonnées latitude/longitude manquantes
- Vérifier la cohérence globale des données
- Produire une carte mondiale lisible des catastrophes naturelles
- Tester des hypothèses globales à partir des données nettoyées

## Problèmes traités
- Données non uniformisées
- Doublons et identifiants incohérents
- Valeurs manquantes
- Colonnes redondantes
- Informations géographiques incomplètes ou incorrectes
- Coordonnées manquantes dans la colonne "geolocation"

## Données
- Source : Global Natural Disasters Dataset (1960–2018)
- https://www.kaggle.com/datasets/cyberevil545/global-natural-disasters-dataset-19602018

## Technologies utilisées
- R / RStudio
- Enrichissement géographique via ArcGIS et GeoNames
- GitHub pour la gestion du projet

## Structure et exécution
Exécution du projet :

1. Lancer "main1.R"  
   → nettoyage, correction, uniformisation et enrichissement géographique de la base de données  
   → génération de la base corrigée
   → analyses, vérifications, hypothèses et visualisations (carte mondiale)

Important:  ⚠️ L’étape de géolocalisation peut être longue selon la disponibilité du service ArcGIS.

