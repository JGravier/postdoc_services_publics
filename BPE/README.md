### Description
Code et données liés à l'étude de l'évolution des services publics à partir de la Base Permanente des Équipements, BPE en 2009, 2013, 2018.
La BPE est une source statistique qui fournit le niveau d'équipements et de services rendus à la population sur un territoire. Les résultats sont proposés sous forme de bases de données dans différents formats et pour deux niveaux géographiques : les communes et les Iris

### Objectifs  
Les questions portent en particulier sur la diminution et la rétraction de l’offre de services publics intra-urbains en France métropolitaine depuis la mise en place de la RGPP (2007).

### Données et licence des données
#### BPE
L'ensemble des données de la BPE sont produites par l'INSEE et ont été téléchargées depuis le site internet de l'INSEE : 
1) voir les métadonnées générales de la BPE : http://www.insee.fr/fr/metadonnees/source/serie/s1161
2) voir l'arrêté portant sur la création d'une base permanente des équipements : http://www.legifrance.gouv.fr/affichTexte.do?cidTexte=LEGITEXT000018041390&dateTexte=20190411

_localisation : data/ensemble des fichiers contenant la mention "bpeDate"

#### Données population
1) Données produites par l'INSEE : Chiffres détaillés - Séries historiques de population (1876 à 2015), France hors Mayotte - Communes 
2) Complétées pour les arrondissements de Lyon et Marseille pour l'année 2009 et 2013 par les données Population légales en 2009 et 2013. Voir : http://www.insee.fr/fr/statistiques/2119804 et http://www.insee.fr/fr/statistiques/2119504

_localisation : data/populations_communales_arrondissements_donnees_temporelles_geographie_2017.xlsx

#### Données spatiales
Trois fichiers :
1) AU2010_au_01-01-2019.xls : données produites par l'INSEE, Aires urbaines 2010 - France ; Liste des aires urbaines 2010, géographie 2019. 
2) COMMUNES (shp) : données produites par l'IGN, ADMIN-EXPRESS-COG édition 2019 France entière
3) REGION (shp) : données produites par l'IGN, ADMIN-EXPRESS-COG édition 2019 France entière

_localisation : data/data_communes_au

### Réalisation
R version 3.6.1 (2019-07-05) -- "Action of the Toes"
Copyright (C) 2019 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)
