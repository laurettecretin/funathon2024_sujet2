library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(leaflet)

source("fonctions/import_donnees.R")


# import des données ----
urls_donnees <- creer_liste_donnees("sources.yml")

donnees_aeroports <- importer_donnees_aeroports(unlist(urls_donnees$airports))
donnees_compagnies <- importer_donnees_compagnies(unlist(urls_donnees$compagnies))
donnees_liaisons <- importer_donnees_liaisons(unlist(urls_donnees$liaisons))


# localisation des aéroports
localisation_aeroports <- st_read(urls_donnees$geojson$airport)


leaflet(localisation_aeroports) |> 
  addTiles() |> 
  addMarkers(popup = ~Nom)
