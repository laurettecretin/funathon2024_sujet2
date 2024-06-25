library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(lubridate)
library(glue)
library(ggplot2)
library(plotly)
library(forcats)
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


# 1 - visualisation graphique

liste_aeroports <- donnees_aeroports |> distinct(apt)
liste_aeroports


plot_aeroport <- function(choix_aeroport){
  
  df <- donnees_aeroports |> 
    filter(apt == choix_aeroport)
  
  nom_aeroport <- df |> distinct(apt_nom) |> pull()
  
  graph <- df |> 
    mutate(trafic = apt_pax_dep + apt_pax_tr + apt_pax_arr,
           date = ymd(glue("{anmois}01"))) |> 
    ggplot(aes(x = date, y = trafic)) + 
    geom_line() + 
    geom_point() +
    labs(title = glue("{choix_aeroport} - {nom_aeroport}"))
  
  ggplotly(graph)
}

plot_aeroport("FMCZ")
plot_aeroport("LFAQ")

calcul_donnees_annuelles_aeroports <- function(choix_annee){
  
  donnees_aeroports |> 
    filter(annee == choix_annee) |> 
    group_by(apt, apt_nom) |> 
    summarise(
      paxdep = round(sum(apt_pax_dep, na.rm = TRUE), 3),
      paxarr = round(sum(apt_pax_arr, na.rm = TRUE), 3),
      paxtra = round(sum(apt_pax_tr, na.rm = TRUE), 3)) |> 
    arrange(desc(paxdep)) |> 
    ungroup()
}

calcul_donnees_annuelles_aeroports(2018)   

# top 10
calcul_donnees_annuelles_aeroports(2019) |> 
  slice_max(paxdep, n = 10) |> 
  mutate(apt_nom = fct_reorder(apt_nom, paxdep)) |> 
  ggplot(aes(x = apt_nom, y = paxdep)) + geom_col() + coord_flip()

plot_aeroports_top10 <- function(choix_annee){
  calcul_donnees_annuelles_aeroports(choix_annee) |> 
    slice_max(paxdep, n = 10) |> 
    mutate(apt_nom = fct_reorder(apt_nom, paxdep)) |> 
    ggplot(aes(x = apt_nom, y = paxdep)) + 
    geom_col() + 
    coord_flip() +
    labs(title = choix_annee)
}

plot_aeroports_top10(2020)

donnees_aeroports |> distinct(annee)
