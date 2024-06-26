library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(lubridate)
library(glue)
library(ggplot2)
library(plotly)
library(gt)
library(forcats)
library(leaflet)

source("fonctions/import_donnees.R")
source("fonctions/calcul_stat.R")


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

palette <- c("green", "blue", "red")

trafic_aeroports <- localisation_aeroports |> 
  inner_join(donnees_aeroports |> 
               filter(annee == 2019) |> 
               group_by(apt, nom_aeroport) |> 
               summarise(paxdep = sum(apt_pax_dep, na.rm = TRUE),
                         paxarr = sum(apt_pax_arr, na.rm = TRUE),
                         paxtra = sum(apt_pax_tr, na.rm = TRUE)) |> 
               mutate(trafic_tot = paxdep + paxarr + paxtra),
             by = c("Code.OACI" = "apt")) |> 
  mutate(volume = ntile(trafic, 3),
         color = palette[volume])

icons <- awesomeIcons(
  icon = 'plane',
  iconColor = 'black',
  library = 'fa',
  markerColor = trafic_aeroports$color
)



leaflet(trafic_aeroports) |> 
  addTiles() |> 
  addAwesomeMarkers(icon=icons[],
                    label = ~ glue("{str_to_title(Nom)} : {trafic_tot} voyageurs")) 


# pour vérification
creer_carte_interactive(donnees_aeroports, localisation_aeroports, 2018)



# visualisation graphique

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



# tableau

donnees_aeroports |> 
  calcul_donnees_annuelles_aeroports(2018) |> 
  slice_max(paxdep, n = 10) |> 
  select(-apt) |> 
  gt() |> 
  tab_header(
    title = md("**Statistiques de fréquentation**"),
    subtitle = md("Classement des aéroports")
  ) |> 
  cols_label(
    apt_nom = md("**Aéroport**"),
    paxdep = md("**Départs**"),
    paxarr = md("**Arrivée**"),
    paxtra = md("**Transit**")
  ) |> 
  tab_style(
    style = cell_fill(color = "powderblue"),
    locations = cells_title()
  ) |> 
  tab_source_note(source_note = md("_Source: DGAC, à partir des données sur data.gouv.fr_"))



donnees_aeroports |> creer_tableau(2020)
