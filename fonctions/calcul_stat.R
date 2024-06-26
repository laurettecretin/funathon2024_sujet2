calcul_donnees_annuelles_aeroports <- function(df, choix_annee){
  
  df |> 
    filter(annee == choix_annee) |> 
    group_by(apt, apt_nom) |> 
    summarise(
      paxdep = round(sum(apt_pax_dep, na.rm = TRUE), 3),
      paxarr = round(sum(apt_pax_arr, na.rm = TRUE), 3),
      paxtra = round(sum(apt_pax_tr, na.rm = TRUE), 3)) |> 
    arrange(desc(paxdep)) |> 
    ungroup()
  
}

plot_aeroports_top10 <- function(df, choix_annee){
  
  df |> 
    calcul_donnees_annuelles_aeroports(choix_annee) |> 
    slice_max(paxdep, n = 10) |> 
    mutate(apt_nom = fct_reorder(apt_nom, paxdep)) |> 
    ggplot(aes(x = apt_nom, y = paxdep)) + 
    geom_col(fill = "powderblue") + 
    coord_flip() +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = " ")) +
    theme_minimal() + 
    labs(title = "Top 10 des aéroports en nombre de passagers au départ",
         sutitle = glue("En {choix_annee}"),
         x = "", y = "")
  
}


plot_aeroport <- function(df, choix_aeroport){
  
  df <- df |> 
    filter(nom_aeroport == choix_aeroport)
  
  nom_aeroport <- df |> distinct(apt_nom) |> pull()
  
  df |> 
    mutate(trafic = apt_pax_dep + apt_pax_tr + apt_pax_arr,
           date = ymd(glue("{anmois}01"))) |> 
    ggplot(aes(x = date, y = trafic)) + 
    geom_area_pattern(pattern = "gradient", 
                      fill = "#00000000",
                      pattern_fill  = "#00000000",
                      pattern_fill2 = "powderblue") +
    geom_line(color="powderblue", size=2) +
    geom_point(shape = 21, size=3, color="powderblue", fill = "white") +
    scale_x_continuous(breaks = 2018:2022) + 
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = " ")) +
    theme_minimal() +
    labs(title = nom_aeroport,
         x = "", y = "")
  
}




creer_carte_interactive <- function(df_aero, df_localisation, choix_annee){
  
  palette <- c("green", "blue", "red")
  
  trafic_aeroports <- df_localisation |> 
    inner_join(df_aero |> 
                 filter(annee == choix_annee) |> 
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
                      label = ~ glue("{str_to_title(Nom)} : {trafic_tot} voyageurs")) |> 
    setView(2, 46, zoom = 5)
  
  
}



creer_tableau <- function(df, choix_annee){
  
  df |> 
    calcul_donnees_annuelles_aeroports(choix_annee) |> 
    slice_max(paxdep, n = 10) |> 
    select(-apt) |> 
    mutate(across(starts_with("pax"), ~format(.x, scientific = FALSE, big.mark = " "))) |> 
    gt() |> 
    tab_header(title = md("**Statistiques de fréquentation**"),
               subtitle = md("Classement des aéroports")) |> 
    cols_label(apt_nom = md("**Aéroport**"),
               paxdep = md("**Départs**"),
               paxarr = md("**Arrivée**"),
               paxtra = md("**Transit**")) |> 
    tab_style(style = cell_fill(color = "powderblue"),
              locations = cells_title()) |> 
    tab_source_note(source_note = md("_Source: DGAC, à partir des données sur data.gouv.fr_"))
  
  
}
