#' Créer une liste d'urls contenant les données à importer
#'
#' @param chemin - nom du fichier contenant les urls
#'
#' @return liste
#'
#' @examples
#' creer_liste_donnees("sources.yml")
creer_liste_donnees <- function(chemin){
  yaml::read_yaml(chemin)
}


#' Mettre en forme les données importées
#'
#' @param df data.frame contenant les données à mettre en forme
#'
#' @return data.frame avec deux nouvelles colonnes : mois & année + noms de colonnes en minuscules
#'
#' @examples
nettoyer_donnees <- function(df){
  df |> 
    rename_with(str_to_lower) |> 
    mutate(annee = str_sub(anmois, 1, 4),
           mois = str_sub(anmois, 5, 6),
           mois = as.character(as.numeric(mois)))
}

#' Importer les données des aéroports
#'
#' @param urls_aeroports urls contenant les données à importer
#'
#' @return data.frame contenant les données
#' 
#' @examples
importer_donnees_aeroports <- function(urls_aeroports){
  read_csv2(unlist(urls_aeroports), 
            col_types = cols(
              ANMOIS = col_character(),
              APT = col_character(),
              APT_NOM = col_character(),
              APT_ZON = col_character(),
              .default = col_double())) |> 
    nettoyer_donnees()
     
}

#' Importer les données des compagnies
#'
#' @param urls_compagnies urls contenant les données à importer
#'
#' @return data.frame contenant les données
#' 
#' @examples
importer_donnees_compagnies <- function(urls_compagnies){
  read_csv2(unlist(urls_compagnies), 
            col_types = cols(
              ANMOIS = col_character(),
              CIE = col_character(),
              CIE_NOM = col_character(),
              CIE_NAT = col_character(),
              CIE_PAYS = col_character(),
              .default = col_double())) |> 
    nettoyer_donnees()
  
}


#' Importer les données des liaisons
#'
#' @param urls_liaisons urls contenant les données à importer
#'
#' @return data.frame contenant les données
#' 
#' @examples
importer_donnees_liaisons <- function(urls_liaisons){
  read_csv2(unlist(urls_liaisons), 
            col_types = cols(ANMOIS = col_character(),
                             LSN = col_character(),
                             LSN_DEP_NOM = col_character(),
                             LSN_ARR_NOM = col_character(),
                             LSN_SCT = col_character(),
                             LSN_FSC = col_character(),
                             .default = col_double())) |> 
    nettoyer_donnees()
  
}

