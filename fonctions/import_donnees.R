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
