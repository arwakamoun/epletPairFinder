#' Creer la table des eplets du soi sur tous les alleles du meme locus
#'
#' @param eplet_soi Data frame contenant les eplets du soi avec colonnes 'allele', 'name', 'locus'
#' @param clean_eplet Liste issue de la fonction `clean_eplet_data()` contenant les tables d'eplets
#' @param choix Entier (1, 2 ou 3) pour choisir le sous-ensemble de clean_eplet
#'
#' @return Data frame associant chaque eplet du soi a tous les alleles du meme locus
#' @export
#'
#' @examples
#' # creer_non_self(eplet_soi, clean_eplet, choix = 1)
creer_non_self <- function(eplet_soi, clean_eplet, choix = 1) {
  message(" Construction des eplets du soi sur tous les alleles du meme locus...")

  # Verification du choix
  if (!choix %in% 1:3) {
    stop("Argument 'choix' doit etre 1, 2 ou 3 :
1 = all_eplets, 2 = conf_eplets, 3 = exp_eplets")
  }

  # Verification des colonnes necessaires
  required_cols <- c("locus", "name")
  if (!all(required_cols %in% names(eplet_soi))) {
    stop(" eplet_soi doit contenir les colonnes : 'locus', 'name'")
  }
  if (!all(required_cols %in% names(clean_eplet[[choix]]))) {
    stop("clean_eplet[[choix]] doit contenir les colonnes : 'locus', 'name'")
  }

  # Selection du sous-ensemble choisi
  selected_select <- clean_eplet[[choix]]

  # Jointure sur locus et name
  result <- eplet_soi %>%
    left_join(selected_select, by = c("locus", "name")) %>%
    select(allele, name, locus) %>%
    rename(self = name)

  return(result)
}
