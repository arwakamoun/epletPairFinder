#' Associer les eplets du soi avec la recherche d'Ac anti-HLA
#'
#' @param case Data frame contenant les informations du cas (avec colonnes 'allele', 'locus')
#' @param seplet_nsallele Data frame contenant les eplets non-soi associes aux alleles (avec colonnes 'allele', 'locus')
#'
#' @return Data frame resultant de la jointure entre le cas et les eplets non-soi
#' @export
#'
#' @examples
#' # associer_acc(case, seplet_nsallele)
associer_acc <- function(case, seplet_nsallele) {
  message(" Association des eplets soi / sur tous les alleles de l'ACC...")

  case_f <- case %>%
    left_join(
      seplet_nsallele,
      by = c("allele", "locus"),
      relationship = "many-to-many"
    )

  return(case_f)
}

# Exemple d'utilisation (a mettre dans les tests ou vignette, pas dans le package directement)
# case_f <- associer_acc(case, seplet_nsallele)
