#' Calculer les resultats bruts des eplets selon un seuil MFI
#'
#' @param case_f Data frame contenant les informations du cas apres association avec les eplets
#'        Doit contenir au minimum les colonnes : 'MFI', 'eplet', 'self', 'exposition', 'status'
#' @param seuil Numeric. Seuil MFI au-dessus duquel un resultat est considere "positif". Par defaut 1000.
#'
#' @return Data frame avec les comptes de positifs et negatifs par eplet, auto-filtre sur les positifs uniquement
#' @export
#'
#' @examples
#' # res <- calculer_resultats(case_f, seuil = 500)
calculer_resultats <- function(case_f, seuil = 1000) {
  message(" Calcul des resultats selon le seuil MFI...")

  # Verification colonnes
  required_cols <- c("MFI", "eplet", "self", "exposition", "status")
  if (!all(required_cols %in% names(case_f))) {
    stop("case_f doit contenir les colonnes : ", paste(required_cols, collapse = ", "))
  }

  res <- case_f %>%
    dplyr::mutate(res = ifelse(MFI > seuil, "pos", "neg")) %>%
    dplyr::count(eplet, self, res, exposition, status) %>%
   tidyr::pivot_wider(names_from = res, values_from = n, values_fill = 0) %>%
    dplyr::arrange(desc(pos)) %>%
    dplyr::filter(is.na(neg) | neg == 0, !is.na(pos) & pos > 0)

  return(res)
}

# Exemple d'utilisation (a mettre dans les tests ou vignette, pas dans le package directement)
# res <- calculer_resultats(case_f, seuil = 500)
