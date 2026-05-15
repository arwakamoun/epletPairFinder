# creer_self.R
# 3 Fonction : Creer la table des eplets du soi
# -----------------------------------------------

#' Creer la table des eplets du soi
#'
#' Cette fonction identifie les eplets du soi a partir :
#' - soit du fichier Excel EpVix (feuille 2 si disponible, table des eplets copiee a partir de Epvix),
#' - soit automatiquement a partir des donnees issues de la fonction \code{ACC()}.
#'
#' @param soi_file Chemin vers le fichier contenant les eplets du soi. Default = NULL.
#' @param case Table issue de la fonction \code{ACC()}, contenant les alleles et eplets analyses.
#' @param clean_eplet Liste issue de la fonction `clean_eplet_data()` contenant les tables d'eplets
#'
#' @return Un tableau des eplets du soi (data.frame)
#' @export
#'
#' @examples
#' \dontrun{
#' eplet_soi <- creer_self(soi_file = NULL, case = case, clean_eplet = clean_eplet)
#' }

creer_self <- function(soi_file = NULL, case, clean_eplet) {
  message("Identification des eplets du soi...")

  # --- etape 1 : lecture depuis fichier si fourni ---
  soi_epvix <- NULL
  if (!is.null(soi_file)) {
    sheets <- tryCatch(readxl::excel_sheets(soi_file), error = function(e) NULL)
    if (!is.null(sheets) && length(sheets) > 1) {
      soi_epvix <- read_excel(soi_file, sheet = 2) %>%
        select( 3) %>%
        setNames(c("eplet")) %>%
        unnest_tokens(name, eplet, to_lower = FALSE,
                                token = "regex", pattern = "\\s+") %>%
        distinct()
      message(" Eplets du soi lus depuis la feuille Excel (EpVix).")
    } else {
      message(" Le fichier fourni n'a qu'une seule feuille : calcul automatique utilise.")
    }
  }



  # --- etape 2 : extraction du bon sous-ensemble ---
  clean_select <- clean_eplet[[1]]

  # --- etape 3 : si pas de fichier, creation automatique ---
  if (is.null(soi_epvix)) {
    eplet_soi <- case %>%
      filter(eplet == "[SELF]") %>%
      left_join(clean_select, by = c("allele")) %>%
      distinct() %>%
      select(name) %>%
      distinct()
    message(" Eplets du soi crees automatiquement a partir des donnees ACC.")
  } else {
    eplet_soi <- soi_epvix
  }

  return(eplet_soi)
}
