# ACC.R
# 2 Fonction : Preparation du fichier ACC (analyse des anticorps anti-HLA a partir du fichier EpVix)

#' Preparation du fichier ACC
#'
#' Cette fonction nettoie et prepare le fichier Excel issu du site EpVix (.xls qui doit etre enregistre en .xlsx)
#' contenant l'analyse des anticorps anti-HLA.
#'
#' @param file Chemin vers le fichier .xlsx a importer
#' @param clean_eplet Liste issue de la fonction `clean_eplet_data()` contenant les tables d'eplets
#'
#' @return Un tableau (data.frame) nettoye et enrichi des eplets
#' @export
#'
#' @examples
#' \dontrun{
#' case <- ACC("exemple.xlsx", clean_eplet)
#' }

ACC <- function(file, clean_eplet) {
  # Verification du type de fichier
  if (grepl("\\.xls$", file, ignore.case = TRUE)) {
    stop(paste0(
      "Le fichier '", basename(file),
      "' est au format .xls (Excel ancien).\n",
      "Ce format n'est pas compatible avec readxl sur GitHub ou Shiny.\n",
      "Veuillez l'ouvrir dans Excel et le reenregistrer en .xlsx avant de le charger."
    ))
  }

  # Lecture du fichier .xlsx sans warnings
  read_excel_without_warnings <- function(file) {
    suppressMessages({
      suppressWarnings({
        readxl::read_excel(file, skip = 12)
      })
    })
  }

  case <- read_excel_without_warnings(file)

  # Nettoyage et preparation
  case <- case %>%
    dplyr::rename(allele = Alelo) %>%
    tidyr::replace_na(list(Epitopos = "[=self]")) %>%
    tidyr::unite("eplet", -c(allele, MFI), sep = " ") %>%
    dplyr::mutate(eplet = stringr::str_replace_all(eplet, pattern = "\"", "")) %>%
    tidytext::unnest_tokens(eplet, eplet, to_lower = FALSE, token = "regex", pattern = "\\s+") %>%
    dplyr::filter(eplet != "NA", !stringr::str_detect(eplet, "\\+")) %>%
    dplyr::mutate(locus = stringr::str_sub(allele, 1, 1)) %>%
    dplyr::left_join(
      clean_eplet[[1]][c("allele", "name", "exposition", "status")],
      by = c("allele", "eplet" = "name")
    )

  return(case)
}
