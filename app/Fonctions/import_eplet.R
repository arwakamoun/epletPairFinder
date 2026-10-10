#' Importer et nettoyer les donnees d'eplets HLA
#'
#' Ces fonctions permettent d'importer les donnees depuis un fichier Excel local
#' ou directement depuis le site epregistry.com.br, puis de les nettoyer pour
#' preparer l'analyse des eplets.
#'
#' @param local_excel Chemin vers un fichier Excel local (.xlsx).
#' @param url URL du site EpRegistry (par defaut, https://www.epregistry.com.br/databases/ABC).
#'
#' @return Une liste de tibbles contenant :
#' \itemize{
#'   \item all_eplets - tous les eplets
#'   \item confirmed - les eplets confirmes
#'   \item exposed - les eplets exposes
#' }
#' @export
#'
#' @importFrom dplyr select rename_with mutate filter if_else left_join
#' @importFrom dplyr arrange count desc distinct
#' @importFrom stringr str_remove_all str_detect str_sub str_replace_all
#' @importFrom tidytext unnest_tokens
#' @importFrom readxl read_excel
#' @importFrom rvest read_html html_table
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider unite replace_na
#' @importFrom chromote ChromoteSession
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' clean_eplet <- import_and_clean_eplets()
#' }

# ---- 1 Fonction d'import ----
#' @export
get_epregistry_data <- function(local_excel = NULL,
                                url = "https://www.epregistry.com.br/databases/ABC") {

  if (!is.null(local_excel)) {
    message("Lecture du fichier local : ", local_excel)

    if (!file.exists(local_excel)) {
      stop("Le fichier specifie n'existe pas : ", local_excel)
    }

    data <- read_excel(local_excel)
    return(tibble::as_tibble(data))
  }

  message("Aucun fichier local fourni. Telechargement depuis le site...")

  b <- chromote::ChromoteSession$new()
  b$Page$navigate(url)
  Sys.sleep(10)

  html_doc <- b$DOM$getDocument()
  page_source <- b$DOM$getOuterHTML(nodeId = html_doc$root$nodeId)$outerHTML
  doc <- rvest::read_html(page_source)
  tables <- rvest::html_table(doc, fill = TRUE)

  if (length(tables) == 0) {
    stop("Aucune table trouvee sur la page. Verifiez l'URL ou augmentez le temps d'attente.")
  }

  message("Table telechargee avec succes.")
  return(tibble::as_tibble(tables[[1]]))
}

# ---- 2 Fonction de nettoyage ----
#' Clean eplet data
#'
#' Fonction interne utilisee pour nettoyer les donnees des eplets (un eplet et un allele HLA par ligne avec le locus correspondant)
#'
#' @param raw_data Un data.frame contenant les donnees brutes issus de la fonction get_epregistry_data.
#'
#' @return Un data.frame nettoye.
#'
#' @export
clean_eplet_data <- function(raw_data) {

  all_eplets <- raw_data %>%
    dplyr::select(c(1:2, 5:6, 9)) %>%
    dplyr::rename_with(~ tolower(stringr::str_remove_all(.x, "\\*"))) %>%
    dplyr::rename_with(~ "alleles", .cols = 5) %>%
    dplyr::mutate(status = dplyr::if_else(status == "Confirmed", "confirmed", "not_confirmed")) %>%
    tidytext::unnest_tokens(
      allele, alleles,
      token = "regex", to_lower = FALSE, pattern = ",\\s+"
    ) %>%
    dplyr::filter(!stringr::str_detect(name, "\\+")) %>%
    dplyr::mutate(locus = stringr::str_sub(allele, 1, 1))

  conf_eplets <- all_eplets %>% dplyr::filter(status == "confirmed")
  exp_eplets  <- all_eplets %>% dplyr::filter(exposition == "High")

  message("Donnees d'eplets nettoyees avec succes.")
  return(list(
    all_eplets = all_eplets,
    confirmed = conf_eplets,
    exposed = exp_eplets
  ))
}

# ---- 3 Fonction globale ----
#' Import and clean eplet data
#'
#' Importe soit un fichier Excel local, soit les eplets Classe I a partir
#' du registre EpRegistry, puis applique un nettoyage standardise.
#'
#' Cette fonction combine l'importation des donnees et leur nettoyage via
#' \code{\link{clean_eplet_data}}.
#'
#' @param local_excel Chemin vers un fichier Excel local contenant les eplets,
#' ou \code{NULL} pour telecharger les donnees depuis EpRegistry.
#' @param url URL de la base de donnees EpRegistry a utiliser si
#' \code{local_excel = NULL}.
#'
#' @return Un \code{data.frame} contenant les eplets nettoyes.
#'
#' @export

chromote::set_chrome_args(
  c(
    chromote::default_chrome_args(),
    "--no-sandbox",
    "--disable-dev-shm-usage"
  )
)

import_and_clean_eplets <- function(local_excel = NULL,
                                    url = "https://www.epregistry.com.br/databases/ABC") {
  message("Import et nettoyage des donnees EpRegistry...")
  raw_data <- get_epregistry_data(local_excel = local_excel, url = url)
  clean <- clean_eplet_data(raw_data)
  return(clean)
}
