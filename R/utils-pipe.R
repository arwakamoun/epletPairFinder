# utils-pipe.R
# -----------------------------------------------
# Ce fichier centralise tous les imports necessaires pour le package
# afin d'eviter les warnings "no visible global function definition".

#' @importFrom dplyr %>% mutate filter left_join arrange count select distinct rename rename_with desc if_else
#' @importFrom tidyr pivot_wider unite replace_na
#' @importFrom stringr str_detect str_replace_all str_sub
#' @importFrom readxl read_excel
#' @importFrom tidytext unnest_tokens
#' @importFrom stats setNames
#' @importFrom rvest read_html html_table
#' @importFrom chromote ChromoteSession
#' @importFrom tibble as_tibble
NULL

