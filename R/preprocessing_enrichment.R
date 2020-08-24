




#' @title NORDCAN Cancer Case Dataset
#' @description
#' Add new necessary columns to the cancer case dataset for use in computing
#' statistics.
#' @param x `[data.frame]` (mandatory, no default)
#' dataset of cancer cases
#' @section Format of input dataset:
#' See https://github.com/CancerRegistryOfNorway/NORDCAN/wiki/Call-for-data---Incidence
#' @section New columns:
#' See https://github.com/CancerRegistryOfNorway/NORDCAN/wiki/Module-PreprocessEnrich
#'
#' @importFrom data.table setDT copy month year
#' @importFrom dbc assert_is_data.frame
#' @export
enrich_nordcan_cancer_case_dataset <- function(x) {
  # source of definitions:
  # https://github.com/CancerRegistryOfNorway/NORDCAN/wiki/Module-PreprocessEnrich
  dbc::assert_is_data.frame(x)

  # take a copy so the original object is not affected
  x <- data.table::setDT(data.table::copy(x))

  # definitions
  x[, "mob" := data.table::month(birth_date)]
  x[, "moy" := data.table::year(birth_date)]
  # sasha: add more definitions here. any definition not requiring
  # IARC CRG Tools. you also cannot create the entity columns here.

  return(x)
}





