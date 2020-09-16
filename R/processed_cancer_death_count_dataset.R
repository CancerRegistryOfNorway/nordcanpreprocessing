


#' @title NORDCAN Cancer Death Count Dataset
#' @description
#' Create the NORDCAN cancer death count dataset from raw data.
#' @param x `[data.table]` (mandatory, no default)
#'
#' the dataset of death counts as per the call for data
#' @examples
#' library("data.table")
#' dt <- data.table::data.table(
#'   year = 2018L,
#'   sex = 1L,
#'   region = 11L,
#'   agegroup = 1L,
#'   icd_code = "1751",
#'   icd_version = 7L,
#'   death_count = 10L
#' )
#'
#' cdcd <- nordcan_processed_cancer_death_count_dataset(x = dt)
#' @importFrom data.table .SD
nordcan_processed_cancer_death_count_dataset <- function(
  x
) {
  assert_dataset_is_valid(x, "unprocessed_cancer_death_count_dataset")

  conversion_dt <- nordcancore::nordcan_metadata_icd_by_version_to_entity()

  x <- merge(x, conversion_dt, by = c("icd_version", "icd_code"),
             all.x = TRUE, all.y = FALSE)

  col_nms <- nordcancore::nordcan_metadata_column_name_set(
    "processed_cancer_death_count_dataset"
  )
  return(x[j = .SD, .SDcols = col_nms][])
}


