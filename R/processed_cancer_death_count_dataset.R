


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
#' @export
nordcan_processed_cancer_death_count_dataset <- function(
  x
) {
  assert_dataset_is_valid(x, "unprocessed_cancer_death_count_dataset")

  conversion_dt <- nordcancore::nordcan_metadata_icd_by_version_to_entity()

  x <- merge(x, conversion_dt, by = c("icd_version", "icd_code"),
             all.x = TRUE, all.y = FALSE)

  col_nms <- nordcancore::nordcan_metadata_column_name_set(
    "column_name_set_processed_cancer_death_count_dataset"
  )
  entity_col_nms <- nordcancore::nordcan_metadata_column_name_set(
    "column_name_set_entity"
  )
  x <- data.table::rbindlist(lapply(entity_col_nms, function(entity_col_nm) {
    sub_x_col_nms <- union(entity_col_nm, setdiff(col_nms, "entity"))
    sub_x <- x[j = .SD, .SDcols = sub_x_col_nms]
    data.table::setnames(sub_x, entity_col_nm, "entity")
    sub_x[]
  }))
  stratum_col_nms <- setdiff(names(x), "death_count")
  x <- x[
    j = lapply(.SD, sum),
    keyby = stratum_col_nms,
    .SDcols = "death_count"
  ]

  return(x[j = .SD, .SDcols = col_nms][])
}


