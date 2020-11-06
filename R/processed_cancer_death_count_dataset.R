


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
#'   cancer_death_count = 10L
#' )
#'
#' cdcd <- nordcan_processed_cancer_death_count_dataset(x = dt)
#' @importFrom data.table .SD
#' @export
nordcan_processed_cancer_death_count_dataset <- function(
  x
) {
  assert_unprocessed_cancer_death_count_dataset(x)

  conversion_dt <- nordcancore::nordcan_metadata_icd_by_version_to_entity()
  conversion_dt[, "was_defined" := TRUE]

  x <- merge(x, conversion_dt, by = c("icd_version", "icd_code"),
             all.x = TRUE, all.y = FALSE)
  x[is.na(x[["was_defined"]]), "was_defined" := FALSE]
  undefined <- x[x[["was_defined"]] == FALSE, ]
  if (nrow(undefined) > 0L) {
    undefined <- undefined[
      i = !duplicated(undefined, by = c("icd_version", "icd_code")),
      j = .SD,
      .SDcols = c("icd_version", "icd_code")
      ]
    message("* nordcanpreprocessing::",
            "nordcan_processed_cancer_death_count_dataset: there were ",
            nrow(undefined), " icd_version-icd_code combinations without ",
            "an entity definition (having some is normal); you can inspect ",
            "these in the object named \"._undefined\" in your workspace; use ",
            "e.g. print(._undefined)")
    ge <- globalenv()
    ge[["._undefined"]] <- undefined
  }
  x[, "was_defined" := NULL]

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

  if(length(unique(x$region)) == 1){
    stratum_col_nms <- setdiff(names(x), "cancer_death_count")
    y <- x[
      j = lapply(.SD, sum),
      keyby = stratum_col_nms,
      .SDcols = "cancer_death_count"
    ]
  }
  else {
    stratum_col_nms_1 <- setdiff(names(x), "cancer_death_count")
    x1 <- x[
      j = lapply(.SD, sum),
      keyby = stratum_col_nms_1,
      .SDcols = "cancer_death_count"
    ]

    stratum_col_nms_2 <- setdiff(names(x), c("region", "cancer_death_count"))
    x2 <- x1[
      j = lapply(.SD, sum),
      keyby = stratum_col_nms_2,
      .SDcols = "cancer_death_count"
    ]

    region_0 <- unique(x1$region[grepl('0$', x1$region)])

    x1 <- x1[!grepl('0$', x1$region), ]

    y <- rbind(x1, x2, fill = TRUE)
    y$region[is.na(y$region)] <- region_0
    data.table::setorderv(y, c("sex", "region", "agegroup", "entity", "year"))

  }

  return(y[j = .SD, .SDcols = col_nms][])
}
