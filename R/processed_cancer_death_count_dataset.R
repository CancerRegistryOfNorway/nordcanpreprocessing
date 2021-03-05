


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
    message(
      "* nordcanpreprocessing::",
      "nordcan_processed_cancer_death_count_dataset: there were ",
      nrow(undefined), " icd_version-icd_code combinations without ",
      "an entity definition (having some is normal); you can inspect ",
      "these in the object named \"._undefined\" in your workspace; use ",
      "e.g. print(._undefined)", "Most ICD7-ICD9-codes from 210 and above and",
      "ICD10-codes starting with 'D' are not supposed to get entity codes. ",
      "You can ignore these. If you have ICD7-ICD9-code in range 140-209 or ",
      "ICD10-codes starting with 'C' which do not get an entity, ",
      "contact Siri Larønningen (siri.laronningen@kreftregisteret.no)."
    )
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

  stratum_col_nms <- setdiff(names(x), "cancer_death_count")

  x <- x[
    i = !is.na(x[["entity"]]),
    j = lapply(.SD, sum),
    keyby = stratum_col_nms,
    .SDcols = "cancer_death_count"
  ]
  if (data.table::uniqueN(x[["region"]]) > 1L) {
    participant_info <- nordcancore::nordcan_metadata_participant_info()
    topregion_number <- participant_info[["topregion_number"]]
    subregion_number_set <- nordcancore::nordcan_metadata_column_level_space_list(
      "region"
    )[["region"]]
    subregion_number_set <- setdiff(subregion_number_set, topregion_number)
    x_subregions <- x[x[["region"]] %in% subregion_number_set, ]
    nonregion_stratum_col_nms <- setdiff(stratum_col_nms, "region")
    x_topregion <- x_subregions[
      j = lapply(.SD, sum),
      keyby = nonregion_stratum_col_nms,
      .SDcols = "cancer_death_count"
    ]
    x_topregion[, "region" := topregion_number]

    x <- rbind(x_topregion, x_subregions, use.names = TRUE)
  }

  ## Full combination of columns
  col_level <- setdiff(col_nms, c("cancer_death_count", "year"))
  print(col_level)
  full_comb <- nordcancore:::nordcan_metadata_column_level_space_dt(col_level)
  ## By default, full_comb contains all regions of the country.
  # full_comb <- full_comb[which(full_comb$region %in% unique(x$region)), ]
  ## agegroup
  ## full_comb <- full_comb[which(full_comb$agegroup %in% 1:18), ]

  yr <- range(x$year, na.rm = TRUE)
  x_fc <- data.table::rbindlist(lapply(yr[1]:yr[2],function(x) {full_comb$year = x; full_comb}))
  x <- merge(x_fc, x, all.x = TRUE)
  id <- which(is.na(x$cancer_death_count))
  if (length(id) >0) {x$cancer_death_count[id] <- 0L}


  data.table::setcolorder(x, stratum_col_nms)
  data.table::setkeyv(x, stratum_col_nms)
  return(x[])
}
