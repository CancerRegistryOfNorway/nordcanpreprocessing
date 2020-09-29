

#' @title NORDCAN Processed Cancer Record Dataset
#' @description
#' Form the NORDCAN processed cancer record dataset.
#' @param x `[data.table]` (mandatory, no default)
#'
#' the NORCDAN cancer record dataset before processing (as documented in
#' call for data)
#' @param iarccrgtools_exe_path `[character]` (mandatory, no default)
#'
#' passed to [iarccrgtools_tool]
#' @export
#' @importFrom data.table :=
nordcan_processed_cancer_record_dataset <- function(
  x,
  iarccrgtools_exe_path
) {
  x_work_col_nms <- nordcancore::nordcan_metadata_column_name_set(
    "column_name_set_unprocessed_cancer_record_dataset"
  )
  dbc::assert_user_input_is_data.table_with_required_names(
    x = x,
    required_names = x_work_col_nms
  )
  dbc::assert_user_input_file_exists(iarccrgtools_exe_path)

  message("* nordcanpreprocessing::nordcan_processed_cancer_record_dataset: ",
          "validating input dataset...")
  report_df <- report_dataset_is_valid(
    x = x,
    dataset_name = "unprocessed_cancer_record_dataset"
  )
  x_work <- data.table::setDT(mget(x_work_col_nms, as.environment(x)))
  data.table::setnames(x_work, names(x_work), x_work_col_nms)
  x_work[, "problem" := NA_character_]

  lapply(1:nrow(report_df), function(test_no) {
    wh_fail <- report_df[["wh_fail"]][[test_no]]
    if (length(wh_fail) > 0L && !(length(wh_fail) == 1L && is.na(wh_fail))) {
      new_values <- data.table::fifelse(
        is.na(x_work[["problem"]][wh_fail]),
        report_df[["message"]][test_no],
        paste0(
          x_work[["problem"]][wh_fail], "; ", report_df[["message"]][test_no]
        )
      )

      data.table::set(
        x_work,
        i = wh_fail,
        j = "problem",
        value = new_values
      )
    }
  })

  n_problematic <- sum(!is.na(x_work[["problem"]]))
  if (n_problematic > 0L) {
    message("* nordcanpreprocessing::nordcan_processed_cancer_record_dataset: ",
            n_problematic, " observations had problems; returned a copy ",
            "of your original dataset with new column \"problem\" to identify ",
            "what is wrong with each observation (where applicable; no ",
            "problem = NA)")
    x_work <- data.table::copy(x_work)
    return(x_work)
  }
  x_work[, "problem" := NULL]

  message("* nordcanpreprocessing::nordcan_processed_cancer_record_dataset: ",
          "creating new columns...")
  gs <- nordcancore::get_global_nordcan_settings()
  iarc_wd <- gs[["iarccrgtools_work_dir"]]
  x <- enrich_nordcan_cancer_record_dataset(
    x = x_work,
    iarccrgtools_exe_path = iarccrgtools_exe_path,
    iarccrgtools_work_dir = iarc_wd
  )

  message("* nordcanpreprocessing::nordcan_processed_cancer_record_dataset: ",
          "finished.")
  return(x[])
}

