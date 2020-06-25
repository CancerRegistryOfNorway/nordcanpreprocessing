



nordcan_raw_incidence_dataset_tests <- function() {
  test_df <- nordcancore::get_exported_dataset(
    dataset_name = "nordcan_tests",
    package_name = "nordcancore"
  )
  test_df[test_df[["raw_incidence_dataset"]] == TRUE, ]
}


nordcan_incidence_dataset <- function(x, ...) {
  report_df <- report_is_valid_raw_nordcan_incidence_dataset(x)

  result_df <- dbc::identify_invalid_observations(x = x, report_df = report_df)

  if (any(!result_df[["is_valid"]])) {
    warning("x contained invalid observations; returning original dataset ",
            "with invalid observations marked in columns \"is_valid\", ",
            "\"fail_test_set\", and \"fail_message_set\". you need to fix ",
            "the invalid observations and run this function again ",
            "before you can proceed.")
    x <- cbind(x, result_df)
    return(x)
  }

  # otherwise, proceed with creating new columns / transforming existing
  # columns

  stopifnot(
    inherits(processed_x, "data.table")
  )
  return(processed_x)
}



#' @importFrom dbc tests_to_report
report_is_valid_raw_nordcan_incidence_dataset <- function(
  x,
  x_nm = NULL,
  col_nms = names(x)
) {
  x_nm <- dbc::handle_x_nm_arg(x_nm)
  test_set <- nordcanpreprocessing::norcan_raw_incidence_dataset_tests()

  report_df <- dbc::report_has_only_valid_observations(
    x = x,
    x_nm = x_nm,
    fail_message = test_set[["fail_message"]],
    pass_message = test_set[["pass_message"]],
    col_nms = col_nms,
    col_nm_set_list = test_set[["col_nm_set"]]
  )
  cbind(
    test_set_nm = test_set[["test_set_nm"]],
    report_df
  )
}

assert_is_valid_raw_nordcan_incidence_dataset <- function(
  x,
  col_nms = names(x)
) {
  report_df <- report_is_valid_nordcan_incidence_dataset(
    x,
    col_nms
  )
  dbc::assert_user_input_report_passes(report_df)
}

log_is_valid_raw_nordcan_incidence_dataset <- function(
  x,
  col_nms = names(x),
  log_file_path = NULL,
  log_to = c("console", "file", "output")[1L],
  format = c("R", "markdown")[1L]
) {
  # TODO: implement option to use format = "markdown".
  dbc::assert_is_one_of(
    format,
    fun_nms = c("assert_is_NULL", "assert_is_character_nonNA_atom")
  )
  if (is.null(log_file_path)) {
    sys_time_string <- as.character(Sys.time())
    sys_time_string <- gsub("[^0-9]", "_", sys_time_string)
    log_file_path <- tempfile(
      paste0("nordcan_incidence_dataset_log_", sys_time_string),
      fileext = ".txt"
    )
  } else {
    dbc::assert_is_character_nonNA_atom(log_file_path)
  }
  dbc::assert_is_character_nonNA_atom(log_to)
  dbc::assert_atom_is_in_set(log_to, set = c("console", "file", "output"))
  dbc::assert_is_character_nonNA_atom(format)
  dbc::assert_atom_is_in_set(format, set = c("R", "markdown"))

  report_df <- nordcanpreprocessing::report_is_valid_nordcan_incidence_dataset(
    x = x,
    col_nms = col_nms
  )

  sink(file = log_file_path, append = FALSE)
  cat("sessionInfo():\n")
  print(sessionInfo())
  cat("\n")
  cat("Sys.time():\n")
  print(Sys.time())
  cat("\n")
  invisible(lapply(1:nrow(report_df), function(test_no) {
    col_nm_set <- report_df[["test_set_nm"]][test_no]
    cat("\n")
    cat("* QA results for column(s) ", col_nm_set, ":\n")
    if (report_df[["pass"]][test_no]) {
      cat("  - test passed, no invalid observations\n")
    } else {
      cat("  - test failure; message from test: ")
      cat(report_df[["message"]][test_no])
      cat("\n")
      cat("  - first five invalid observation:\n")
      print(x[utils::head(report_df[["wh_fail"]], 5L), ])
      cat("\n")
    }
  }))
  sink(NULL)

  if (log_to == "console") {
    cat(readLines(log_file_path), sep = "\n")
  } else if (log_to == "file") {
    message("* log_is_valid_nordcan_incidence_dataset: log written into file ",
            deparse(log_file_path))
  } else if (log_to == "output") {
    return(readLines(log_file_path))
  } else {
    stop("no action defined for log_to = ", deparse(log_to))
  }

  return(invisible(NULL))
}



