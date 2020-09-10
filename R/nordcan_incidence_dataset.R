
#' @importFrom utils sessionInfo
report_to_log <- function(
  x,
  report_df,
  log_file_path = NULL,
  log_to = c("console", "file", "output")[1L],
  format = c("R", "markdown")[1L]
) {
  # TODO: implement option to use format = "markdown";
  dbc::assert_is_data.frame(x)
  dbc::assert_is_data.frame_with_required_names(
    report_df, required_names = c("test", "pass", "message")
  )
  dbc::assert_is_character_nonNA_atom(format)
  if (is.null(log_file_path)) {
    sys_time_string <- as.character(Sys.time())
    sys_time_string <- gsub("[^0-9]", "_", sys_time_string)
    log_file_path <- tempfile(
      paste0("report_log_", sys_time_string),
      fileext = ".txt"
    )
  } else {
    dbc::assert_is_character_nonNA_atom(log_file_path)
  }
  dbc::assert_is_character_nonNA_atom(log_to)
  dbc::assert_atom_is_in_set(log_to, set = c("console", "file", "output"))
  dbc::assert_is_character_nonNA_atom(format)
  dbc::assert_atom_is_in_set(format, set = c("R", "markdown"))

  sink(file = log_file_path, append = FALSE)
  cat("utils::sessionInfo():\n")
  print(utils::sessionInfo())
  cat("\n")
  cat("Sys.time():\n")
  print(Sys.time())
  cat("\n")
  invisible(lapply(1:nrow(report_df), function(test_no) {
    test <- report_df[["test"]][test_no]
    cat("\n")
    cat("* QA results for test ", test, ":\n")
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
    message(paste0(readLines(log_file_path), collapse = "\n"))
  } else if (log_to == "file") {
    message("* report_to_log: log written into file ",
            deparse(log_file_path))
  } else if (log_to == "output") {
    return(readLines(log_file_path))
  } else {
    stop("no action defined for log_to = ", deparse(log_to))
  }

  return(invisible(NULL))
}



