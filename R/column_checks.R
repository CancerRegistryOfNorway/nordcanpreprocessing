
#' @title Reports, Assertions, and Tests on NORDCAN Datasets and Columns
#' @description
#' Functions to create reports, assert, and test NORDCAN datasets and columns
#' using functionality from package `dbc`.
#' @details
#' `report_` functions produce a `data.frame` describing whether tests passed,
#' and if not, how they failed. See e.g. `[dbc::tests_to_report]`.
#'
#' `assert_` functions raise an error if any test in the corresponding report
#' did not pass. See e.g. `[dbc::report_to_assertion]`.
#'
#' `test_` functions return `TRUE` if all tests in the corresponding report
#' passed.
#' @name reports_assertions_tests


#' @rdname reports_assertions_tests
#' @param x `[data.frame]` (mandatory, no default)
#'
#' dataset to report / assert / test on
#' @param dataset_name `[character]` (mandatory, no default)
#'
#' name of a NORDCAN dataset; must be one of the elements of output of
#' `nordcancore::nordcan_dataset_names()`; therefore see also
#' `[nordcancore::nordcan_dataset_names]`
#' @export
#' @importFrom dbc assert_is_character_nonNA_atom assert_atom_is_in_set
report_dataset_is_valid <- function(
  x,
  dataset_name
) {
  dbc::assert_is_character_nonNA_atom(dataset_name)
  dbc::assert_atom_is_in_set(
    x = dataset_name, set = nordcancore::nordcan_dataset_names()
  )
  report_dataset_columns_are_valid(
    x = x,
    nordcancore::nordcan_column_name_set(
      paste0("column_name_set_", dataset_name)
    )
  )
}

#' @rdname reports_assertions_tests
#' @export
#' @importFrom dbc report_to_assertion
assert_dataset_is_valid <- function(
  x,
  dataset_name
) {
  report_df <- report_dataset_is_valid(
    x = x, dataset_name = dataset_name
  )
  dbc::report_to_assertion(report_df)
}


#' @rdname reports_assertions_tests
#' @export
test_dataset_is_valid <- function(
  x,
  dataset_name
) {
  report_df <- report_dataset_is_valid(
    x = x, dataset_name = dataset_name
  )
  all(report_df[["pass"]])
}

#' @rdname reports_assertions_tests
#' @param column_names `[character]` (mandatory, no default)
#'
#' names of columns to check
#' @export
#' @importFrom data.table rbindlist
#' @importFrom dbc assert_is_character_nonNA_vector
#' report_is_data.frame_with_required_names tests_to_report
#' @importFrom nordcancore get_internal_dataset
report_dataset_columns_are_valid <- function(
  x,
  column_names
) {
  dbc::assert_is_character_nonNA_vector(column_names)
  report_df <- dbc::report_is_data.frame_with_required_names(
    x = x,
    required_names = column_names
  )

  # reports on individual columns
  report_df <- rbind(
    report_df,
    data.table::rbindlist(lapply(column_names, function(col_nm) {
      report_column_is_valid(x = x, column_name = col_nm)
    }))
  )

  # reports on sets of columns tested together
  reports_on_column_name_sets <- nordcancore::get_internal_dataset(
    dataset_name = "reports_on_column_name_sets",
    package_name = "nordcanpreprocessing"
  )
  column_name_set_in_column_names <- vapply(
    reports_on_column_name_sets[["col_nm_set"]],
    function(col_nm_vec) all(col_nm_vec %in% column_names),
    logical(1L)
  )
  reports_on_column_name_sets <- reports_on_column_name_sets[
    i = column_name_set_in_column_names
  ]
  if (nrow(reports_on_column_name_sets) > 0L) {
    dataset_env <- as.environment(x)
    parent.env(dataset_env) <- parent.frame(1L)
    report_df <- rbind(
      report_df,
      dbc::tests_to_report(
        tests = reports_on_column_name_sets[["test_string"]],
        env = dataset_env
      )
    )
  }

  return(report_df)
}


#' @rdname reports_assertions_tests
#' @export
#' @importFrom dbc report_to_assertion
assert_dataset_columns_are_valid <- function(
  x,
  column_names
) {
  report_df <- report_dataset_columns_are_valid(
    x = x,
    column_names = column_names
  )
  dbc::report_to_assertion(report_df)
}

#' @rdname reports_assertions_tests
#' @export
test_dataset_columns_are_valid <- function(
  x,
  column_names
) {
  report_df <- report_dataset_columns_are_valid(
    x = x,
    column_names = column_names
  )
  all(report_df[["pass"]])
}

#' @rdname reports_assertions_tests
#' @param column_name `[character]` (mandatory, no default)
#'
#' name of column to check
#' @export
report_column_is_valid <- function(x, column_name) {
  dbc::assert_is_character_nonNA_atom(column_name)
  dbc::assert_atom_is_in_set(
    column_name,
    set = names(report_funs_by_column_name)
  )
  report_fun <- report_funs_by_column_name[[column_name]]
  message(column_name)
  report_fun(x = x, column_name = column_name)
}

#' @rdname reports_assertions_tests
#' @export
#' @importFrom dbc report_to_assertion
assert_column_is_valid <- function(x, column_name) {
  report_df <- report_column_is_valid(x = x, column_name = column_name)
  dbc::report_to_assertion(report_df)
}

#' @rdname reports_assertions_tests
#' @export
test_column_is_valid <- function(x, column_name) {
  report_df <- report_column_is_valid(x = x, column_name = column_name)
  all(report_dt[["pass"]])
}

report_funs_by_column_format <- list(
  ID = function(x, column_name) {
    dataset_env <- as.environment(x)
    parent.env(dataset_env) <- parent.frame(1L)
    dbc::tests_to_report(
      tests = c(
        paste0("nchar(", column_name, ") <= 50L")
      ),
      fail_messages = c(
        paste0(
          "Column \"${column_name}\" has values that contain more than ",
          "50 characters (digits); positions of first five invalid values: ",
          "${utils::head(wh_fail, 5L)}"
        )
      ),
      pass_messages = "Column \"${column_name}\" passed checks.",
      env = dataset_env
    )
  },
  Date = function(x, column_name) {
    dataset_env <- as.environment(x)
    parent.env(dataset_env) <- parent.frame(1L)
    dbc::tests_to_report(
      tests = c(
        paste0("inherits(", column_name, ", \"Date\")"),
        paste0("grepl(\"[0-9]{4}-[0-1][[0-9]-[0-3][0-9]\", ",column_name,")")
      ),
      fail_messages = c(
        "Column \"${column_name}\" is not of class Date; see ?Date",
        paste0(
          "Column \"${column_name}\" has values that contain more than ",
          "50 characters (digits); positions of first five invalid values: ",
          "${utils::head(wh_fail, 5L)}"
        )
      ),
      pass_messages = c(
        "Column \"${column_name}\" has correct class.",
        "Column \"${column_name}\" passed checks."
      ),
      env = dataset_env
    )
  }
)


assert_funs_by_column_format <- lapply(
  report_funs_by_column_format,
  function(report_fun) {
    assert_fun <- function(x, column_name) {
      report_df <- report_fun(x = x, column_name = column_name)
      dbc::report_to_assertion(report_df)
    }
    return(assert_fun)
  }
)


test_funs_by_column_format <- lapply(
  report_funs_by_column_format,
  function(report_fun) {
    test_fun <- function(x, column_name) {
      report_df <- report_fun(x = x, column_name = column_name)
      all(report_df[["pass"]])
    }
    return(test_fun)
  }
)



report_funs_by_column_name <- lapply(
  nordcancore::nordcan_column_name_set("column_name_set_all"),
  function(column_name) {
    report_fun <- function(x, column_name) {
      specs <- nordcancore::nordcan_column_specifications(column_name)
      format <- specs[["format"]]
      report_funs_by_column_format[[format]](x = x, column_name = column_name)
    }
    formals(report_fun)[["column_name"]] <- column_name
    return(report_fun)
  }
)
names(report_funs_by_column_name) <-
  nordcancore::nordcan_column_name_set("column_name_set_all")


assert_funs_by_column_name <- lapply(
  nordcancore::nordcan_column_name_set("column_name_set_all"),
  function(column_name) {
    assert_fun <- function(x, column_name) {
      specs <- nordcancore::nordcan_column_specifications(column_name)
      format <- specs[["format"]]
      assert_funs_by_column_format[[format]](x = x, column_name = column_name)
    }
    formals(assert_fun)[["column_name"]] <- column_name
    return(assert_fun)
  }
)
names(report_funs_by_column_name) <- names(report_funs_by_column_name)



test_funs_by_column_name <- lapply(
  nordcancore::nordcan_column_name_set("column_name_set_all"),
  function(column_name) {
    test_fun <- function(x, column_name) {
      specs <- nordcancore::nordcan_column_specifications(column_name)
      format <- specs[["format"]]
      test_funs_by_column_format[[format]](x = x, column_name = column_name)
    }
    formals(test_fun)[["column_name"]] <- column_name
    return(test_fun)
  }
)
names(test_funs_by_column_name) <- names(report_funs_by_column_name)



incidence_columns_check_Date        <- function(data_input, column_name) {
  if(all(grepl("[0-9]{4}-[0-1][[0-9]-[0-3][0-9]", data_input[,column_name]))) {
    cat("Date format of variable '%s' is correct!\n", column_name)

    if (!is.null(nordcancore::nordcan_column_specifications(column_name)$min)) {
      if (all(as.Date(data_input[,column_name]) >= as.Date(nordcancore::nordcan_column_specifications(column_name)$min))) {
        cat("Earliest value of variable %s passed checking!\n", column_name)
      } else {
        cat("Earliest value of variable %s not passed checking! \n", column_name)
      }
    }

    if (!is.null(nordcancore::nordcan_column_specifications(column_name)$max)) {
      if (all(as.Date(data_input[,column_name]) >= as.Date(nordcancore::nordcan_column_specifications(column_name)$max))) {
        cat("Earliest value of variable %s passed checking!\n", column_name)
      } else {
        cat("Earliest value of variable %s not passed checking!\n", column_name)
      }
    }

  } else {

    id <- which(!grepl("[0-9]{4}-[0-1][[0-9]-[0-3][0-9]", data_input[,column_name]))
    cat(sprintf("The date format of variable '%s' is not correct, all valide values should be in formation: 'yyyy-mm-dd' \n", column_name))
  }
}

incidence_columns_check_Categorical <- function(data_input, column_name) {
  if (all(data_input[,column_name] %in%  nordcancore::nordcan_column_specifications(column_name)$levels)) {
    cat (sprintf( "All values of variable %s are valid\n", column_name))
  } else {
    cat (sprintf( "Variable %s contains invalid values\n", column_name))

  }
}

incidence_columns_check_Numeric     <- function(data_input, column_name) {
  if (all(!is.na(as.numeric(data_input[,column_name])))) {
    cat (sprintf("All values of variable %s are numeric\n", column_name))

    if (!is.null(nordcancore::nordcan_column_specifications(column_name)$min)) {
      if (all(data_input[,column_name] >= nordcancore::nordcan_column_specifications(column_name)$min)) {
        cat("Minimum value of variable %s passed checking\n", column_name)
      } else {
        cat("Minimum value of variable %s not passed checking\n", column_name)
      }
    }


    if (!is.null(nordcancore::nordcan_column_specifications(column_name)$max)) {
      if (all(data_input[,column_name] <= nordcancore::nordcan_column_specifications(column_name)$max)) {
        cat("Maximun value of variable %s passed checking\n", column_name)
      } else {
        cat("Maximun value of variable %s not passed checking\n", column_name)
      }
    }



  } else {
    cat (sprintf("Variable %s contains values which are not numeric\n", column_name))
  }


}




incidence_columns_check_region      <- function(data_input, column_name) {
  region <- nordcancore::nordcan_column_specifications(column_name)$table

  if (all(data_input[, column_name] %in% nordcancore::nordcan_column_specifications(column_name)$table$Value)) {
    cat("All region codes are valid! \n")

    if(length(unique(substr(data_input[, column_name] %in% region$Value, 1,1))) != 1) {
      cat("Some region codes may come from different countries/regions!\n")
    }
  } else {
    cat("Region codes contain unvalid value(s). \n")
  }
}


incidence_columns_check_Other       <- function(data_input, column_name) {
  cat(nordcancore::nordcan_column_specifications(column_name)$message)
}

