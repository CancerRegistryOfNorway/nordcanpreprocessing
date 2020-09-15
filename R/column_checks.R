
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
#' `nordcancore::nordcan_metadata_dataset_names()`; therefore see also
#' `[nordcancore::nordcan_metadata_dataset_names]`
#' @export
#' @importFrom dbc assert_is_character_nonNA_atom assert_atom_is_in_set
report_dataset_is_valid <- function(
  x,
  dataset_name
) {
  dbc::assert_is_character_nonNA_atom(dataset_name)
  dbc::assert_atom_is_in_set(
    x = dataset_name, set = nordcancore::nordcan_metadata_dataset_names()
  )
  report_dataset_columns_are_valid(
    x = x,
    nordcancore::nordcan_metadata_column_name_set(
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
#' @importFrom dbc report_to_assertion
assert_processed_cancer_record_dataset_is_valid <- function(
  x
) {
  report_df <- report_dataset_is_valid(
    x = x, dataset_name = "processed_cancer_record_dataset"
  )
  dbc::report_to_assertion(report_df)
}
#' @rdname reports_assertions_tests
#' @export
#' @importFrom dbc report_to_assertion
assert_unprocessed_cancer_record_dataset_is_valid <- function(
  x
) {
  report_df <- report_dataset_is_valid(
    x = x, dataset_name = "unprocessed_cancer_record_dataset"
  )
  dbc::report_to_assertion(report_df)
}
#' @rdname reports_assertions_tests
#' @export
#' @importFrom dbc report_to_assertion
assert_general_population_size_dataset_is_valid <- function(
  x
) {
  report_df <- report_dataset_is_valid(
    x = x, dataset_name = "general_population_size_dataset"
  )
  dbc::report_to_assertion(report_df)
}
#' @rdname reports_assertions_tests
#' @export
#' @importFrom dbc report_to_assertion
assert_general_population_death_count_dataset_is_valid <- function(
  x
) {
  report_df <- report_dataset_is_valid(
    x = x, dataset_name = "general_population_death_count_dataset"
  )
  dbc::report_to_assertion(report_df)
}
#' @rdname reports_assertions_tests
#' @export
#' @importFrom dbc report_to_assertion
assert_national_population_life_table_is_valid <- function(
  x
) {
  report_df <- report_dataset_is_valid(
    x = x, dataset_name = "national_population_life_table"
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
  if (!column_name %in% names(report_funs_by_column_name)) {
    stop("Internal error: no report function defined for column_name = ",
         deparse(column_name))
  }
  report_fun <- report_funs_by_column_name[[column_name]]
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
  all(report_df[["pass"]])
}

report_funs_by_column_format <- list(
  ID = function(x, column_name) {
    column_specification <- nordcancore::nordcan_metadata_column_specifications(
      column_name
    )
    dataset_env <- as.environment(x)
    parent.env(dataset_env) <- environment()
    dbc::tests_to_report(
      tests = c(
        paste0("nchar(", column_name, ") <= 50L"),
        "!duplicated(x)"
      ),
      fail_messages = c(
        paste0(
          "Column \"${column_name}\" has values that contain more than ",
          "50 characters (digits); positions of first five invalid values: ",
          "${utils::head(wh_fail, 5L)}"
        ),
        paste0(
          "${column_name} values were duplicated; first five positions of ",
          "duplicates: ${utils::head(wh_fail, 5L)}"
        )
      ),
      pass_messages = c(
        "Column \"${column_name}\" passed width check.",
        "Column \"${column_name}\" had no duplicates."
      ),
      env = dataset_env
    )
  },
  Date = function(x, column_name) {
    dataset_env <- as.environment(x)
    parent.env(dataset_env) <- environment()
    column_specification <- nordcancore::nordcan_metadata_column_specifications(
      column_name
    )
    report_df <- dbc::tests_to_report(
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
        "Column \"${column_name}\" is formatted correctly."
      ),
      env = dataset_env
    )

    min <- column_specification[["min"]]
    if (!is.null(min)) {
      report_df <- rbind(
        report_df,
        dbc::tests_to_report(
          tests = "x[[column_name]] >= min",
          fail_messages = paste0(
            "Column ${column_name} had values less than ${min}; ",
            "positions of first five invalid values: ",
            "${utils::head(wh_fail, 5L)}"
          ),
          pass_messages = paste0(
            "All column ${column_name} values were >= ${min}"
          ),
          env = dataset_env
        )
      )
    }
    max <- column_specification[["max"]]
    if (!is.null(max)) {
      report_df <- rbind(
        report_df,
        dbc::tests_to_report(
          tests = "x[[column_name]] <= max",
          fail_messages = paste0(
            "Column ${column_name} had values > ${max}; ",
            "positions of first five invalid values: ",
            "${utils::head(wh_fail, 5L)}"
          ),
          pass_messages = paste0(
            "All column ${column_name} values were <= ${max}"
          ),
          env = dataset_env
        )
      )
    }

    return(report_df)
  },
  Numeric = function(x, column_name) {
    column_specification <- nordcancore::nordcan_metadata_column_specifications(
      column_name
    )
    report_df <- dbc::report_is_number_nonNA_vector(
      x = x[[column_name]],
      x_nm = paste0("x$", column_name)
    )
    min <- column_specification[["min"]]
    max <- column_specification[["max"]]
    report_df <- rbind(
      report_df,
      dbc::tests_to_report(
        tests = c(
          "x[[column_name]] >= min",
          "x[[column_name]] <= max"
        ),
        fail_messages = c(
          paste0(
            "${column_name} had values < ${min}; first five positions of ",
            "invalid values: ${utils::head(wh_fail, 5L)}"
          ),
          paste0(
            "${column_name} had values > ${max}; first five positions of ",
            "invalid values: ${utils::head(wh_fail, 5L)}"
          )
        ),
        pass_messages = c(
          "All ${column_name} values were >= ${min}",
          "All ${column_name} values were <= ${max}"
        )
      )
    )
    return(report_df[])
  },
  Integer = function(x, column_name) {
    column_specification <- nordcancore::nordcan_metadata_column_specifications(
      column_name
    )
    report_df <- dbc::report_is_number_nonNA_vector(
      x = x[[column_name]],
      x_nm = paste0("x$", column_name)
    )
    min <- column_specification[["min"]]
    max <- column_specification[["max"]]
    report_df <- rbind(
      report_df,
      dbc::tests_to_report(
        tests = c(
          "x[[column_name]] >= min",
          "x[[column_name]] <= max"
        ),
        fail_messages = c(
          paste0(
            "${column_name} had values < ${min}; first five positions of ",
            "invalid values: ${utils::head(wh_fail, 5L)}"
          ),
          paste0(
            "${column_name} had values > ${max}; first five positions of ",
            "invalid values: ${utils::head(wh_fail, 5L)}"
          )
        ),
        pass_messages = c(
          "All ${column_name} values were >= ${min}",
          "All ${column_name} values were <= ${max}"
        )
      )
    )
    return(report_df[])
  },
  Categorical = function(x, column_name) {
    column_specification <- nordcancore::nordcan_metadata_column_specifications(
      column_name
    )
    levels <- column_specification[["levels"]]
    dbc::report_vector_elems_are_in_set(
      x = x[[column_name]],
      x_nm = column_name,
      set = levels
    )
  },
  String = function(x, column_name) {
    column_specification <- nordcancore::nordcan_metadata_column_specifications(
      column_name
    )
    dataset_env <- as.environment(x)
    parent.env(dataset_env) <- environment()
    levels <- column_specification[["levels"]]
    dbc::tests_to_report(
      tests = paste0("nchar(",column_name,") <= 50L"),
      fail_messages = paste0(
        "some elements of ${column_name} had more than 50 characters; first ",
        "five overlong elements: ${utils::head(wh_fail, 5L)}"
      ),
      pass_messages = paste0(
        "All elements of column ${column_name} had at most 50 characters."
      ),
      env = dataset_env
    )
  },
  "ICD-10" = function(x, column_name) {
    column_specification <- nordcancore::nordcan_metadata_column_specifications(
      column_name
    )
    report_df <- dbc::tests_to_report(
      tests = paste0(
        "grepl(\"[A-Z][0-9]+\", ", column_name,")"
      ),
      fail_messages = paste0(
        "Column ${column_name} has invalid values; valid values start with ",
        "one uppercase letter and proceed with digits only, e.g. C0004 ",
        "(and not e.g. c0004, C00.04, etc). positions of first five invalid ",
        "values: ${utils::head(wh_fail, 5L)}"
      ),
      pass_messages = "Column ${column_name} is formatted correctly."
    )
    return(report_df)
  },
  Other = function(x, column_name) {
    column_specification <- nordcancore::nordcan_metadata_column_specifications(
      column_name
    )
    msg <- column_specification[["message"]]
    format <- "Other"
    report_df <- dbc::tests_to_report(
      tests = "format == \"Other\"",
      fail_messages = "internal error: expected format to be \"Other\"",
      pass_messages = "No checks defined for column ${column_name}"
    )
    return(report_df)
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
  nordcancore::nordcan_metadata_column_name_set("column_name_set_all"),
  function(column_name) {
    report_fun <- function(x, column_name) {
      specs <- nordcancore::nordcan_metadata_column_specifications(column_name)
      format <- specs[["format"]]
      if (!format %in% names(report_funs_by_column_format)) {
        stop("Internal error: no report function defined for format = ",
             deparse(format))
      }
      report_funs_by_column_format[[format]](x = x, column_name = column_name)
    }
    formals(report_fun)[["column_name"]] <- column_name
    return(report_fun)
  }
)
names(report_funs_by_column_name) <-
  nordcancore::nordcan_metadata_column_name_set("column_name_set_all")


assert_funs_by_column_name <- lapply(
  nordcancore::nordcan_metadata_column_name_set("column_name_set_all"),
  function(column_name) {
    assert_fun <- function(x, column_name) {
      specs <- nordcancore::nordcan_metadata_column_specifications(column_name)
      format <- specs[["format"]]
      assert_funs_by_column_format[[format]](x = x, column_name = column_name)
    }
    formals(assert_fun)[["column_name"]] <- column_name
    return(assert_fun)
  }
)
names(report_funs_by_column_name) <- names(report_funs_by_column_name)



test_funs_by_column_name <- lapply(
  nordcancore::nordcan_metadata_column_name_set("column_name_set_all"),
  function(column_name) {
    test_fun <- function(x, column_name) {
      specs <- nordcancore::nordcan_metadata_column_specifications(column_name)
      format <- specs[["format"]]
      test_funs_by_column_format[[format]](x = x, column_name = column_name)
    }
    formals(test_fun)[["column_name"]] <- column_name
    return(test_fun)
  }
)
names(test_funs_by_column_name) <- names(report_funs_by_column_name)



