
#' @title Reports, Assertions, and Tests on NORDCAN Datasets and Columns
#' @description
#' Functions to create reports, assert, and test NORDCAN datasets and columns
#' using functionality from package `dbc`.
#' @details
#' `report_` functions produce a `data.frame` describing whether tests passed,
#' and if not, how they failed. See e.g. `[dbc::expressions_to_report]`.
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
      dbc::expressions_to_report(
        expressions = reports_on_column_name_sets[["test_string"]],
        env = dataset_env
      )
    )
  }

  return(report_df)
}


#' @rdname reports_assertions_tests
#' @export
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
  if (!column_name %in% names(report_funs_by_column_name)) {
    stop("Internal error: no report function defined for column_name = ",
         deparse(column_name))
  }
  report_fun <- report_funs_by_column_name[[column_name]]
  report_fun(x = x, column_name = column_name)
}

#' @rdname reports_assertions_tests
#' @export
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
    dbc::expressions_to_report(
      expressions = c(
        paste0("nchar(", column_name, ") <= 50L"),
        "!duplicated(x)"
      ),
      fail_messages = c(
        paste0(
          "Column \"${column_name}\" has values that contain more than ",
          "50 characters (digits); positions of first five invalid values: ",
          "${deparse(utils::head(wh_fail, 5L))}"
        ),
        paste0(
          "${column_name} values were duplicated; first five positions of ",
          "duplicates: ${deparse(utils::head(wh_fail, 5L))}"
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
    column_specification[["max"]] <- Sys.Date()
    report_df <- dbc::expressions_to_report(
      expressions = c(
        paste0("inherits(", column_name, ", \"Date\")"),
        paste0("grepl(\"[0-9]{4}-[0-1][[0-9]-[0-3][0-9]\", ",column_name,")")
      ),
      fail_messages = c(
        "Column \"${column_name}\" is not of class Date; see ?Date",
        paste0(
          "Column \"${column_name}\" has values that contain more than ",
          "50 characters (digits); positions of first five invalid values: ",
          "${deparse(utils::head(wh_fail, 5L))}"
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
        dbc::expressions_to_report(
          expressions = "x[[column_name]] >= min",
          fail_messages = paste0(
            "Column ${column_name} had values less than ${min}; ",
            "positions of first five invalid values: ",
            "${deparse(utils::head(wh_fail, 5L))}"
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
        dbc::expressions_to_report(
          expressions = "x[[column_name]] <= max",
          fail_messages = paste0(
            "Column ${column_name} had values > ${max}; ",
            "positions of first five invalid values: ",
            "${deparse(utils::head(wh_fail, 5L))}"
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
      dbc::expressions_to_report(
        expressions = c(
          "x[[column_name]] >= min",
          "x[[column_name]] <= max"
        ),
        fail_messages = c(
          paste0(
            "${column_name} had values < ${min}; first five positions of ",
            "invalid values: ${deparse(utils::head(wh_fail, 5L))}"
          ),
          paste0(
            "${column_name} had values > ${max}; first five positions of ",
            "invalid values: ${deparse(utils::head(wh_fail, 5L))}"
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
      dbc::expressions_to_report(
        expressions = c(
          "x[[column_name]] >= min",
          "x[[column_name]] <= max"
        ),
        fail_messages = c(
          paste0(
            "${column_name} had values < ${min}; first five positions of ",
            "invalid values: ${deparse(utils::head(wh_fail, 5L))}"
          ),
          paste0(
            "${column_name} had values > ${max}; first five positions of ",
            "invalid values: ${deparse(utils::head(wh_fail, 5L))}"
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
    if (levels[1] == 1800L) {1800L:data.table::year(Sys.Date())}
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
    report_df <- dbc::report_is_character_nonNA_vector(
      x = x[[column_name]],
      x_nm = paste0("x$", column_name)
    )
    max_nchars <- column_specification[["max_nchars"]]
    digit_only <- column_specification[["digit_only"]]
    if (!is.null(max_nchars)) {
      report_df <- rbind(
        report_df,
        dbc::expressions_to_report(
          expressions = c(
            "nchar(x[[column_name]]) <= max_nchars"
          ),
          fail_messages = c(
            paste0(
              "Some of ${column_name} had characters more than ${max_nchars}; first five positions of ",
              "invalid values: ${deparse(utils::head(wh_fail, 5L))}"
            )
          ),
          pass_messages = c(
            "All values of ${column_name} having characters less than ${max_nchars}"
          )
        )
      )
    }

    if (digit_only) {
      report_df <- rbind(
        report_df,
        dbc::expressions_to_report(
          expressions = c(
            "grepl('^[0-9]*$', x[[column_name]])"
          ),
          fail_messages = c(
            paste0(
              "Some of ${column_name} containing non-digit characters; first five positions of ",
              "invalid values: ${deparse(utils::head(wh_fail, 5L))}"
            )
          ),
          pass_messages = c(
            "All values of ${column_name} containing only digit characters!"
          )
        )
      )
    }

    return(report_df)
  },
  "ICD-10" = function(x, column_name) {
    column_specification <- nordcancore::nordcan_metadata_column_specifications(
      column_name
    )
    dataset_env <- as.environment(x)
    parent.env(dataset_env) <- environment()
    report_df <- dbc::expressions_to_report(
      expressions = paste0(
        "is.na(", column_name,") | grepl(\"[A-Z][0-9]+\", ", column_name,")"
      ),
      fail_messages = paste0(
        "Column ${column_name} has invalid values; valid values start with ",
        "one uppercase letter and proceed with digits only, e.g. C0004 ",
        "(and not e.g. c0004, C00.04, etc). positions of first five invalid ",
        "values: ${deparse(utils::head(wh_fail, 5L))}; note that when the ",
        "ICD-10 code is not known, it should be coded as NA."
      ),
      pass_messages = "Column ${column_name} is formatted correctly.",
      env = dataset_env
    )
    return(report_df)
  },
  Other = function(x, column_name) {
    column_specification <- nordcancore::nordcan_metadata_column_specifications(
      column_name
    )
    msg <- column_specification[["message"]]
    format <- "Other"
    report_df <- dbc::expressions_to_report(
      expressions = "format == \"Other\"",
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


#' @rdname reports_assertions_tests
#' @export
report_national_population_life_table_is_valid <- function(
  x
) {
  dataset_env <- as.environment(x)
  parent.env(dataset_env) <- environment()

  gs <- nordcancore::get_global_nordcan_settings()
  first_year_survival <- gs[["first_year_survival"]]
  year_nordcan <- gs[["last_year_survival"]]

  report_df <- dbc::expressions_to_report(
    expressions = c("first_year_survival:year_nordcan %in% unique(year) ",
                    "0:90 %in% age",
                    "prob >= 0 & prob <= 1"
    ),
    fail_messages = c(
      paste0(sprintf("Column 'year' should have every year between the first survival year (%s) and the current NORDCAN year (%s).",
                     first_year_survival, year_nordcan), " First five positions of invalid values: ${deparse(utils::head(wh_fail, 5L))}"),
      "Column 'age' should have at least the values 0-90.",
      paste0("The values of column 'prob' must between 0 and 1",
             " First five positions of invalid values: ${deparse(utils::head(wh_fail, 5L))}")
    ),
    pass_messages = c(
      "Column 'year' has every year between the first survival year and the current NORDCAN year!",
      "Column 'age' has at least the values 0-90.",
      "The values of 'prob' are between 0 and 1."
    ),
    env = dataset_env,
    call = match.call()
  )
  return(report_df)
}


#' @rdname reports_assertions_tests
#' @export
assert_national_population_life_table_is_valid <- function(
  x
) {
  report_df <- report_dataset_is_valid(
    x = x, dataset_name = "national_population_life_table"
  )
  dbc::report_to_assertion(report_df)

  report_df <- report_national_population_life_table_is_valid(
    x = x
  )
  dbc::report_to_assertion(report_df)
}


#' @rdname reports_assertions_tests
#' @export
report_unprocessed_cancer_death_count_dataset <- function(x) {
  dataset_env <- as.environment(x)
  parent.env(dataset_env) <- environment()
  report_df <- dbc::expressions_to_report(
    expressions = c(
      "icd_version %in% 6:10",
      "nchar(icd_code) %in% 3:4",
      "(icd_version == 10 & grepl('^[a-zA-Z]', icd_code)) | icd_version != 10",
      "(icd_version == 10) | (icd_version != 10 & !grepl('^[a-zA-Z]', icd_code))",
      "!duplicated(x, by = c('year', 'sex', 'region', 'agegroup', 'icd_code', 'icd_version'))"
    ),
    fail_messages = c(
      paste0("Only icd_version values 6-10 are allowed; you had also these ",
             "values (first five): ${deparse(utils::head(unique(icd_version[wh_fail]), 5L))}"),
      paste0("The length of some values of 'icd_code' are not 3 or 4. ",
             "first five positions of invalid values: ${deparse(utils::head(wh_fail, 5L))}"),
      paste0("Some ICD-10 codes do not start with a letter; ",
             "first five positions of invalid values: ${deparse(utils::head(wh_fail, 5L))}"),
      paste0("Some ICD-6/7/8/9 codes start with a letter; ",
             "first five positions of invalid values: ${deparse(utils::head(wh_fail, 5L))}"),
      paste0("Dataset has duplicate records",
             "first five positions of invalid values: ${deparse(utils::head(wh_fail, 5L))}")
    ),
    pass_messages = c(
      "All values of 'icd_version' are valid.",
      "All values of 'icd_code' having 3 or 4 characters!",
      "All ICD-10 codes start with a letter",
      "No ICD-6/7/8/9 codes start with a letter",
      "Dataset has no duplicate record"
    ),
    env = dataset_env,
    call = match.call()
  )

  return(report_df)
}





#' @rdname reports_assertions_tests
#' @export
assert_unprocessed_cancer_death_count_dataset <- function(
  x
) {
  report_df <- report_dataset_is_valid(
    x = x, dataset_name = "unprocessed_cancer_death_count_dataset"
  )
  dbc::report_to_assertion(report_df)

  report_df <- report_unprocessed_cancer_death_count_dataset(
    x = x
  )
  dbc::report_to_assertion(report_df)

}




