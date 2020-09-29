




#' @title NORDCAN Cancer Case Dataset
#' @description
#' Add new necessary columns to the cancer case dataset for use in computing
#' statistics.
#' @param x `[data.frame]` (mandatory, no default)
#' dataset of cancer cases
#' @param iarccrgtools_exe_path `[character]` (mandatory, no default)
#' passed to `[iarccrgtools::set_tools_exe_path]`
#' @param iarccrgtools_work_dir `[character]` (mandatory, no default)
#' passed to `[iarccrgtools::set_tools_work_dir]`
#' @section Format of input dataset:
#' See https://github.com/CancerRegistryOfNorway/NORDCAN/wiki/Call-for-data---Incidence
#' @section New columns:
#' See https://github.com/CancerRegistryOfNorway/NORDCAN/wiki/Module-PreprocessEnrich
#'
#' @export

###enrichment" part of our preprocessing.
###function for creating columns based on information in the other columns.
###for the moment for the columns other than the entity ones and the ones "Created by R-program from checktool-info"

#
# ###data
# data=read.csv("Cancer_case_dataset.csv")
# data$date_of_birth=as.Date(data$date_of_birth,format="%d.%m.%Y")
# data$date_of_incidence=as.Date(data$date_of_incidence,format="%d.%m.%Y")
# data$end_of_followup=as.Date(data$end_of_followup,format="%d.%m.%Y")
# data=data[,-which(names(data) %in% c("icd10","mob","yob","moi","mof","yof","surv_time","period","excl_surv_age","excl_surv_dco",
# "excl_surv_autopsy","excl_surv_negativefou","excl_surv_zerofou","excl_imp_error"))]


#' @importFrom data.table := .SD
enrich_nordcan_cancer_record_dataset <- function(
  x,
  iarccrgtools_exe_path,
  iarccrgtools_work_dir
  ) {
  dbc::assert_is_data.frame_with_required_names(
    x,
    required_names = nordcancore::nordcan_metadata_column_name_set(
      "column_name_set_unprocessed_cancer_record_dataset"
    )
  )

  #takes a copy so the original object is not affected
  x <- data.table::setDT(data.table::copy(x))

  #definitions
  x[, "mob" := data.table::month(x$date_of_birth)]
  x[, "yob" := data.table::year(x$date_of_birth)]
  x[, "moi" := data.table::month(x$date_of_incidence)]
  x[, "yoi" := data.table::year(x$date_of_incidence)]
  x[, "mof" := data.table::month(x$end_of_followup)]
  x[, "yof" := data.table::year(x$end_of_followup)]
  x[, "surv_time" := as.numeric(x$end_of_followup - x$date_of_incidence)]
  x[x$autopsy == 1, "surv_time" := 0.0]

  age_breaks <- c(seq(0.0, 90.0, 5.0), Inf)
  x[, "agegroup" := cut(x$age, age_breaks, right = FALSE, labels = FALSE)]
  x[x$agegroup == 19L, "agegroup" := 21L]
  x[, "agr_all_ages" := 1L]
  agr_bone_breaks <- c(0, 30, 40, 50, 70, 90, Inf)
  x[, "agr_bone" := cut(x$age, agr_bone_breaks, right = FALSE, labels = FALSE)]
  agr_all_sites_breaks <- c(0, 30, 50, 70, 80, 90, Inf)
  x[
    j = "agr_all_sites" := cut(
      x$age, agr_all_sites_breaks, right = FALSE, labels = FALSE
    )
  ]

  period_levels <- nordcancore::nordcan_metadata_column_level_space_list(
    "period"
  )[["period"]]
  year_breaks <- as.integer(c(period_levels, max(period_levels) + 5L))
  x[, "period" := cut(x$yoi, year_breaks, right = FALSE, labels = FALSE)]
  x[, "period" := period_levels[x$period]]
  x[, "excl_surv_age" := ifelse (x$age<90,0L,1L)]
  x[, "excl_surv_dco" := ifelse (x$bod==0,1L,0L)]
  x[, "excl_surv_autopsy" := ifelse (x$autopsy==1,1L,0L)]
  x[, "excl_surv_negativefou" := ifelse (x$surv_time<0,1L,0L)]
  x[, "excl_surv_zerofou" := ifelse (x$surv_time==0,1L,0L)]

  excl_surv_col_nms <- names(x)[grepl("excl_surv_", names(x))]
  x[
    j = "excl_surv_total" := as.integer(rowSums(.SD) > 0L),
    .SDcols = excl_surv_col_nms
  ]

  icd10_dt <- nordcanpreprocessing::iarccrgtools_tool(
    x = x,
    tool_name = "icdo3_to_icd10",
    iarccrgtools_exe_path = iarccrgtools_exe_path,
    iarccrgtools_work_dir = iarccrgtools_work_dir
  )
  i.icdo3_to_icd10_output.txt <- NULL # this only to appease R CMD CHECK
  x[
    i = icd10_dt,
    on = "tum",
    j = "icd10" := i.icdo3_to_icd10_output.txt,
  ]
  i.icdo3_to_icd10_input.eO3to10 <- NULL # this only to appease R CMD CHECK
  x[
    i = icd10_dt,
    on = "tum",
    j = "excl_imp_error" := i.icdo3_to_icd10_input.eO3to10,
  ]
  x[, "excl_imp_icd10conversion" := ifelse (is.na(x$excl_imp_error),0L,1L)]

  mp <- nordcanpreprocessing::iarccrgtools_tool(
    x = x,
    tool_name = "multiple_primary",
    iarccrgtools_exe_path = iarccrgtools_exe_path,
    iarccrgtools_work_dir = iarccrgtools_work_dir
  )
  i.multiple_primary_input.mul <- NULL # this only to appease R CMD CHECK
  x[
    i = mp,
    on = "tum",
    j = "excl_imp_duplicate" := i.multiple_primary_input.mul,
  ]
  x[, "excl_imp_duplicate" := ifelse(grepl("\\*",x$excl_imp_duplicate),1L,0L)]

  # it may be that multiple_primary_input.exl is not created if IARC CRG Tools
  # found nothing to exclude.
  if (!"in_multiple_primary_input.exl" %in% names(mp)) {
    x[, "excl_imp_benign" := rep(0L, nrow(x))]
  } else {
    i.in_multiple_primary_input.exl <- NULL # this only to appease R CMD CHECK
    x[
      i = mp,
      on = "tum",
      j = "excl_imp_benign" := i.in_multiple_primary_input.exl,
    ]
    x[, "excl_imp_benign" := ifelse(x$excl_imp_benign,1L,0L)]
  }

  i.in_multiple_primary_output.txt <- NULL # this only to appease R CMD CHECK
  x[
    i = mp,
    on = "tum",
    j = "excluded_multiple" := i.in_multiple_primary_output.txt,
  ]
  x[, "excluded_multiple" := ifelse(x$excluded_multiple==FALSE,1L,0L)]

  icd10_to_entity_dt <- nordcancore::nordcan_metadata_icd10_to_entity()
  entity_col_nms <- nordcancore::nordcan_metadata_column_name_set("column_name_set_entity")
  x <- merge(x, icd10_to_entity_dt, by = "icd10")

  x[, "excl_imp_entitymissing" := ifelse (is.na(x$entity_level_30),1L,0L)]

  excl_imp_col_nms <- names(x)[grepl("^excl_imp_", names(x))]
  excl_imp_col_nms <- setdiff(excl_imp_col_nms, "excl_imp_error")
  x[
    j = "excl_imp_total" := as.integer(rowSums(.SD) > 0L),
    .SDcols = excl_imp_col_nms
  ]

  return(x[])
}

#' @title IARC CRG Tools
#' @description
#' Pass NORDCAN cancer record dataset to IARC CRG Tools from R.

#' @export
#' @param x `[data.table]` (mandatory, no default)
#'
#' the NORDCAN cancer record dataset
#' @param tool_name `[character]` (mandatory, no default)
#'
#' name of tool in IARC CRG Tools to use; one of the output of
#' `[nordcancore::nordcan_iarccrgtools_tool_names()]`
#' @name iarccrgtools


#' @rdname iarccrgtools
#' @details
#' - `iarccrgtools_dataset` collects a dataset from `x` that corresponds to the
#'   requirements of the appropriate tool given in `tool_name`;
#'   see `[iarccrgtools::create_example]` for the formats of each column, and
#'   use `[nordcancore::nordcan_metadata_column_name_set]` to see which columns are used
#' @export
iarccrgtools_dataset <- function(
  x,
  tool_name
) {
  dbc::assert_is_character_nonNA_atom(tool_name)
  tool_name_set <- nordcancore::nordcan_iarccrgtools_tool_names()
  dbc::assert_atom_is_in_set(
    tool_name,
    set = tool_name_set
  )
  template <- iarccrgtools::create_example(paste0("mandatory_", tool_name),
                                            n.rows = 10L)
  nc_col_nms <- nordcancore::nordcan_metadata_column_name_set(
    paste0("column_name_set_iarccrgtools_mandatory_", tool_name)
  )
  dbc::assert_is_data.frame_with_required_names(
    x = x, required_names = unname(nc_col_nms)
  )
  iarc_col_nms <- names(nc_col_nms)
  if (!identical(sort(iarc_col_nms), sort(names(template)))) {
    stop("internal error: mis-specified IARC CRG Tools column names; ",
         "iarc_col_nms = ", deparse(sort(iarc_col_nms)), "; ",
         "sort(names(template)) = ", deparse(sort(names(template))))
  }
  nc_col_nms <- unname(nc_col_nms)
  iarc_data <- data.table::setDT(lapply(seq_along(nc_col_nms), function(j) {
    nc_col_nm <- nc_col_nms[j]
    iarc_col_nm <- iarc_col_nms[j]
    tgt_class <- class(template[[iarc_col_nm]])[1L]
    conversion_fun <- match.fun(paste0("as.", tgt_class))
    values <- conversion_fun(x[[nc_col_nm]])
    if (is.character(values)&&all(grepl("^[0-9]+$", template[[iarc_col_nm]]))) {
      # numbers as strings, such as topo and morpho
      n_digits <- nchar(template[[iarc_col_nm]][1L])
      values <- gsub("[^0-9]", "", values)
      values <- formatC(values, digits = n_digits, flag = "0")
    }
    return(values)
  }))
  data.table::setnames(iarc_data, names(iarc_data), iarc_col_nms)
  return(iarc_data[])
}


#' @rdname iarccrgtools
#' @param iarccrgtools_exe_path `[character]` (mandatory, no default)
#'
#' path to executable of IARC CRG Tools; passed to
#' [iarccrgtools::set_tools_exe_path]
#' @param iarccrgtools_work_dir `[character]` (mandatory, no default)
#'
#' path to a directory where inputs and outputs to IARC CRG Tools will be
#' stored; passed to [iarccrgtools::set_tools_work_dir()]
#' @details
#' - `iarccrgtools_tool` runs first `[iarccrgtools::interact_with_tool]` and
#'   then `[iarccrgtools::connect_tool_results_to_observations]`; see those
#'   functions for more information
#' @export
iarccrgtools_tool <- function(
  x,
  tool_name,
  iarccrgtools_exe_path,
  iarccrgtools_work_dir
  ) {
  x <- iarccrgtools_dataset(x, tool_name = tool_name)

  iarccrgtools::set_tools_exe_path(iarccrgtools_exe_path)
  iarccrgtools::set_tools_work_dir(iarccrgtools_work_dir)
  iarc_results <- iarccrgtools::interact_with_tool(
    data = x,
    tool.name = tool_name,
    clean = FALSE,
    verbose = FALSE
  )

  iarc_dt <- iarccrgtools::connect_tool_results_to_observations(
    record.ids = x[["record_id"]],
    tool.results = iarc_results
  )
  data.table::setkeyv(iarc_dt, "record_id")
  col_nms <- nordcancore::nordcan_metadata_column_name_set(
    paste0("column_name_set_iarccrgtools_mandatory_", tool_name)
  )
  iarc_col_nms <- names(col_nms)
  nc_col_nms <- unname(col_nms)
  data.table::setnames(
    iarc_dt, "record_id", nc_col_nms[iarc_col_nms == "record_id"]
  )
  return(iarc_dt[])
}









