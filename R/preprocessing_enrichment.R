




add_nordcan_entity_columns <- function(x) {

  icd10_to_entity_dt <- nordcancore::nordcan_metadata_icd10_to_entity()

  undefined_icd10_codes <- setdiff(
    x$icd10,
    c(icd10_to_entity_dt$icd10, NA_character_)
  )

  if (length(undefined_icd10_codes) > 0L) {
    warning("Following icd10 codes in x$icd10 did not exist in definition ",
            "table: ", deparse(undefined_icd10_codes), "; if you see this, ",
            "please contact the software maintainers")
  }

  x <- merge(x, icd10_to_entity_dt, all.x = TRUE, all.y = FALSE,
             by = "icd10")

  # @codedoc_comment_block entity
  # entity 300 is modfied by removing bascal cell carcinoma morpho-beh
  # combinations from it in function
  # nordcanpreprocessing:::add_nordcan_entity_columns. the following
  # combinations are set to 888L in column entity_level_30:
  # ```{R}
  # dt_basal <- data.table::CJ(
  #   morpho = 8090:8098, beh = 3L
  # )
  # knitr::kable(dt_basal)
  # ````
  # @codedoc_comment_block entity
  dt_basal <- data.table::CJ(
    morpho = 8090:8098, beh = 3L, entity_level_30 = 888L
  )
  i.entity_level_30 <- NULL # appease R CMD CHECK
  x[
    i = dt_basal,
    on = c("morpho", "beh"),
    j = "entity_level_30" := i.entity_level_30
  ]

  # @codedoc_comment_block entity
  # entity 319 is defined as all sub-types of 320 except 316 and 317. we define
  # it in function nordcanpreprocessing:::add_nordcan_entity_columns
  # by first assigning 319 to every entity_level_30 value to 319 under
  # entity_level_20 == 320, and then defining 316 and 317 as exceptions to this.
  # @codedoc_comment_block entity
  x[
    i = x[["entity_level_20"]] == 320L,
    j = "entity_level_30" := 319L
  ]

  # @codedoc_comment_block entity
  # entity 316 is defined as a sub-type of 320 based on morpho-beh combinations.
  # the combinations are as follows:
  # ```{R}
  # dt_316 <-  data.table::CJ(
  #   morpho = c(9530:9535, 9537:9539), beh = 0:3,
  #   entity_level_20 = 320L
  # )
  # knitr::kable(dt_316)
  # ```
  # @codedoc_comment_block entity
  dt_316 <-  data.table::CJ(
    morpho = c(9530:9535, 9537:9539), beh = 0:3,
    entity_level_20 = 320L, entity_level_30 = 316L
  )
  x[
    i = dt_316,
    on = c("morpho", "beh", "entity_level_20"),
    j = "entity_level_30" := i.entity_level_30
  ]

  # @codedoc_comment_block entity
  # entity 317 is defined as a sub-type of 320 based on morpho-beh combinations
  # in function nordcanpreprocessing:::add_nordcan_entity_columns.
  # the combinations are as follows:
  # ```{R}
  # morpho_317 <- c(
  #   9380, 9381, 9382, 9383, 9384, 9385, 9390, 9391, 9392, 9393,
  #   9394, 9395, 9396, 9400, 9401, 9410, 9411, 9412, 9413, 9420, 9421,
  #   9423, 9424, 9425, 9430, 9431, 9432, 9440, 9441, 9442, 9444, 9445,
  #   9450, 9451, 9460, 9470, 9471, 9472, 9473, 9474, 9475, 9476, 9477,
  #   9478, 9480
  # )
  # dt_317 <- rbind(
  #   data.table::CJ(morpho = morpho_317, beh = 1:3)
  # )
  # dt_317[, "entity_level_20" := 320L]
  # knitr::kable(dt_317)
  # ```
  # @codedoc_comment_block entity
  morpho_317 <- c(
    9380, 9381, 9382, 9383, 9384, 9385, 9390, 9391, 9392, 9393,
    9394, 9395, 9396, 9400, 9401, 9410, 9411, 9412, 9413, 9420, 9421,
    9423, 9424, 9425, 9430, 9431, 9432, 9440, 9441, 9442, 9444, 9445,
    9450, 9451, 9460, 9470, 9471, 9472, 9473, 9474, 9475, 9476, 9477,
    9478, 9480
  )
  dt_317 <- rbind(
    data.table::CJ(morpho = morpho_317, beh = 1:3)
  )
  dt_317[, "entity_level_20" := 320L]
  dt_317[, "entity_level_30" := 317L]
  x[
    i = dt_317,
    on = c("morpho", "beh", "entity_level_20"),
    j = "entity_level_30" := i.entity_level_30
  ]

  # @codedoc_comment_block entity
  # bladder/urinary tumours are only allowed to be in the following
  # morpho-beh combinations (and set to 999L otherwise):
  # ```{R}
  # dt_280 <- data.table::CJ(
  #   morpho = c(8010L, 8120L, 8130L), beh = 0:2
  # )
  # knitr::kable(dt_280)
  # ```
  # see function nordcanpreprocessing:::add_nordcan_entity_columns for the
  # implementation.
  #
  # the NORDCAN 8.2 stata code was
  # `* limit included bladder/urinary tumors to 8010/2, 8120/1, 8120/2, 8130/1, 8130/2`
  # `replace entity_level1 = "999" if entity_level1 == "280" & inrange(beh, 0,2) & !inlist(morpho, 8010, 8120, 8130)`
  # @codedoc_comment_block entity
  dt_280_to_999 <-  data.table::CJ(
    morpho = setdiff(x[["morpho"]], c(8010L, 8120L, 8130L)),
    beh = 0:2,
    entity_level_30 = 280L,
    new_entity_level_30 = 999L
  )
  i.new_entity_level_30 <- NULL
  x[
    i = dt_280_to_999,
    on = c("morpho", "beh", "entity_level_30"),
    j = "entity_level_30" := i.new_entity_level_30
  ]

  # @codedoc_comment_block entity
  # we enforce all other entity_level_* columns to NA other than
  # entity_level_30 when entity_level_30 is either 888 or 999. see function
  # nordcanpreprocessing:::add_nordcan_entity_columns for the implementation.
  # @codedoc_comment_block entity
  entity_col_nms <- nordcancore::nordcan_metadata_column_name_set(
    "column_name_set_entity"
  )
  x[
    i = x[["entity_level_30"]] %in% c(888L, 999L),
    j = (setdiff(entity_col_nms, "entity_level_30")) := NA_integer_
  ]

  return(x[])
}





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

  period_5_levels <- nordcancore::nordcan_metadata_column_level_space_list(
    "period_5"
  )[["period_5"]]
  year_breaks <- as.integer(c(period_5_levels, max(period_5_levels) + 5L))
  x[, "period_5" := cut(x$yoi, year_breaks, right = FALSE, labels = FALSE)]
  x[, "period_5" := period_5_levels[x$period_5]]

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

  if (!"icdo3_to_icd10_input.eO3to10" %in% names(icd10_dt)) {
    x[, "excl_imp_error" := rep(NA_character_, nrow(x))]
  } else {
    i.icdo3_to_icd10_input.eO3to10 <- NULL # this only to appease R CMD CHECK
    x[
      i = icd10_dt,
      on = "tum",
      j = "excl_imp_error" := i.icdo3_to_icd10_input.eO3to10,
    ]
    x[, "excl_imp_icd10conversion" := ifelse (is.na(x$excl_imp_error),0L,1L)]
  }

  mp <- nordcanpreprocessing::iarccrgtools_tool(
    x = x,
    tool_name = "multiple_primary",
    iarccrgtools_exe_path = iarccrgtools_exe_path,
    iarccrgtools_work_dir = iarccrgtools_work_dir
  )

   if (!"multiple_primary_input.mul" %in% names(mp)) {
    x[, "excl_imp_duplicate" := rep(0L, nrow(x))]
  } else {
  i.multiple_primary_input.mul <- NULL # this only to appease R CMD CHECK
  x[
    i = mp,
    on = "tum",
    j = "excl_imp_duplicate" := i.multiple_primary_input.mul,
  ]
  x[, "excl_imp_duplicate" := ifelse(grepl("\\*",x$excl_imp_duplicate),1L,0L)]
     }

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

  x <- add_nordcan_entity_columns(x)

  x[, "excl_imp_entitymissing" := ifelse(x$entity_level_30 %in% c(888L, 999L), 1L,0L)]
  gs <- nordcancore::get_global_nordcan_settings()
  first_yoi <- gs[["stat_cancer_record_count_first_year"]]
  last_yoi <- nordcancore::nordcan_metadata_nordcan_year()
  x[
    j = "excl_imp_year" := as.integer(
      !data.table::between(x$yoi, first_yoi, last_yoi, incbounds = TRUE)
    )
  ]

  excl_imp_col_nms <- names(x)[grepl("^excl_imp_", names(x))]
  excl_imp_col_nms <- setdiff(excl_imp_col_nms, "excl_imp_error")
  x[
    j = "excl_imp_total" := as.integer(rowSums(.SD) > 0L),
    .SDcols = excl_imp_col_nms
  ]

  x[, "excl_surv_age" := ifelse (x$age<90,0L,1L)]
  x[, "excl_surv_dco" := ifelse (x$bod==0,1L,0L)]
  x[, "excl_surv_autopsy" := ifelse (x$autopsy==1,1L,0L)]
  x[, "excl_surv_negativefou" := ifelse (x$surv_time<0,1L,0L)]
  x[, "excl_surv_zerofou" := ifelse (x$surv_time==0,1L,0L)]
  x[, "excl_surv_year" := ifelse(x$period_5 %in% period_5_levels, 0L,1L)]
  x[, "excl_surv_entitymissing" := ifelse(x$entity_level_30 %in% c(888L, 999L), 1L,0L)]
  x[, "excl_surv_vit_sta" := as.integer(x$vit_sta == 9L)]

  x[, "excl_surv_benign" := ifelse (x$excl_imp_benign==1,1L,0L)]
  x[, "excl_surv_icd10conversion" := ifelse (x$excl_imp_icd10conversion==1,1L,0L)]
  x[, "excl_surv_duplicate" := ifelse (x$excl_imp_duplicate==1,1L,0L)]

  excl_surv_col_nms <- names(x)[grepl("excl_surv_", names(x))]
  x[
    j = "excl_surv_total" := as.integer(rowSums(.SD) > 0L),
    .SDcols = excl_surv_col_nms
  ]

  dbc::report_to_assertion(
    dbc::tests_to_report(
      tests = "pmax(x$excl_imp_total, x$excl_surv_total) == x$excl_surv_total",
      fail_messages = paste0(
        "x$excl_surv_total did not have every exclusion that ",
        "x$excl_imp_total has"
      ),
      pass_messages = c(
        "x$excl_surv_total had every exclusion that x$excl_imp_total has "
      )
    ),
    assertion_type = "prod_output"
  )

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

  message("* nordcanpreprocessing::iarccrgtools_dataset: collected columns\n",
          "  ", deparse(nc_col_nms), "\n  to pass as columns\n",
          "  ", deparse(iarc_col_nms), "\n  for using iarccrgtools")

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

  message("* nordcanpreprocessing::iarccrgtools_tool: calling ",
          "iarccrgtools::interact_with_tool with tool.name = ",
          deparse(tool_name))
  if (grepl("multiple_primary", tool_name)) {
    message("* nordcanpreprocessing::iarccrgtools_tool: for the ",
            "multiple_primary tool, please check all of the following boxes:\n",
            "  - \"Includes unspecified (/1) or in situ (/2) tumours of the bladder\"\n",
            "  - \"Includes benign (/0) and unspecified (/1) tumours of the brain\"\n",
            "  - \"Write duplicate records into a separate file\"\n")
  }
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









