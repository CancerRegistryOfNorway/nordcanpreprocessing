




#' @title NORDCAN Cancer Case Dataset
#' @description
#' Add new necessary columns to the cancer case dataset for use in computing
#' statistics.
#' @param x `[data.frame]` (mandatory, no default)
#' dataset of cancer cases
#' @section Format of input dataset:
#' See https://github.com/CancerRegistryOfNorway/NORDCAN/wiki/Call-for-data---Incidence
#' @section New columns:
#' See https://github.com/CancerRegistryOfNorway/NORDCAN/wiki/Module-PreprocessEnrich
#'
#' @importFrom data.table setDT copy month year
#' @importFrom dbc assert_is_data.frame
#' @export

###enrichment" part of our preprocessing.
###function for creating columns based on information in the other columns.
###for the moment for the columns other than the entity ones and the ones "Created by R-program from checktool-info"

# ####working directory
# setwd("")
#
# ####packages
# packages=c('data.table','remotes')
# for (p in packages){
#   if (!require(p,character.only=T)){install.packages(p,dependencies=T,repos='http://cran.us.r-project.org')}
#   library(p,character.only=T)
# }
#
# packagesgithub=c('easyassertions','basicepistats','nordcanepistats','nordcanpreprocessing')
# for (p in packagesgithub){
#   if (!require(p,character.only=T)){install_github("WetRobot/p",force=T)}
#   library(p,character.only=T)
# }
#
# ###data
# date_of_birth=sample(seq(as.Date('1900-01-01'),as.Date('1950-12-31'),by="day"),rep=T,10000)
# date_of_incidence=date_of_birth+sample(as.numeric(difftime(as.Date('1953-01-01'),as.Date('1900-01-01'))):(as.numeric(difftime(as.Date('1953-01-01'),as.Date('1900-01-01')))+1000),rep=T,10000)
# end_of_followup=date_of_incidence+sample(-600:3000,rep=T,10000)
# cancer_case_dataset=cbind.data.frame(
#   date_of_birth=date_of_birth,
#   date_of_incidence=date_of_incidence,
#   end_of_followup=end_of_followup,
#   Age=sample(0:104,10000,rep=T),
#   bod=sample(0:7,10000,rep=T),
#   autopsy=sample(0:1,10000,rep=T,prob=c(0.99,0.01))
# )

#' @importFrom data.table setDT copy := month year
enrich_nordcan_cancer_case_dataset <- function(x) {
  dbc::assert_is_data.frame(x)

  #takes a copy so the original object is not affected
  x <- data.table::setDT(data.table::copy(x))

  #definitions
  x[, "mob" := data.table::month(date_of_birth)]
  x[, "yob" := data.table::year(date_of_birth)]
  x[, "moi" := data.table::month(date_of_incidence)]
  x[, "yoi" := data.table::year(date_of_incidence)]
  x[, "mof" := data.table::month(end_of_followup)]
  x[, "yof" := data.table::year(end_of_followup)]
  x[, "surv_time" := as.numeric(end_of_followup-date_of_incidence)]
  x[autopsy == 1, "surv_time" := 0.0]
  x[, "agegroup" := cut(Age, seq(0,5*round(max(Age)/5),5),right=FALSE)]
  levels(x$agegroup)=c(1:21)
  x[, "period" := substr(cut(x$yoi,seq(5*floor(min(x$yoi)/5),
  5*ceiling(max(x$yoi)/5),5),right=FALSE),2,5)]
  x[, "excl_surv_age" := ifelse (x$Age<90,"0","1")]
  x[, "excl_surv_dco" := ifelse (x$bod==0,"1","0")]
  x[, "excl_surv_autopsy" := ifelse (x$autopsy==1,"1","0")]
  x[, "excl_surv_negativefou" := ifelse (x$surv_time<0,"1","0")]
  x[, "excl_surv_zerofou" := ifelse (x$surv_time==0,"1","0")]

  return(x[])
}



#' @importFrom iarccrgtools create_example
#' @importFrom nordcancore nordcan_column_name_set_names nordcan_column_name_set
#' @importFrom data.table setDT setnames
iarccrgtools_dataset <- function(
  x,
  tool_name
) {
  dbc::assert_is_character_nonNA_atom(tool_name)
  iarccrgtools_column_name_set_names <-
    nordcancore::nordcan_column_name_set_names()
  iarccrgtools_column_name_set_names <- iarccrgtools_column_name_set_names[
    grepl("iarccrgtools", iarccrgtools_column_name_set_names)
  ]
  tool_name_set <- sub("column_name_set_iarccrgtools_((all)|(mandatory))_",
                       "",
                       iarccrgtools_column_name_set_names)
  tool_name_set <- unique(tool_name_set)
  dbc::assert_atom_is_in_set(
    tool_name,
    set = tool_name_set
  )
  template <- iarccrgtools:::create_example(paste0("mandatory_", tool_name),
                                            n.rows = 10L)
  nc_col_nms <- nordcancore::nordcan_column_name_set(
    paste0("column_name_set_iarccrgtools_mandatory_", tool_name)
  )
  dbc::assert_is_data.frame_with_required_names(
    x = x, required_names = unname(nc_col_nms)
  )
  iarc_col_nms <- names(nc_col_nms)
  if (any(!iarc_col_nms %in% names(template))) {
    stop("internal error: mis-specified IARC CRG Tools column names: ",
         deparse(setdiff(names(template), iarc_col_nms)))
  }
  nc_col_nms <- unname(nc_col_nms)
  iarc_data <- data.table::setDT(lapply(seq_along(nc_col_nms), function(j) {
    nc_col_nm <- nc_col_nms[j]
    iarc_col_nm <- iarc_col_nms[j]
    tgt_class <- class(template[[iarc_col_nm]])[1L]
    conversion_fun <- match.fun(paste0("as.", tgt_class))
    conversion_fun(x[[nc_col_nm]])
  }))
  data.table::setnames(iarc_data, names(iarc_data), iarc_col_nms)
  return(iarc_data[])
}


#' @importFrom iarccrgtools set_tools_exe_path set_tools_work_dir
#' interact_with_tool connect_tool_results_to_observations
#' @importFrom data.table setkeyv
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
    verbose = TRUE
  )

  iarc_dt <- iarccrgtools::connect_tool_results_to_observations(
    record.ids = x[["record_id"]],
    tool.results = iarc_results
  )
  data.table::setkeyv(iarc_dt, "record_id")

  return(iarc_dt[])
}









