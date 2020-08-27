




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
# date_of_incidence=date_of_birth+sample(as.numeric(difftime(as.Date('1953-01-01'),as.Date('1900-01-01'))):(as.numeric(difftime(as.Date('1953-01-01'),as.Date('1900-01-01')))+10000),rep=T,10000)
# end_of_followup=date_of_incidence+sample(-600:6000,rep=T,10000)
# cancer_case_dataset=cbind.data.frame(
#   date_of_birth=date_of_birth,
#   date_of_incidence=date_of_incidence,
#   end_of_followup=end_of_followup,
#   Age=sample(0:104,10000,rep=T),
#   bod=sample(0:7,10000,rep=T),
#   autopsy=sample(0:1,10000,rep=T,prob=c(0.99,0.01))
# )

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

  return(x[])
}







