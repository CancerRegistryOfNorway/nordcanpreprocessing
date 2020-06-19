###########################################
## Quality assurance: insidence data ######

QA_insidence <- function(data, tempfile, mode = "batch") {
  
  mode <- match.arg(mode, c("interactive", "batch"))
  N <- 0
  
  output <- function(obj, append = TRUE) {
    if (is.object(obj)) {
      capture.output(obj, file = tempfile, append = TRUE)
    } else {
      cat(obj, file = tempfile, append = TRUE, row.names = FALSE)
    }
    cat("\n\n", file = tempfile, append = TRUE)
    if (mode == "interactive") {print(obj); cat("\n\n"); readline(prompt = "press 'Enter' key to continue...")}
  }
  
  
  #######################
  ## Start to import data into R..
  output("Start to import the data into R...", append = FALSE)
  require(data.table)
  DAT <- as.data.table(data)
  output("Data importing finished.")
  output(sprintf("The data contains %s rows & %s columns", dim(DAT)[1], dim(DAT)[2]))
  output(head(DAT, 2))
  
  
  #######################
  ## Are all mandatory variables exist?
  Var_mandatory <- c("Age", "Autopsy", "Beh", "BoD", "CoD", "Date_of_birth", 
                     "Date_of_incidence", "End_of_FollowUp", "Laterality", 
                     "Morpho", "Pat_ID", "Region", "Sex", "Topo", "Tum_ID", "Vit_stat")
  if ( all(tolower(Var_mandatory) %in% tolower(names(DAT)))) {
    output(obj = "All mandatory columns exist!")
    if (!all(Var_mandatory %in% names(DAT))) {
      for (i in 1:length(Var_mandatory)) {
        names(DAT)[which(tolower(names(DAT)) == tolower(Var_mandatory[i]))] <- Var_mandatory[i]
      }
      output(obj = "However, some variable names not meet required style (e.g. case), and were converted to required style.")
      output(head(DAT, 2))
    }
  } else {
    var_missed <- Var_mandatory[!tolower(Var_mandatory) %in% tolower(names(DAT))]
    output(sprintf("The following variables missed: %s", paste(var_missed, collapse = ", ")))
    N <- N + 1
  }
  
  
  
  #######################
  ## Are all variables names correct?
  Var_all <- c("Pat_ID", "Date_of_birth", "Age", "Sex", "Region", "NUTS2", "Tum_ID", 
               "Date_of_incidence", "BoD", "Beh", "DCI", "Topo", "Morpho", "Grade", 
               "Autopsy", "Laterality", "Vit_stat", "End_of_FollowUp", "ICD", "CoD", "TNM_ed", 
               "cT", "cN", "cM", "pT", "pN", "pM", "ToS", "Stage", "Surgery", 
               "Rt", "Ct", "Tt", "It", "Ht", "Ot", "SCT")
  
  
  if ( all(tolower(Var_all) %in% tolower(names(DAT)))) {
    output(obj = "All variables exist!")
    if (!all(Var_all %in% names(DAT))) {
      for (i in 1:length(Var_all)) {
        names(DAT)[which(tolower(names(DAT)) == tolower(Var_all[i]))] <- Var_all[i]
      }
      output(obj = "However, some variable names not meet required style (e.g. case), and were converted to required style.")
      output(head(DAT, 2))
    }
  } else {
    var_missed <- Var_all[!tolower(Var_all) %in% tolower(names(DAT))]
    output(sprintf("The following variables missed: %s", paste(var_missed, collapse = ", ")))
    N <- N + 1
  }
  
  
  #########################################3
  ## The following variables don't allow missing
  non_miss_vars <- c("Pat_ID", "Date_of_birth", "Age", "Sex", "Region", "Tum_ID", 
                     "Date_of_incidence", "BoD", "Topo", "Morpho", 
                     "Autopsy", "Laterality", "Vit_stat", "End_of_FollowUp")
  
  output(obj = "The following variables should not have missing value: ")
  
  for (v in non_miss_vars) {
    if (all(!is.null(DAT[,v, with = FALSE]))) {
      output(paste(v, ": No missing value!" ))
    } else {
      output(paste(v, ": !!! Contain missing value !!!" ))
      N <- N + 1
    }
  }
  
  
  #######################
  ### Correct format of data ### 
  date_vars <- c("Date_of_birth", "Date_of_incidence", "End_of_FollowUp")
  
  output(obj = "The following variables should be string in format: 'YYYY-MM-DD', and the latest value can't be later than today! ")
  
  for (v in date_vars) {
    if (all(grepl("^[0-9]{4}\\.[01][0-9]\\.[0-3][0-9]$", DAT[,v, with = FALSE]))) {
      output(paste(v, ": format is correct!" ))
      if (all(as.Date(DAT[,v, with = FALSE], "%Y.%m.%d") <= Sys.Date())) {
        output(paste(v, ": all value are earlier than today" ))
      } else {
        output(paste(v, ": constains date in future!" ))
        N <- N + 1
      }
    } else {
      output(paste(v, ": !!! format is NOT correct! !!!" ))
      N <- N + 1
    }
  }
  
  
  #######################
  ## Pat_ID
  
  DAT$Pat_ID <- as.character(DAT$Pat_ID)
  
  data_size <- range(nchar(DAT$Pat_ID))
  if (all(data_size <= 50)) {
    output(sprintf("The digital length range of 'Pat_ID' is: %s - %s", data_size[1], data_size[2]))
  } else {
    output("Some value have exceeded the max length allowed.")
    id <- which(nchar(DAT$Pat_ID) > 50)
    output(head(DAT[id, ]))
    N <- N + 1
  }
  
  
  output(paste("There are", length(unique(DAT$Pat_ID)), "unique value in Pat_ID"))
  
  
  
  #######################
  ## Date_of_birth
  DAT$Date_of_birth <- as.Date(DAT$Date_of_birth, format = "%d.%m.%Y")
  data_range <- range( DAT$Date_of_birth)
  
  if (data_range[1] >= as.Date("1870-01-01")) {
    cat("All the value of 'Date_of_birth' bigger than 01.01.1870.")
  } else {
    cat("Variable 'Date_of_birth' contains value less than 01.01.1870!")
    id <- which(DAT$Date_of_birth < as.Date("1870-01-01"))
    cat(head(DAT[id, ]))
    N <- N + 1
  }
  
  
  #######################
  ## Age
  
  DAT$Age <- as.numeric(DAT$Age)
  if (all(is.numeric(DAT$Age))) {
    if (max( DAT$Age) <= 121) {
      output("All the value of 'Age' are no bigger than 121")
    } else {
      output("Variable 'Age' contains value not allowed!")
      id <- which(!is.numeric(DAT$Age))
      output(head(DAT[id, ]))
      N <- N + 1
    }
  } else {
    output("Variable 'Age' contains value which is not numeric!")
    id <- which(!DAT$Age > 121)
    output(head(DAT[id, ]))
    N <- N + 1
  }
  
  
  #######################
  ## Sex
  
  sex_values <- unique(DAT$Sex)
  
  if (all(sex_values %in% c(1, 2, 3))) {
    output("All value of 'Sex' are legal")
  } else {
    output("Variable 'Sex' contains value not allowed!")
    
    id <- which(!DAT$Sex %in%  c(1, 2, 3))
    output(head(DAT[id, ]))
    N <- N + 1
  }
  
  
  #######################
  ## Region
  
  region_values <- unique(DAT$Region)
  
  if (all(region_values %in% 10:90)) {
    output("All value of 'Region' are legal")
  } else {
    output("Variable 'Region' contains value not allowed!")
    
    id <- which(!DAT$Region %in%  10:90)
    output(head(DAT[id, ]))
    N <- N + 1
  }
  
  
  #######################
  ## Tum_ID
  
  Tum_ID_size <- max(nchar(DAT$Tum_ID))
  
  if (Tum_ID_size <= 50) {
    output("All character length of 'Tum_ID' are no longer than 50")
    if (length(DAT$Tum_ID) == length(unique(DAT$Tum_ID))) {
      output("All value of 'Tum_ID' are unique!")
    } else {
      output("Some value of 'Tum_ID' are NOT unique!")
      id <- which(duplicated(DAT$Tum_ID))
      output(head(DAT[id, ]))
      N <- N + 1
    }
    
  } else {
    output("Variable 'Tum_ID' contains value which length are longer than 50!")
    id <- which(nchar(DAT$Tum_ID) > 50)
    output(head(DAT[id, ]))
    N <- N + 1
  }
  
  output(paste("There are", length(unique(DAT$Tum_ID)), "unique values in variable 'Tum_ID'"))
  
  #######################
  ## Date_of_incidence
  DAT$Date_of_incidence <- as.Date(DAT$Date_of_incidence, format = "%d.%m.%Y")
  data_range <- range( DAT$Date_of_incidence)
  
  if (data_range[1] >= as.Date("1901-01-01")) {
    output("All the value of 'Date_of_incidence' bigger than 1901-01-01.")
  } else {
    output("Variable 'Date_of_incidence' contains value less than 1901-01-01!")
    id <- which(DAT$Date_of_incidence < as.Date("1901-01-01"))
    output(head(DAT[id, ]))
    N <- N + 1
  }
  
  
  #######################
  ## BoD
  BoD_values <- unique(DAT$BoD)
  if (all(BoD_values %in% c(0,1, 2, 4,5,6,7,9))) {
    output("All value of 'BoD' are legal")
  } else {
    output("Variable 'BoD' contains value not allowed!")
    id <- which(!DAT$BoD %in% c(0,1, 2, 4,5,6,7,9))
    output(head(DAT[id, ]))
    N <- N + 1
  }
  
  #######################
  ## Topo
  Topo_size <- max(nchar(DAT$Topo))
  if (Topo_size <= 4) {
    output("The length of charcter for variable 'BoD' meet requirement.")
  } else {
    output("Some value of variable 'BoD' are not allowed.")
    id <- which(nchar(DAT$Topo) > 4)
    output(head(DAT[id, ]))
    N <- N + 1
  }
  
  
  #######################
  ## Morpho
  Morpho_size <- max(nchar(DAT$Morpho))
  if (Morpho_size <= 4) {
    output("The length of charcter for variable 'Morpho' meet requirement.")
  } else {
    output("Some value of variable 'Morpho' are not allowed.")
    id <- which(nchar(DAT$Morpho) > 4)
    output(head(DAT[id, ]))
    N <- N + 1
  }
  
  #######################
  ## Autospy
  Autospy_values <- unique(DAT$Autopsy)
  if (all(Autospy_values %in% c(0,1,9))) {
    output("All value of 'Autopsy' are legal")
  } else {
    output("Variable 'Autopsy' contains value not allowed!")
    id <- which(!DAT$Autopsy %in% c(0,1,9))
    output(head(DAT[id, ]))
    N <- N + 1
  }
  
  #######################
  ## Laterality
  Laterality_values <- unique(DAT$Laterality)
  if (all(Laterality_values %in% c(0,1,2,3,4,9))) {
    output("All value of 'Laterality' are legal")
  } else {
    output("Variable 'Laterality' contains value not allowed!")
    id <- which(!DAT$Laterality %in% c(0,1,2,3,4,9))
    output(head(DAT[id, ]))
    N <- N + 1
  }
  
  
  #######################
  ## Vit_stat
  Vit_stat_values <- unique(DAT$Vit_stat)
  if (all(Vit_stat_values %in% c(1,2,3))) {
    output("All value of 'Vit_stat' are legal")
    Vit_stat_value_autopsy <- unique(DAT$Vit_stat[DAT$Autopsy == 1])
    if (all(Vit_stat_value_autopsy == 2)) {
      output("All value of 'Vit_stat' are 2 when 'Autopsy' equal 1")
    } else {
      output("Some value of 'Vit_stat' are NOT 2 when 'Autopsy' equal 1")
      
      id <- which(DAT$Autopsy == 1 & DAT$Vit_stat != 2)
      output(head(DAT[id, ]))
      N <- N + 1
    }
  } else {
    output("Variable 'Vit_stat' contains value not allowed!")
    
    id <- which(!DAT$Vit_stat %in% c(1,2,3))
    output(head(DAT[id, ]))
    N <- N + 1
  }
  
  #######################
  ## End_of_FollowUp
  DAT$End_of_FollowUp <- as.Date(DAT$End_of_FollowUp, format = "%d.%m.%Y")
  data_range <- range( DAT$End_of_FollowUp)
  
  if (data_range[1] >= as.Date("1901-01-01")) {
    output("All the value of 'End_of_FollowUp' are bigger than 1901-01-01.")
  } else {
    output("Variable 'End_of_FollowUp' contains value less than 1901-01-01!")
    id <- which(DAT$End_of_FollowUp < as.Date("1901-01-01"))
    output(head(DAT[id, ]))
    N <- N + 1
  }
  
  #######################
  ## CoD
  CoD_size <- max(nchar(DAT$CoD))
  if (CoD_size <= 5) {
    output("All character length of the values of 'CoD' are no longer than 5")
    CoD_Vit_stat_value <- unique(DAT$CoD[DAT$Vit_stat == 1])
    if (all(CoD_Vit_stat_value == "")) {
      output("All value of 'CoD' are '' when 'Vit_stat' equal 1")
    } else {
      output("Some value of 'CoD' are NOT '' when 'Vit_stat' equal 1")
      
      id <- which(DAT$Vit_stat == 1 & DAT$CoD != "")
      output(head(DAT[id, ]))
      N <- N + 1
    }
  } else {
    output("Some values of 'CoD' are longer than 5 digits.")
    id <- which(nchar(DAT$CoD) > 5)
    output(head(DAT[id, ]))
    N <- N + 1
  }
  
  
  if (N > 0) {
    output(paste("There are", N, "issues need to fix, see the report file for details!"))
  }
  
}








