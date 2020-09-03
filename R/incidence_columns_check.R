

incidence_columns_check_ID          <- function(data_input, column_name) {
  if(all(nchar(data_input[,column_name]) <= 50)) {
    cat(sprintf("Variable '%s' passed checking!\n", column_name))
  } else {
    id <- which(nchar(data_input[,column_name]) > 50)
    cat(sprintf("Variable '%s' has %s records can not pass the checking due to their values contains more than 50 characters.\n", column_name, length(id)))
  }
}

incidence_columns_check_Date        <- function(data_input, column_name) {
  if(all(grepl("[0-9]{4}-[0-1][[0-9]-[0-3][0-9]", data_input[,column_name]))) {
    cat("Date format of variable '%s' is correct!\n", column_name)
    
    if (!is.null(incidence_columns_regulation_list[[column_name]]$min)) {
      if (all(as.Date(data_input[,column_name]) >= as.Date(incidence_columns_regulation_list[[column_name]]$min))) {
        cat("Earliest value of variable %s passed checking!\n", column_name)
      } else {
        cat("Earliest value of variable %s not passed checking! \n", column_name)
      }
    } 
    
    if (!is.null(incidence_columns_regulation_list[[column_name]]$max)) {
      if (all(as.Date(data_input[,column_name]) >= as.Date(incidence_columns_regulation_list[[column_name]]$max))) {
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
  if (all(data_input[,column_name] %in%  incidence_columns_regulation_list[[column_name]]$levels)) {
    cat (sprintf( "All values of variable %s are valid\n", column_name))
  } else {
    cat (sprintf( "Variable %s contains invalid values\n", column_name))
    
  }
}

incidence_columns_check_Numeric     <- function(data_input, column_name) {
  if (all(!is.na(as.numeric(data_input[,column_name])))) {
    cat (sprintf("All values of variable %s are numeric\n", column_name))
    
    if (!is.null(incidence_columns_regulation_list[[column_name]]$min)) {
      if (all(data_input[,column_name] >= incidence_columns_regulation_list[[column_name]]$min)) {
        cat("Minimum value of variable %s passed checking\n", column_name)
      } else {
        cat("Minimum value of variable %s not passed checking\n", column_name)
      }
    }
    
    
    if (!is.null(incidence_columns_regulation_list[[column_name]]$max)) {
      if (all(data_input[,column_name] <= incidence_columns_regulation_list[[column_name]]$max)) {
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
  region <- incidence_columns_regulation_list[[column_name]]$table
  
  if (all(data_input[, column_name] %in% incidence_columns_regulation_list[[column_name]]$table$Value)) {
    cat("All region codes are valid! \n")
    
    if(length(unique(substr(data_input[, column_name] %in% region$Value, 1,1))) != 1) {
      cat("Some region codes may come from different countries/regions!\n")
    }
  } else {
    cat("Region codes contain unvalid value(s). \n")
  }
}


incidence_columns_check_Other       <- function(data_input, column_name) {
  cat(incidence_columns_regulation_list[[column_name]]$message)
}

