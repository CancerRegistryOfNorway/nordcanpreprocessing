


incidence_quality_check <- function(input = "synthetic_raw_2018_incidence_2020_04_23_16_00.csv") {
  
  data_input <- as.data.frame(fread(input))
  data_input_columns <- names(data_input)
  
  incidence_columns_list_names <-  names(incidence_columns_regulation_list)
  
  
  
  
  
  for (column_name in data_input_columns) {
    if (column_name %in% incidence_columns_list_names) {
      
      Format <- incidence_columns_regulation_list[[column_name]]
      
      if (Format == "ID") {
        incidence_columns_check_ID(data_input, column_name)
      } else if (Format == "Date") {
        incidence_columns_check_Date(data_input, column_name)
      } else if (Format == "Categorical") {
        incidence_columns_check_Categorical(data_input, column_name)
      } else if (Format == "Numeric") {
        incidence_columns_check_Numeric(data_input, column_name)
      } else if (Format == "Region") {
        incidence_columns_check_Region(data_input, column_name)
      } else if (Format == "Other") {
        incidence_columns_check_Other(data_input, column_name)
      }
      
    } else {
      warning(sprintf("The quanlity rule for '%s' is not registered! Please report this issue to the maintainer. ", column_name))
    }
  }
  
  
}

