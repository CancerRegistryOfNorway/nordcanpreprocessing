
reports_on_column_name_sets <- data.table::fread(
  "data-raw/reports_on_column_sets.csv"
)
reports_on_column_name_sets[
  j = "col_nm_set" := strsplit(col_nm_set, split = "\\s*,\\s*")
]
invisible(lapply(reports_on_column_name_sets[["col_nm_set"]], function(col_nms){
  dbc::assert_vector_elems_are_in_set(
    col_nms,
    set = nordcancore::nordcan_column_name_set("column_name_set_all")
  )
}))

usethis::use_data(
  reports_on_column_name_sets, overwrite = TRUE, internal = TRUE
)
