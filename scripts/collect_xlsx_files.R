# This script was used to collect all the xlsx files in the results folder and save them as a single xlsx file for the supplementary materials. 
#It is not used in the main analyses.

rm(list = ls(all = TRUE))
gc()

library(openxlsx)

# xlsx_files_paths <- list.files("results", pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)

xlsx_files_paths = c(
    "results/correlation_matrices.xlsx",
    "results/descriptives.xlsx",
    "results/measurement_invariance_results.xlsx",
    "results/model_output.xlsx",
    "results/post_hoc_investigations/post.xlsx",
    "results/post_hoc_investigations/post_sensitivity.xlsx",
    "results/post_hoc_investigations/post_sensitivity_all_raters.xlsx"
)

xlsx_files = list()

# reads all the sheets of an xlsx file and returns them as a list of dataframes. The names are the names of the sheets."
read_all_sheets = function(xlsxFile) {
  sheet_names = openxlsx::getSheetNames(xlsxFile)
  sheet_list = as.list(rep(NA, length(sheet_names)))
  names(sheet_list) = sheet_names
  for (sn in sheet_names) {
    sheet_list[[sn]] = openxlsx::read.xlsx(xlsxFile, sheet=sn)
  }
  return(sheet_list)
}

for (xlsx_file_path in xlsx_files_paths) {
  xlsx_files = append(xlsx_files, read_all_sheets(xlsx_file_path))
}

openxlsx::write.xlsx(xlsx_files, file = "results/supplementary_tables.xlsx")
