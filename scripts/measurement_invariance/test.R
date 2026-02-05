update_threshold_lines <- function(base_dir = "scripts/measurement_invariance") {
  # Folders to process
  folders <- c("self")
  
  # Counter for files processed
  files_processed <- 0
  
  # Old and new lines
  old_lines_pattern <- c(
    "^\\s*! constrain the second threshold of the reference item.*",
    "^\\s*\\[item8\\$2\\]\\s*\\(tb8\\);",
    "^\\s*! free all other second thresholds.*",
    "^\\s*\\[item1\\$2-item7\\$2 item9\\$2 item10\\$2\\];"
  )
  
  new_lines <- c(
    "! constrain the second threshold of the reference item (4)",
    "[item4$2] (tb4);",
    "! free all other second thresholds",
    "[item1$2-item3$2 item5$2-item10$2];"
  )
  
  for (folder in folders) {
    folder_path <- file.path(base_dir, folder)
    
    if (!dir.exists(folder_path)) {
      warning(sprintf("Folder not found: %s", folder_path))
      next
    }
    
    # Select files starting with 01_configural or 02_metric and ending with .inp
    inp_files <- list.files(folder_path, pattern = "^(01_configural|02_metric).*\\.inp$", full.names = TRUE)
    
    if (length(inp_files) == 0) {
      message(sprintf("No matching .inp files found in: %s", folder_path))
      next
    }
    
    for (inp_file in inp_files) {
      tryCatch({
        lines <- readLines(inp_file)
        
        # Find indices of lines to replace
        idx_to_replace <- sapply(old_lines_pattern, function(pat) {
          which(grepl(pat, lines))
        })
        
        # Flatten and check if all lines were found
        if (all(sapply(idx_to_replace, length) > 0)) {
          # Replace the lines
          lines[unlist(idx_to_replace)] <- new_lines
          writeLines(lines, inp_file)
          
          files_processed <- files_processed + 1
          message(sprintf("Updated: %s", basename(inp_file)))
        } else {
          message(sprintf("Threshold lines not found in: %s", basename(inp_file)))
        }
        
      }, error = function(e) {
        warning(sprintf("Error processing %s: %s", basename(inp_file), e$message))
      })
    }
  }
  
  message(sprintf("\nTotal files processed: %d", files_processed))
  return(invisible(files_processed))
}

# Example usage:
update_threshold_lines()
# update_threshold_lines(base_dir = "/path/to/measurement_invariance")
