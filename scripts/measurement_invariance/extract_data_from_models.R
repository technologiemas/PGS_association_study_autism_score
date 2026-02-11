# This script extracts all the data from the Mplus model outputs and collects them in a csv file
# Large parts of this code are AI generated, it seems to work well

library(MplusAutomation)
library(dplyr)

# -------------------------------------------------------------------------
# 1. READ MODELS
# -------------------------------------------------------------------------

mother_configural = readModels("scripts/measurement_invariance/mother/01_configural_mother.out") 
mother_configural_extended = readModels("scripts/measurement_invariance/mother/01_configural_mother_extended.out")
mother_configural_extended_item1 = readModels("scripts/measurement_invariance/mother/01_configural_mother_extended_item1.out")
father_configural = readModels("scripts/measurement_invariance/father/01_configural_father.out")
father_configural_extended = readModels("scripts/measurement_invariance/father/01_configural_father_extended.out")
father_configural_extended_item1 = readModels("scripts/measurement_invariance/father/01_configural_father_extended_item1.out")
self_configural = readModels("scripts/measurement_invariance/self/01_configural_self.out")
self_configural_extended = readModels("scripts/measurement_invariance/self/01_configural_self_extended.out")
self_configural_extended_item1 = readModels("scripts/measurement_invariance/self/01_configural_self_extended_item1.out")
teacher_configural = readModels("scripts/measurement_invariance/teacher/01_configural_teacher.out")
teacher_configural_extended = readModels("scripts/measurement_invariance/teacher/01_configural_teacher_extended.out")
teacher_configural_extended_item1 = readModels("scripts/measurement_invariance/teacher/01_configural_teacher_extended_item1.out")

mother_metric = readModels("scripts/measurement_invariance/mother/02_metric_mother.out")
mother_metric_extended = readModels("scripts/measurement_invariance/mother/02_metric_mother_extended.out")
mother_metric_extended_item1 = readModels("scripts/measurement_invariance/mother/02_metric_mother_extended_item1.out")
father_metric = readModels("scripts/measurement_invariance/father/02_metric_father.out")
father_metric_extended = readModels("scripts/measurement_invariance/father/02_metric_father_extended.out")
father_metric_extended_item1 = readModels("scripts/measurement_invariance/father/02_metric_father_extended_item1.out")
self_metric = readModels("scripts/measurement_invariance/self/02_metric_self.out")
self_metric_extended = readModels("scripts/measurement_invariance/self/02_metric_self_extended.out")
self_metric_extended_item1 = readModels("scripts/measurement_invariance/self/02_metric_self_extended_item1.out")
teacher_metric = readModels("scripts/measurement_invariance/teacher/02_metric_teacher.out")
teacher_metric_extended = readModels("scripts/measurement_invariance/teacher/02_metric_teacher_extended.out")
teacher_metric_extended_item1 = readModels("scripts/measurement_invariance/teacher/02_metric_teacher_extended_item1.out")

mother_strong = readModels("scripts/measurement_invariance/mother/03_strong_mother.out")
mother_strong_extended = readModels("scripts/measurement_invariance/mother/03_strong_mother_extended.out")
mother_strong_extended_item1 = readModels("scripts/measurement_invariance/mother/03_strong_mother_extended_item1.out")
father_strong = readModels("scripts/measurement_invariance/father/03_strong_father.out")
father_strong_extended = readModels("scripts/measurement_invariance/father/03_strong_father_extended.out")
father_strong_extended_item1 = readModels("scripts/measurement_invariance/father/03_strong_father_extended_item1.out")
self_strong = readModels("scripts/measurement_invariance/self/03_strong_self.out")
self_strong_extended = readModels("scripts/measurement_invariance/self/03_strong_self_extended.out")
self_strong_extended_item1 = readModels("scripts/measurement_invariance/self/03_strong_self_extended_item1.out")
teacher_strong = readModels("scripts/measurement_invariance/teacher/03_strong_teacher.out")
teacher_strong_extended = readModels("scripts/measurement_invariance/teacher/03_strong_teacher_extended.out")
teacher_strong_extended_item1 = readModels("scripts/measurement_invariance/teacher/03_strong_teacher_extended_item1.out")

mother_strict = readModels("scripts/measurement_invariance/mother/04_strict_mother.out")
mother_strict_extended = readModels("scripts/measurement_invariance/mother/04_strict_mother_extended.out")
mother_strict_extended_item1 = readModels("scripts/measurement_invariance/mother/04_strict_mother_extended_item1.out")
father_strict = readModels("scripts/measurement_invariance/father/04_strict_father.out")
father_strict_extended = readModels("scripts/measurement_invariance/father/04_strict_father_extended.out")
father_strict_extended_item1 = readModels("scripts/measurement_invariance/father/04_strict_father_extended_item1.out")
self_strict = readModels("scripts/measurement_invariance/self/04_strict_self.out")
self_strict_extended = readModels("scripts/measurement_invariance/self/04_strict_self_extended.out")
self_strict_extended_item1 = readModels("scripts/measurement_invariance/self/04_strict_self_extended_item1.out")
teacher_strict = readModels("scripts/measurement_invariance/teacher/04_strict_teacher.out")
teacher_strict_extended = readModels("scripts/measurement_invariance/teacher/04_strict_teacher_extended.out")
teacher_strict_extended_item1 = readModels("scripts/measurement_invariance/teacher/04_strict_teacher_extended_item1.out")

mother_full = readModels("scripts/measurement_invariance/mother/05_full_mother.out")
mother_full_extended = readModels("scripts/measurement_invariance/mother/05_full_mother_extended.out")
mother_full_extended_item1 = readModels("scripts/measurement_invariance/mother/05_full_mother_extended_item1.out")
father_full = readModels("scripts/measurement_invariance/father/05_full_father.out")
father_full_extended = readModels("scripts/measurement_invariance/father/05_full_father_extended.out")
father_full_extended_item1 = readModels("scripts/measurement_invariance/father/05_full_father_extended_item1.out")
self_full = readModels("scripts/measurement_invariance/self/05_full_self.out")
self_full_extended = readModels("scripts/measurement_invariance/self/05_full_self_extended.out")
self_full_extended_item1 = readModels("scripts/measurement_invariance/self/05_full_self_extended_item1.out")
teacher_full = readModels("scripts/measurement_invariance/teacher/05_full_teacher.out")
teacher_full_extended = readModels("scripts/measurement_invariance/teacher/05_full_teacher_extended.out")
teacher_full_extended_item1 = readModels("scripts/measurement_invariance/teacher/05_full_teacher_extended_item1.out")

# -------------------------------------------------------------------------
# 2. ORGANIZE AND EXTRACT FIT INDICES
# -------------------------------------------------------------------------

# safe helpers for extraction
safe_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  v <- suppressWarnings(as.numeric(as.character(x)))
  if (length(v) == 0) return(NA_real_) else v
}
safe_rmsea_str <- function(m) {
  if (is.null(m) || is.null(m$summaries)) return(NA_character_)
  rm <- safe_num(m$summaries$RMSEA_Estimate)
  if (is.na(rm)) return(NA_character_)
  return(rm)
  # lb <- safe_num(m$summaries$RMSEA_90CI_LB)
  # ub <- safe_num(m$summaries$RMSEA_90CI_UB)
  if (all(is.na(c(rm, lb, ub)))) return(NA_character_)
  sprintf("%.3f (%.3f, %.3f)", rm, lb, ub)
}

# gather models into lists
models <- list(
  Mother  = list(mother_configural, mother_metric, mother_strong, mother_strict, mother_full),
  Mother_Extended = list(mother_configural_extended, mother_metric_extended, mother_strong_extended, mother_strict_extended, mother_full_extended),
  Mother_Extended_Item1 = list(mother_configural_extended_item1, mother_metric_extended_item1, mother_strong_extended_item1, mother_strict_extended_item1, mother_full_extended_item1),
  Father  = list(father_configural, father_metric, father_strong, father_strict, father_full),
  Father_Extended = list(father_configural_extended, father_metric_extended, father_strong_extended, father_strict_extended, father_full_extended),
  Father_Extended_Item1 = list(father_configural_extended_item1, father_metric_extended_item1, father_strong_extended_item1, father_strict_extended_item1, father_full_extended_item1),
  Self    = list(self_configural, self_metric, self_strong, self_strict, self_full),
  Self_Extended = list(self_configural_extended, self_metric_extended, self_strong_extended, self_strict_extended, self_full_extended),
  Self_Extended_Item1 = list(self_configural_extended_item1, self_metric_extended_item1, self_strong_extended_item1, self_strict_extended_item1, self_full_extended_item1),
  Teacher = list(teacher_configural, teacher_metric, teacher_strong, teacher_strict, teacher_full),
  Teacher_Extended = list(teacher_configural_extended, teacher_metric_extended, teacher_strong_extended, teacher_strict_extended, teacher_full_extended),
  Teacher_Extended_Item1 = list(teacher_configural_extended_item1, teacher_metric_extended_item1, teacher_strong_extended_item1, teacher_strict_extended_item1, teacher_full_extended_item1)
)

extract_metric <- function(lst, field, fun = safe_num) {
  unlist(lapply(lst, function(m) {
    if (is.null(m) || is.null(m$summaries)) return(NA_real_)
    fun(m$summaries[[field]])
  }))
}

ChiSq <- unlist(lapply(models, function(lst) extract_metric(lst, "ChiSqM_Value", safe_num)))
CFI   <- unlist(lapply(models, function(lst) extract_metric(lst, "CFI", safe_num)))
RMSEA <- unlist(lapply(models, function(lst) sapply(lst, safe_rmsea_str)))
DiffTest_p <- unlist(lapply(models, function(lst) extract_metric(lst, "ChiSqDiffTest_PValue", safe_num)))

# Create Main Fit Statistics Dataframe
df <- data.frame(
  Rater = rep(names(models), each = 5),
  Model = rep(c("Configural", "Metric", "Strong", "Strict", "Full"), times = length(models)),
  ChiSq = as.numeric(ChiSq),
  CFI = as.numeric(CFI),
  RMSEA = RMSEA,
  DiffTest_p = as.numeric(DiffTest_p),
  stringsAsFactors = FALSE
)

df <- df %>% 
  rename(`DIFFTEST p-value` = DiffTest_p,
         `Chi-Square` = ChiSq)

library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "Results")
addWorksheet(wb, "Modification Indices")

writeData(wb, "Results", df)

# -------------------------------------------------------------------------
# 3. EXTRACT MODIFICATION INDICES (MEANS/INTERCEPTS/THRESHOLDS)
# -------------------------------------------------------------------------

# Function to extract detailed M.I.s for Means/Intercepts/Thresholds
get_mi_details <- function(model_list, rater_name) {
  model_stages <- c("Configural", "Metric", "Strong", "Strict", "Full")
  
  # Iterate over each model stage in the list
  mi_list <- lapply(seq_along(model_list), function(i) {
    m <- model_list[[i]]
    stage <- model_stages[i]
    
    # Check if MI data exists
    if (is.null(m) || is.null(m$mod_indices)) return(NULL)
    
    # Extract Modification Indices
    mis <- m$mod_indices
    
    # only extract MIs when the p-value from the chi-square difference test is below 0.01 and is not the configural or full model, otherwise return NULL
    if (!is.null(m$summaries$ChiSqDiffTest_PValue)) {
      p_value <- safe_num(m$summaries$ChiSqDiffTest_PValue)
      if (!is.na(p_value) && p_value >= 0.01) {
        return(NULL)
      }
    }

    # Filter for Means/Intercepts/Thresholds, eg [item1] or [item1$1]:
    target_mis <- mis %>%
      filter((is.na(operator) | operator == "") | grepl("\\[.*\\]", modV1) & !grepl("F", modV1)) %>%
      select(modV1, MI, EPC, Group, Std_EPC, StdYX_EPC) %>% # Select relevant columns
      mutate(
        Rater = rater_name,
        Model = stage,
        Parameter = modV1, # Format to look like Mplus output
        Group = Group, # this has to be adapted based on grouping variable
        `Modification Index (MI)` = MI,
        `Expected Parameter Change (EPC)` = EPC,
        `Std EPC` = Std_EPC,
        `StdYX EPC` = StdYX_EPC
      ) %>%
      select(Rater, Model, Parameter, Group, `Modification Index (MI)`, `Expected Parameter Change (EPC)`, `Std EPC`, `StdYX EPC`) %>%
      filter(!Model %in% c("Configural", "Full")) # Exclude Configural and Full models

      
    return(target_mis)
  })

  
  # Combine into one dataframe
  do.call(rbind, mi_list)
}

# Run the extraction across all models
all_mi_data <- do.call(rbind, lapply(names(models), function(n) {
  get_mi_details(models[[n]], n)
}))

writeData(wb, "Modification Indices", all_mi_data)

note_col_results <- ncol(df) + 2
note_col_mi <- ncol(all_mi_data) + 2
note_rows <- 2:11  # Span 10 rows for the note

# Write notes
writeData(wb, "Results", 
          "Note: Extended models represent those with modeled covariance between residuals of items. For Mother, Father and Teacher these are item4 WITH item10, and for self this is item4 WITH item9. The DiffTest compare the model against the previous model (i.e. Metric vs Configural). The first p-value under our threshold of p=0.01 in each rater group represents the level of measurement invariance that is not supported by the data.",
          startRow = note_rows[1], startCol = note_col_results)

writeData(wb, "Modification Indices", 
          "Note: This file contains detailed Modification Indices (M.I.s) for Means/Intercepts/Thresholds of items extracted from Mplus measurement invariance models. Only M.I.s above 20 from models where the DIFFTEST p-value is below 0.01 are included. Configural and Full models are excluded. \n \"The Std E.P.C. indices are standardized using the variances of the continuous latent variables. The StdYX E.P.C. indices are standardized using the variances of the continuous latent variables as well as the variances of the background and/or outcome variables.\" - (Muthén & Muthén, 1998-2017)",
          startRow = note_rows[1], startCol = note_col_mi)

# Merge cells vertically for the note
mergeCells(wb, "Modification Indices", cols = note_col_mi, rows = note_rows)
mergeCells(wb, "Results", cols = note_col_results, rows = note_rows)

# Apply text wrapping and set column width
note_style <- createStyle(wrapText = TRUE, valign = "top")
addStyle(wb, "Modification Indices", note_style, rows = note_rows, cols = note_col_mi)
setColWidths(wb, "Modification Indices", cols = note_col_mi, widths = 50)

addStyle(wb, "Results", note_style, rows = note_rows, cols = note_col_results)
setColWidths(wb, "Results", cols = note_col_results, widths = 50)

saveWorkbook(wb, "results/measurement_invariance_results.xlsx", overwrite = TRUE)
