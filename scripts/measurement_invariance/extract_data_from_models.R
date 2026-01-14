library(MplusAutomation)
library(dplyr)

mother_configural = readModels("scripts/measurement_invariance/mother/01_configural_mother.out") 
father_configural = readModels("scripts/measurement_invariance/father/01_configural_father.out")
father_configural_extended = readModels("scripts/measurement_invariance/father/01_configural_father_extended.out")
self_configural = readModels("scripts/measurement_invariance/self/01_configural_self.out")
self_configural_extended = readModels("scripts/measurement_invariance/self/01_configural_self_extended.out")
self_configural_item1 = readModels("scripts/measurement_invariance/self/01_configural_self_item1.out")
self_configural_extended_item1 = readModels("scripts/measurement_invariance/self/01_configural_self_extended_item1.out")
teacher_configural = readModels("scripts/measurement_invariance/teacher/01_configural_teacher.out")
teacher_configural_extended = readModels("scripts/measurement_invariance/teacher/01_configural_teacher_extended.out")

mother_metric = readModels("scripts/measurement_invariance/mother/02_metric_mother.out")
father_metric = readModels("scripts/measurement_invariance/father/02_metric_father.out")
father_metric_extended = readModels("scripts/measurement_invariance/father/02_metric_father_extended.out")
self_metric = readModels("scripts/measurement_invariance/self/02_metric_self.out")
self_metric_extended = readModels("scripts/measurement_invariance/self/02_metric_self_extended.out")
self_metric_extended_item1 = readModels("scripts/measurement_invariance/self/02_metric_self_extended_item1.out")
teacher_metric = readModels("scripts/measurement_invariance/teacher/02_metric_teacher.out")
teacher_metric_extended = readModels("scripts/measurement_invariance/teacher/02_metric_teacher_extended.out")

mother_strong = readModels("scripts/measurement_invariance/mother/03_strong_mother.out")
father_strong = readModels("scripts/measurement_invariance/father/03_strong_father.out")
father_strong_extended = readModels("scripts/measurement_invariance/father/03_strong_father_extended.out")
self_strong = readModels("scripts/measurement_invariance/self/03_strong_self.out")
self_strong_extended = readModels("scripts/measurement_invariance/self/03_strong_self_extended.out")
self_strong_extended_item1 = readModels("scripts/measurement_invariance/self/03_strong_self_extended_item1.out")
teacher_strong = readModels("scripts/measurement_invariance/teacher/03_strong_teacher.out")
teacher_strong_extended = readModels("scripts/measurement_invariance/teacher/03_strong_teacher_extended.out")

mother_strict = readModels("scripts/measurement_invariance/mother/04_strict_mother.out")
father_strict = readModels("scripts/measurement_invariance/father/04_strict_father.out")
father_strict_extended = readModels("scripts/measurement_invariance/father/04_strict_father_extended.out")
self_strict = readModels("scripts/measurement_invariance/self/04_strict_self.out")
self_strict_extended = readModels("scripts/measurement_invariance/self/04_strict_self_extended.out")
self_strict_extended_item1 = readModels("scripts/measurement_invariance/self/04_strict_self_extended_item1.out")
teacher_strict = readModels("scripts/measurement_invariance/teacher/04_strict_teacher.out")
teacher_strict_extended = readModels("scripts/measurement_invariance/teacher/04_strict_teacher_extended.out")

mother_full = readModels("scripts/measurement_invariance/mother/05_full_mother.out")
father_full = readModels("scripts/measurement_invariance/father/05_full_father.out")
father_full_extended = readModels("scripts/measurement_invariance/father/05_full_father_extended.out")
self_full = readModels("scripts/measurement_invariance/self/05_full_self.out")
self_full_extended = readModels("scripts/measurement_invariance/self/05_full_self_extended.out")
self_full_extended_item1 = readModels("scripts/measurement_invariance/self/05_full_self_extended_item1.out")
teacher_full = readModels("scripts/measurement_invariance/teacher/05_full_teacher.out")
teacher_full_extended = readModels("scripts/measurement_invariance/teacher/05_full_teacher_extended.out")


# This code is AI generated but it seems to work perfectly
# safe helpers for extraction
safe_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  v <- suppressWarnings(as.numeric(as.character(x)))
  if (length(v) == 0) return(NA_real_) else v
}
safe_rmsea_str <- function(m) {
  if (is.null(m) || is.null(m$summaries)) return(NA_character_)
  rm <- safe_num(m$summaries$RMSEA_Estimate)
  lb <- safe_num(m$summaries$RMSEA_90CI_LB)
  ub <- safe_num(m$summaries$RMSEA_90CI_UB)
  if (all(is.na(c(rm, lb, ub)))) return(NA_character_)
  sprintf("%.3f (%.3f, %.3f)", rm, lb, ub)
}

# gather models into lists and extract measures robustly
models <- list(
  Mother  = list(mother_configural, mother_metric, mother_strong, mother_strict, mother_full),
  Father  = list(father_configural, father_metric, father_strong, father_strict, father_full),
  Father_Extended = list(father_configural_extended, father_metric_extended, father_strong_extended, father_strict_extended, father_full_extended),
  Self    = list(self_configural, self_metric, self_strong, self_strict, self_full),
  Self_Extended = list(self_configural_extended, self_metric_extended, self_strong_extended, self_strict_extended, self_full_extended),
  Self_Item1 = list(self_configural_item1, self_configural_item1, self_configural_item1, self_configural_item1, self_configural_item1),
  Self_Extended_Item1 = list(self_configural_extended_item1, self_metric_extended_item1, self_strong_extended_item1, self_strict_extended_item1, self_full_extended_item1),
  Teacher = list(teacher_configural, teacher_metric, teacher_strong, teacher_strict, teacher_full),
  Teacher_Extended = list(teacher_configural_extended, teacher_metric_extended, teacher_strong_extended, teacher_strict_extended, teacher_full_extended)
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

df <- data.frame(
  Reporter = rep(names(models), each = 5),
  Model = rep(c("Configural", "Metric", "Strong", "Strict", "Full"), times = length(models)),
  ChiSq = as.numeric(ChiSq),
  CFI = as.numeric(CFI),
  RMSEA = RMSEA,
  DiffTest_p = as.numeric(DiffTest_p),
  stringsAsFactors = FALSE
)

df <- df %>% 
  rename(`RMSEA (90% CI)` = RMSEA,
         `DiffTest p-value` = DiffTest_p,
         `Chi-Square` = ChiSq)

write.csv(df, "results/measurement_invariance_results.csv", row.names = FALSE)
