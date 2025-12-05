library(MplusAutomation)
library(dplyr)

mother_configural = readModels("scripts/measurement_invariance/01_configural_mother.out") 
father_configural = readModels("scripts/measurement_invariance/01_configural_father.out")
self_configural = readModels("scripts/measurement_invariance/01_configural_self.out")
teacher_configural = readModels("scripts/measurement_invariance/01_configural_teacher.out")

mother_metric = readModels("scripts/measurement_invariance/02_metric_mother.out")
father_metric = readModels("scripts/measurement_invariance/02_metric_father.out")
self_metric = readModels("scripts/measurement_invariance/02_metric_self.out")
teacher_metric = readModels("scripts/measurement_invariance/02_metric_teacher.out")

mother_strong = readModels("scripts/measurement_invariance/03_strong_mother.out")
father_strong = readModels("scripts/measurement_invariance/03_strong_father.out")
self_strong = readModels("scripts/measurement_invariance/03_strong_self.out")
teacher_strong = readModels("scripts/measurement_invariance/03_strong_teacher.out")

mother_strict = readModels("scripts/measurement_invariance/04_strict_mother.out")
father_strict = readModels("scripts/measurement_invariance/04_strict_father.out")
self_strict = readModels("scripts/measurement_invariance/04_strict_self.out")
teacher_strict = readModels("scripts/measurement_invariance/04_strict_teacher.out")

mother_full = readModels("scripts/measurement_invariance/05_full_mother.out")
father_full = readModels("scripts/measurement_invariance/05_full_father.out")
self_full = readModels("scripts/measurement_invariance/05_full_self.out")
teacher_full = readModels("scripts/measurement_invariance/05_full_teacher.out")


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
  Self    = list(self_configural, self_metric, self_strong, self_strict, self_full),
  Teacher = list(teacher_configural, teacher_metric, teacher_strong, teacher_strict, teacher_full)
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
