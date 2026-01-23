# This script contains some helper functions to rename variables in the dataframes

# helper functions
relabel_rater <- function(x) {
  factor(
    x,
    levels = c("m12", "v12", "t12", "ysr14"),
    labels = c("Mother", "Father", "Teacher", "Self")
  )
}

relabel_sex <- function(x) {
  factor(
    x,
    levels = c("MALE", "FEMALE"),
    labels = c("Male", "Female")
  )
}

relabel_autism_score <- function(x) {
  factor(
    x,
    levels = c(1, 2, 3),
    labels = c("No", "Low", "High"),
    ordered = TRUE
  )
}

relable_wrapper = function(df) {
  if ("autism_score_ordinal" %in% names(df)) {
    df$autism_score_ordinal = relabel_autism_score(df$autism_score_ordinal)
  }
  if ("autism_score_ordinal_sensitivity" %in% names(df)) {
    df$autism_score_ordinal_sensitivity = relabel_autism_score(df$autism_score_ordinal_sensitivity)
  }
  if ("rater_type" %in% names(df)) {
    df$rater_type = relabel_rater(df$rater_type)
  }
  if ("sex" %in% names(df)) {
    df$sex = relabel_sex(df$sex)
  }
  return(df)
}