
calc_fdr_p <- function(emm_obj, method = "fdr") {
  p_vals <- summary(emm_obj)$p.value

  out <- as.data.frame(emm_obj) %>%
    rename(any_of(c(
      "p_unadjusted"   = "p.value",
      "p_unadjusted"   = "p-value",
      "p_unadjusted"   = "p_value"
    )))

  # out$p_fdr <- p.adjust(out$p_unadjusted, method = method)
  out$p_fdr <- p.adjust(p_vals, method = method)

  out
}

rename_prob_columns <- function(emm_obj) {
  out <- as.data.frame(emm_obj)

  out <- out %>%
    mutate(
      `Probability Difference` = estimate,
      SE = SE,
      `Prob 95% CI Lower` = estimate - 1.96 * SE,
      `Prob 95% CI Upper` = estimate + 1.96 * SE
    ) %>%
    select(-estimate, -SE) # remove the original 'estimate' column as it's now represented as 'Probability'

  out
}

rename_columns <- function(emm_obj) {
  out <- as.data.frame(emm_obj)

  target_col <- intersect(c("estimate", "emmean", "PGS_scaled.trend"), names(out))[1]

  if (!is.na(target_col)) { # else it is a log-odds estimate
    out <- out %>%
      mutate(
        `Odds Ratio`      = exp(.data[[target_col]]),
        `OR 95% CI Lower` = exp(.data[[target_col]] - 1.96 * SE),
        `OR 95% CI Upper` = exp(.data[[target_col]] + 1.96 * SE)
      )
  }

  out <- out %>%
    rename(any_of(c(
      "p-value"                            = "p.value",
      "Estimated Probability"              = "prob",
      "Estimate difference (log-odds)"     = "estimate",
      "Estimated Marginal Mean (log-odds)" = "emmean",
      "Slope of PGS (scaled) in log-odds"  = "PGS_scaled.trend"
    ))) %>%
    select(-contains("asymp.LCL"), -contains("asymp.UCL"))

  return(out)
}
