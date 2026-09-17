# This script investigates why self-reported autism sum scores are much higher than
# mother, father and teacher reports by looking at the individual items.
# Question: is the higher self score spread evenly over the items, or do a few items
# account for most of the gap?
#
# NB: the self-report (YSR) sum has 9 items (q80 differs in the YSR), the other raters 10.
# So the raw sum scores are not on the same number of items. Comparisons below are
# made on the 9 shared items where it matters.
#
# All numbers are computed in the analytic sample: only the reports that end up in
# 02_full_dataset_long.rds (i.e. that are used in the models).

rm(list = ls(all = TRUE))
gc()

library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)

source("scripts/_column_names.R")
source("scripts/_10_functions.R")  # theme_pgs(), level_colours, figure sizes

data_all_items = readRDS("data/processed/02_data_all_items.rds")
data_long = readRDS("data/processed/02_full_dataset_long.rds")

rater_levels = c("Mother", "Father", "Teacher", "Self")

# short item wordings (CBCL/TRF wording, the YSR asks the same in first person).
# The SPSS labels are removed in 02_clean_data.R, so check these against the questionnaire.
item_labels = c(
  q1   = "1 Acts too young for age",
  q9   = "9 Can't get mind off thoughts",
  q17  = "17 Daydreams / lost in thoughts",
  q42  = "42 Would rather be alone",
  q62  = "62 Poorly coordinated / clumsy",
  q66  = "66 Repeats acts (compulsions)",
  q79  = "79 Speech problem",
  q80  = "80 Stares blankly",
  q84  = "84 Strange behaviour",
  q111 = "111 Withdrawn"
)

# rater colours, validated with the dataviz palette validator (all pairs, light mode).
# Self gets the warm hue so it stands out against the three cool "other" raters.
# Rater is encoded with shape as well, so the figures also work in greyscale.
rater_colours = c(Mother = "#2a78d6", Father = "#1baf7a", Teacher = "#4a3aa7", Self = "#eb6834")
rater_shapes  = c(Mother = 16,        Father = 17,        Teacher = 15,        Self = 18)

response_colours = c(`Somewhat true (1)` = unname(level_colours["Low"]),
                     `Very true (2)`     = unname(level_colours["High"]))


# --- ITEM DATA IN LONG FORMAT ---
# one row per individual x rater x item, restricted to the analytic sample
analytic_sample = data_long %>%
  distinct(FISNumber, rater) %>%
  mutate(rater = as.character(rater))

item_long = data_all_items %>%
  select(FISNumber, all_of(c(items_m12, items_v12, items_t12, items_ysr14))) %>%
  pivot_longer(-FISNumber, names_to = "variable", values_to = "response") %>%
  mutate(
    item = sub("^(q[0-9]+).*$", "\\1", variable),
    rater = case_when(
      endsWith(variable, "ysr14") ~ "Self",
      endsWith(variable, "m12")   ~ "Mother",
      endsWith(variable, "v12")   ~ "Father",
      endsWith(variable, "t12")   ~ "Teacher"
    )
  ) %>%
  semi_join(analytic_sample, by = c("FISNumber", "rater")) %>%
  filter(!is.na(response)) %>%
  mutate(rater = factor(rater, levels = rater_levels),
         item_label = factor(item_labels[item], levels = item_labels))


# --- ITEM DESCRIPTIVES PER RATER ---
item_descriptives = item_long %>%
  group_by(item_label, rater) %>%
  summarise(
    n = n(),
    mean = mean(response),
    sd = sd(response),
    ci_lower = mean - 1.96 * sd / sqrt(n),
    ci_upper = mean + 1.96 * sd / sqrt(n),
    pct_0 = 100 * mean(response == 0),
    pct_1 = 100 * mean(response == 1),
    pct_2 = 100 * mean(response == 2),
    pct_endorsed = pct_1 + pct_2,
    .groups = "drop"
  )

item_means_wide = item_descriptives %>%
  select(item_label, rater, mean) %>%
  pivot_wider(names_from = rater, values_from = mean) %>%
  mutate(`Self - Mother` = Self - Mother,
         `Self - Father` = Self - Father,
         `Self - Teacher` = Self - Teacher)

item_means_wide # which items jump out?

# order items by how much higher self is than mother, so the figures read from the
# largest gap to the smallest. q80 has no self score and goes last.
item_order = item_means_wide %>%
  arrange(desc(`Self - Mother`)) %>%  # NA (q80) is sorted last by arrange
  pull(item_label) %>%
  as.character()


# --- SUM SCORES ON THE 9 SHARED ITEMS ---
# autism_score in data_long is the 10-item sum for mother/father/teacher. Subtracting
# q80 gives the 9-item sum that is comparable with self (q80 NA counted as 0, as in
# create_autism_score() in 01_create_datasets_from_raw.R)
q80_long = item_long %>%
  filter(item == "q80") %>%
  select(FISNumber, rater, q80 = response)

sum_scores = data_long %>%
  select(FISNumber, rater, autism_score) %>%
  mutate(rater = factor(as.character(rater), levels = rater_levels)) %>%
  left_join(q80_long, by = c("FISNumber", "rater")) %>%
  mutate(autism_score_9_items = autism_score - coalesce(q80, 0))

sum_score_summary = sum_scores %>%
  group_by(rater) %>%
  summarise(
    n = n(),
    items_in_sum = if_else(first(rater) == "Self", 9L, 10L),
    mean_sum_score_used_in_models = mean(autism_score),
    median_sum_score_used_in_models = median(autism_score),
    mean_sum_score_9_items = mean(autism_score_9_items),
    median_sum_score_9_items = median(autism_score_9_items),
    .groups = "drop"
  )

sum_score_summary


# --- WITHIN-PERSON DIFFERENCES: SELF vs OTHER RATERS PER ITEM ---
# For individuals rated by both self and the other rater, the mean self - other
# difference per item. Because a sum is a sum, the mean difference in the 9-item sum
# score is the sum of these item differences, so share_of_sum_difference shows how
# much of the gap each item accounts for. (Items use all available pairs, so the
# shares add up to ~100% rather than exactly.)
# NB: CIs and p-values ignore that twins are clustered in families, so treat them as
# descriptive rather than as formal tests.
item_wide = item_long %>%
  select(FISNumber, item, item_label, rater, response) %>%
  pivot_wider(names_from = rater, values_from = response)

paired_item_difference = function(other) {
  item_wide %>%
    filter(item != "q80", !is.na(Self), !is.na(.data[[other]])) %>%
    mutate(difference = Self - .data[[other]]) %>%
    group_by(item_label) %>%
    summarise(
      comparison = paste("Self vs", other),
      n_pairs = n(),
      mean_self = mean(Self),
      mean_other = mean(.data[[other]]),
      mean_difference = mean(difference),
      se = sd(difference) / sqrt(n_pairs),
      ci_lower = mean_difference - qt(0.975, n_pairs - 1) * se,
      ci_upper = mean_difference + qt(0.975, n_pairs - 1) * se,
      p_value = 2 * pt(-abs(mean_difference / se), n_pairs - 1),
      pct_self_higher = 100 * mean(difference > 0),
      pct_other_higher = 100 * mean(difference < 0),
      .groups = "drop"
    )
}

paired_differences = bind_rows(lapply(c("Mother", "Father", "Teacher"), paired_item_difference)) %>%
  group_by(comparison) %>%
  mutate(share_of_sum_difference = 100 * mean_difference / sum(mean_difference)) %>%
  ungroup() %>%
  mutate(p_fdr = p.adjust(p_value, method = "fdr"),
         comparison = factor(comparison, levels = paste("Self vs", c("Mother", "Father", "Teacher"))))

paired_differences %>% arrange(comparison, desc(mean_difference)) %>% print(n = 30)

# the same comparison on the 9-item sum score, for reference
paired_sum_difference = function(other) {
  sum_scores %>%
    select(FISNumber, rater, autism_score_9_items) %>%
    filter(rater %in% c("Self", other)) %>%
    pivot_wider(names_from = rater, values_from = autism_score_9_items) %>%
    filter(!is.na(Self), !is.na(.data[[other]])) %>%
    summarise(
      comparison = paste("Self vs", other),
      n_pairs = n(),
      mean_self_9_items = mean(Self),
      mean_other_9_items = mean(.data[[other]]),
      mean_difference = mean(Self - .data[[other]]),
      correlation = cor(Self, .data[[other]], method = "spearman")
    )
}

paired_sum_differences = bind_rows(lapply(c("Mother", "Father", "Teacher"), paired_sum_difference))
paired_sum_differences


# --- SAVE TABLES ---
round_numeric = function(df) df %>% mutate(across(where(is.numeric), ~ round(.x, 3)))

write.xlsx(
  lapply(list(
    `Item descriptives` = item_descriptives,
    `Item means wide` = item_means_wide,
    `Sum scores` = sum_score_summary,
    `Paired item differences` = paired_differences,
    `Paired sum differences` = paired_sum_differences
  ), round_numeric),
  "results/post_hoc_investigations/item_level_rater_differences.xlsx"
)


# --- FIGURE 1: ENDORSEMENT PER ITEM PER RATER ---
# One panel per item, one bar per rater. Bars show the % of reports scoring the item
# 1 or 2, split into the two response options. Most responses are 0, so a full
# 100% stacked bar would be almost entirely "Not true" - only the endorsed part is shown.
endorsement = item_descriptives %>%
  select(item_label, rater, pct_1, pct_2) %>%
  pivot_longer(c(pct_1, pct_2), names_to = "response", values_to = "pct") %>%
  mutate(
    response = factor(response, levels = c("pct_2", "pct_1"),
                      labels = c("Very true (2)", "Somewhat true (1)")),
    item_label = factor(item_label, levels = item_order),
    rater = factor(rater, levels = rev(rater_levels))  # Mother on top within a panel
  )

ggplot(endorsement, aes(x = pct, y = rater, fill = response)) +
  geom_col(width = 0.75, colour = "white", linewidth = 0.3) +
  facet_wrap(~ item_label, ncol = 2) +
  scale_fill_manual(values = response_colours, breaks = names(response_colours)) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Reports endorsing the item", y = NULL, fill = NULL,
       caption = "Items ordered by the self - mother difference. Item 80 is not part of the YSR.") +
  theme_pgs() +
  theme(panel.grid.major.y = element_blank(),
        strip.text = element_text(hjust = 0),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave("results/figures/item_endorsement_rater.png",
       width = fig_width_2col, height = 7.5, dpi = fig_dpi, bg = "white")


# --- FIGURE 2: MEAN ITEM SCORE PER RATER ---
# all raters on one row per item, so an item where self breaks away from the others
# is visible at a glance
item_means_plot = item_descriptives %>%
  mutate(item_label = factor(item_label, levels = rev(item_order)))  # largest gap on top

ggplot(item_means_plot, aes(x = item_label, y = mean, colour = rater, shape = rater)) +
  # reversed group so the dodge reads Mother -> Self from top to bottom, like the legend
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper, group = factor(rater, levels = rev(rater_levels))),
                  position = position_dodge(width = 0.7), size = 0.35, linewidth = 0.5) +
  coord_flip() +
  scale_colour_manual(values = rater_colours) +
  scale_shape_manual(values = rater_shapes) +
  labs(x = NULL, y = "Mean item score (0-2), 95% CI", colour = "Rater", shape = "Rater") +
  theme_pgs() +
  theme(panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave("results/figures/item_means_rater.png",
       width = fig_width_2col, height = 5, dpi = fig_dpi, bg = "white")


# --- FIGURE 3: WHICH ITEMS MAKE UP THE SELF - OTHER GAP ---
# within-person mean difference per item, labelled with its share of the sum score gap
paired_plot = paired_differences %>%
  mutate(item_label = factor(item_label, levels = rev(setdiff(item_order, item_labels["q80"]))))

ggplot(paired_plot, aes(x = item_label, y = mean_difference)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), colour = "grey20",
                  size = 0.3, linewidth = 0.5) +
  geom_text(aes(y = ci_upper, label = sprintf("%.0f%%", share_of_sum_difference)),
            hjust = -0.3, size = fig_base_size / .pt - 0.8, colour = "grey35") +
  coord_flip() +
  facet_wrap(~ comparison, nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  labs(x = NULL, y = "Mean within-person difference (self - other), 95% CI",
       caption = "Labels: share of the 9-item sum score difference accounted for by the item.") +
  theme_pgs() +
  theme(panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA))

ggsave("results/figures/item_self_minus_other.png",
       width = fig_width_2col, height = 4.2, dpi = fig_dpi, bg = "white")
