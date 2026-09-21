# STAT5003 Group Project: Phase 1 EDA and Planning
# Dataset: 2015 Street Tree Census - Tree Data (NYC Open Data)
#
# Proposed classification question:
# Can observable tree, site and geographic characteristics classify the
# perceived health of living NYC street trees as Good, Fair or Poor?
#
# Run this file from the project root with:
#   Rscript main.r
#
# The script creates report-ready tables and figures in eda_outputs/.

# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------

required_packages <- c(
  "readr", "dplyr", "tidyr", "ggplot2", "forcats", "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  library(scales)
})

options(dplyr.summarise.inform = FALSE)

data_file <- "2015_Street_Tree_Census_-_Tree_Data_20260912.csv"
output_dir <- "eda_outputs"
figure_dir <- file.path(output_dir, "figures")
table_dir <- file.path(output_dir, "tables")

if (!file.exists(data_file)) {
  stop("Cannot find the data file: ", data_file, call. = FALSE)
}

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

health_colours <- c(
  "Good" = "#2E7D32",
  "Fair" = "#F9A825",
  "Poor" = "#C62828"
)

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
)

save_plot <- function(plot, filename, width = 8, height = 5) {
  ggsave(
    filename = file.path(figure_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

print_table <- function(x, title) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
  print(x, n = Inf)
}

# -----------------------------------------------------------------------------
# 1. Read and audit the raw data
# -----------------------------------------------------------------------------

trees_raw <- read_csv(
  data_file,
  na = c("", "NA", "N/A", "NULL"),
  show_col_types = FALSE,
  progress = FALSE
)

data_overview <- tibble(
  metric = c(
    "Rows",
    "Columns",
    "Unique tree IDs",
    "Duplicated tree IDs",
    "Numeric columns",
    "Non-numeric columns"
  ),
  value = c(
    nrow(trees_raw),
    ncol(trees_raw),
    n_distinct(trees_raw$tree_id),
    sum(duplicated(trees_raw$tree_id)),
    sum(vapply(trees_raw, is.numeric, logical(1))),
    sum(!vapply(trees_raw, is.numeric, logical(1)))
  )
)

variable_types <- tibble(
  variable = names(trees_raw),
  imported_class = vapply(
    trees_raw,
    function(x) paste(class(x), collapse = "/"),
    character(1)
  ),
  distinct_values = vapply(trees_raw, n_distinct, integer(1), na.rm = FALSE)
)

status_distribution <- trees_raw %>%
  count(status, name = "n", sort = TRUE) %>%
  mutate(proportion = n / sum(n))

write_csv(data_overview, file.path(table_dir, "data_overview.csv"))
write_csv(variable_types, file.path(table_dir, "variable_types.csv"))
write_csv(status_distribution, file.path(table_dir, "status_distribution.csv"))

print_table(data_overview, "Raw data overview")
print_table(status_distribution, "Tree status distribution")

# -----------------------------------------------------------------------------
# 2. Missingness and data-quality patterns
# -----------------------------------------------------------------------------

missing_summary <- trees_raw %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_n"
  ) %>%
  mutate(missing_pct = missing_n / nrow(trees_raw)) %>%
  arrange(desc(missing_n))

write_csv(missing_summary, file.path(table_dir, "missing_summary.csv"))
print_table(filter(missing_summary, missing_n > 0), "Variables with missing values")

p_missing <- missing_summary %>%
  filter(missing_n > 0) %>%
  mutate(variable = fct_reorder(variable, missing_pct)) %>%
  ggplot(aes(x = missing_pct, y = variable)) +
  geom_col(fill = "#3F6B8A", width = 0.72) +
  geom_text(
    aes(label = percent(missing_pct, accuracy = 0.1)),
    hjust = -0.1,
    size = 3.4
  ) +
  scale_x_continuous(
    labels = label_percent(),
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    title = "Missingness in the raw census data",
    subtitle = "Several missing fields are structurally related to dead trees and stumps",
    x = "Missing records",
    y = NULL
  )

save_plot(p_missing, "01_missingness_overall.png", width = 8, height = 5.5)

# Health, species and condition fields were not collected for dead trees/stumps.
# This plot separates structural missingness from missingness among living trees.
missing_pattern_variables <- c(
  "health", "spc_common", "steward", "guards", "sidewalk",
  "problems", "bin", "bbl", "council district", "census tract"
)

missing_by_status <- trees_raw %>%
  group_by(status) %>%
  summarise(
    across(all_of(missing_pattern_variables), ~ mean(is.na(.x))),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -status,
    names_to = "variable",
    values_to = "missing_pct"
  )

write_csv(missing_by_status, file.path(table_dir, "missingness_by_status.csv"))

p_missing_status <- ggplot(
  missing_by_status,
  aes(x = status, y = fct_rev(variable), fill = missing_pct)
) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(
    aes(label = percent(missing_pct, accuracy = 0.1)),
    size = 3
  ) +
  scale_fill_gradient(
    low = "#F7FBFF",
    high = "#08519C",
    labels = label_percent()
  ) +
  labs(
    title = "Missingness pattern depends on tree status",
    x = "Tree status",
    y = NULL,
    fill = "Missing"
  )

save_plot(p_missing_status, "02_missingness_by_status.png", width = 8, height = 5.5)

# -----------------------------------------------------------------------------
# 3. Define the classification cohort and engineer EDA features
# -----------------------------------------------------------------------------

issue_variables <- c(
  "root_stone", "root_grate", "root_other",
  "trunk_wire", "trnk_light", "trnk_other",
  "brch_light", "brch_shoe", "brch_other"
)

cohort_flow <- tibble(
  step = c(
    "All census records",
    "Living trees",
    "Living trees with Good/Fair/Poor health"
  ),
  n = c(
    nrow(trees_raw),
    sum(trees_raw$status == "Alive", na.rm = TRUE),
    sum(trees_raw$status == "Alive" & trees_raw$health %in% c("Good", "Fair", "Poor"), na.rm = TRUE)
  )
)

write_csv(cohort_flow, file.path(table_dir, "cohort_flow.csv"))
print_table(cohort_flow, "Classification cohort flow")

trees <- trees_raw %>%
  filter(status == "Alive", health %in% c("Good", "Fair", "Poor")) %>%
  mutate(
    health = factor(health, levels = c("Good", "Fair", "Poor")),
    problem_count = rowSums(across(all_of(issue_variables), ~ .x == "Yes"), na.rm = TRUE),
    problem_group = factor(
      if_else(problem_count >= 3, "3+", as.character(problem_count)),
      levels = c("0", "1", "2", "3+")
    )
  )

class_distribution <- trees %>%
  count(health, name = "n") %>%
  mutate(proportion = n / sum(n))

write_csv(class_distribution, file.path(table_dir, "class_distribution.csv"))
print_table(class_distribution, "Health class distribution among living trees")

p_class <- ggplot(class_distribution, aes(x = health, y = n, fill = health)) +
  geom_col(width = 0.68, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(comma(n), "\n", percent(proportion, accuracy = 0.1))),
    vjust = -0.25,
    size = 3.5
  ) +
  scale_fill_manual(values = health_colours) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Living-tree health classes are strongly imbalanced",
    x = "Perceived health",
    y = "Trees"
  )

save_plot(p_class, "03_health_class_distribution.png", width = 7, height = 5)

# -----------------------------------------------------------------------------
# 4. Univariate exploration
# -----------------------------------------------------------------------------

dbh_summary <- trees %>%
  summarise(
    n = n(),
    missing_n = sum(is.na(tree_dbh)),
    mean = mean(tree_dbh, na.rm = TRUE),
    median = median(tree_dbh, na.rm = TRUE),
    p01 = quantile(tree_dbh, 0.01, na.rm = TRUE),
    p99 = quantile(tree_dbh, 0.99, na.rm = TRUE),
    minimum = min(tree_dbh, na.rm = TRUE),
    maximum = max(tree_dbh, na.rm = TRUE),
    zero_n = sum(tree_dbh == 0, na.rm = TRUE),
    over_100_n = sum(tree_dbh > 100, na.rm = TRUE)
  )

write_csv(dbh_summary, file.path(table_dir, "dbh_summary.csv"))
print_table(dbh_summary, "Diameter summary")

p_dbh <- ggplot(trees, aes(x = health, y = tree_dbh, fill = health)) +
  geom_boxplot(outlier.shape = NA, width = 0.62) +
  coord_cartesian(ylim = c(0, quantile(trees$tree_dbh, 0.999, na.rm = TRUE))) +
  scale_fill_manual(values = health_colours, guide = "none") +
  labs(
    title = "Diameter distribution by perceived health",
    subtitle = "Displayed scale ends at the 99.9th percentile to keep the central distribution visible",
    x = "Health",
    y = "Diameter at breast height (inches)"
  )

save_plot(p_dbh, "04_dbh_distribution_by_health.png", width = 8, height = 5)

top_species <- trees %>%
  count(spc_common, name = "n", sort = TRUE) %>%
  mutate(proportion = n / nrow(trees)) %>%
  slice_head(n = 15)

write_csv(top_species, file.path(table_dir, "top_species.csv"))

p_species <- top_species %>%
  mutate(spc_common = fct_reorder(spc_common, n)) %>%
  ggplot(aes(x = n, y = spc_common)) +
  geom_col(fill = "#4C956C", width = 0.7) +
  geom_text(aes(label = percent(proportion, accuracy = 0.1)), hjust = -0.1, size = 3) +
  scale_x_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.14))) +
  labs(title = "Most common living-tree species", x = "Trees", y = NULL)

save_plot(p_species, "05_top_species.png", width = 8, height = 6)

# -----------------------------------------------------------------------------
# 5. Feature-outcome relationships
# -----------------------------------------------------------------------------

health_by_borough <- trees %>%
  count(boroname, health, name = "n") %>%
  group_by(boroname) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

write_csv(health_by_borough, file.path(table_dir, "health_by_borough.csv"))

p_borough <- ggplot(health_by_borough, aes(x = boroname, y = proportion, fill = health)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = health_colours) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Health composition differs across boroughs", x = NULL, y = "Share of living trees", fill = "Health")

save_plot(p_borough, "06_health_by_borough.png", width = 8, height = 5)

poor_share_by_species <- trees %>%
  filter(!is.na(spc_common)) %>%
  group_by(spc_common) %>%
  summarise(
    n = n(),
    poor_n = sum(health == "Poor"),
    poor_share = mean(health == "Poor"),
    .groups = "drop"
  ) %>%
  filter(n >= 1000) %>%
  arrange(desc(poor_share))

write_csv(poor_share_by_species, file.path(table_dir, "poor_share_by_species.csv"))

p_poor_species <- poor_share_by_species %>%
  slice_head(n = 15) %>%
  mutate(spc_common = fct_reorder(spc_common, poor_share)) %>%
  ggplot(aes(x = poor_share, y = spc_common)) +
  geom_col(fill = "#B85C5C", width = 0.7) +
  scale_x_continuous(labels = label_percent()) +
  labs(
    title = "Poor-health share among common species",
    subtitle = "Species shown have at least 1,000 living-tree records",
    x = "Poor-health share",
    y = NULL
  )

save_plot(p_poor_species, "07_poor_share_by_species.png", width = 8, height = 6)

health_by_problem_count <- trees %>%
  count(problem_group, health, name = "n") %>%
  group_by(problem_group) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

write_csv(health_by_problem_count, file.path(table_dir, "health_by_problem_count.csv"))

p_problem <- ggplot(health_by_problem_count, aes(x = problem_group, y = proportion, fill = health)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = health_colours) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Health composition changes with recorded problem burden", x = "Number of recorded problems", y = "Share of trees", fill = "Health")

save_plot(p_problem, "08_health_by_problem_count.png", width = 8, height = 5)

health_by_user_type <- trees %>%
  filter(!is.na(user_type)) %>%
  count(user_type, health, name = "n") %>%
  group_by(user_type) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

write_csv(health_by_user_type, file.path(table_dir, "health_by_user_type.csv"))

p_user <- ggplot(health_by_user_type, aes(x = user_type, y = proportion, fill = health)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = health_colours) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Perceived health differs by census user type", x = NULL, y = "Share of trees", fill = "Health")

save_plot(p_user, "09_health_by_user_type.png", width = 8, height = 5)

# Common species and tree diameter together.
top_species_names <- top_species$spc_common[seq_len(min(8, nrow(top_species)))]

p_species_dbh <- trees %>%
  filter(spc_common %in% top_species_names) %>%
  mutate(spc_common = fct_reorder(spc_common, tree_dbh, .fun = median, na.rm = TRUE)) %>%
  ggplot(aes(x = spc_common, y = tree_dbh, fill = health)) +
  geom_boxplot(outlier.shape = NA, width = 0.7) +
  coord_cartesian(ylim = c(0, quantile(trees$tree_dbh, 0.995, na.rm = TRUE))) +
  scale_fill_manual(values = health_colours) +
  labs(
    title = "Diameter and health among common species",
    x = NULL,
    y = "Diameter at breast height (inches)",
    fill = "Health"
  ) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot(p_species_dbh, "10_species_vs_dbh.png", width = 10, height = 6)

# Spatial sample for visual exploration. Sampling avoids overplotting hundreds of thousands of points.
set.seed(5003)
spatial_sample <- trees %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  slice_sample(n = min(25000, n()))

p_spatial <- ggplot(spatial_sample, aes(x = longitude, y = latitude, colour = health)) +
  geom_point(alpha = 0.25, size = 0.45) +
  scale_colour_manual(values = health_colours) +
  coord_equal() +
  labs(
    title = "Spatial distribution of perceived tree health",
    subtitle = "Random sample of living trees for visibility",
    x = "Longitude",
    y = "Latitude",
    colour = "Health"
  )

save_plot(p_spatial, "11_spatial_health_sample.png", width = 8, height = 8)

# -----------------------------------------------------------------------------
# 6. Initial modelling feature plan
# -----------------------------------------------------------------------------

feature_plan <- tibble(
  feature = c(
    "tree_dbh", "spc_common", "steward", "guards", "sidewalk", "user_type",
    "problem_count", "boroname", "latitude", "longitude"
  ),
  role = c(
    "numeric predictor", "categorical predictor", "categorical predictor",
    "categorical predictor", "categorical predictor", "categorical predictor",
    "engineered numeric predictor", "categorical geographic predictor",
    "numeric geographic predictor", "numeric geographic predictor"
  ),
  rationale = c(
    "Tree size / maturity proxy",
    "Species-specific health patterns",
    "Stewardship context",
    "Guard condition / site context",
    "Sidewalk damage context",
    "Potential measurement/user effect to assess carefully",
    "Summary of recorded physical problems",
    "Broad geographic context",
    "Fine-scale spatial information",
    "Fine-scale spatial information"
  )
)

write_csv(feature_plan, file.path(table_dir, "feature_plan.csv"))
print_table(feature_plan, "Initial feature plan")

# Save reproducibility information.
sink(file.path(output_dir, "session_info.txt"))
print(sessionInfo())
sink()

cat("\nEDA complete. Outputs written to:", normalizePath(output_dir), "\n")
