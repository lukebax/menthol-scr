# Open menthol-scr.Rproj, then run this whole script.
# It creates Figure 2 and the in-text results for Section 02.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(patchwork)
})

input_path <- file.path("data", "data_extraction_form.csv")
output_dir <- file.path("outputs", "02_study_designs")

studies <- read_csv(
  input_path,
  col_types = cols(.default = col_character()),
  na = character(),
  show_col_types = FALSE
)

# Keep checks short and tied to the requested outputs.
required_columns <- c(
  "record_id", "first_author", "publication_year", "study_design",
  "pain_contexts", "pain_measurements", "participant_sex"
)
if (!all(required_columns %in% names(studies))) {
  stop("The extraction form is missing a column needed for Section 02.")
}
if (nrow(studies) == 0L || anyDuplicated(studies$record_id)) {
  stop("The extraction form must contain at least one study row with unique record IDs.")
}
if (any(is.na(studies)) || any(studies == "")) {
  stop("The extraction form contains a blank value.")
}

allowed_designs <- c(
  "Parallel groups", "Cross-over", "Pre-post", "Single arm", "Case report", "NI"
)
sex_levels <- c("Female", "Male", "Mixed", "NI")
sex_legend_order <- c("Mixed", "Male", "Female", "NI")
if (any(!studies$study_design %in% allowed_designs)) {
  stop("study_design contains an unexpected value.")
}
if (any(!studies$participant_sex %in% sex_levels)) {
  stop("participant_sex contains an unexpected value.")
}

# Read controlled labels from the dictionary so accepted additions stay in sync.
dictionary <- read_csv(
  file.path("data", "data_dictionary.csv"),
  col_types = cols(.default = col_character()),
  na = character(), show_col_types = FALSE
)
context_format <- dictionary$format_or_allowed_values[dictionary$column_name == "pain_contexts"]
context_vocabulary <- str_match(context_format, "contexts: ([^;]+);")[, 2]
if (length(context_vocabulary) != 1L || is.na(context_vocabulary) || !nzchar(context_vocabulary)) {
  stop("Cannot read the pain-context vocabulary from the dictionary; check its list wording.")
}
allowed_contexts <- str_split(context_vocabulary, ", ")[[1]]
measurement_format <- dictionary$format_or_allowed_values[dictionary$column_name == "pain_measurements"]
method_vocabulary <- str_match(
  measurement_format,
  "store the method labels (.+), without the parenthetical explanations\\."
)[, 2]
if (length(method_vocabulary) != 1L || is.na(method_vocabulary) || !nzchar(method_vocabulary)) {
  stop("Cannot read the measurement-method vocabulary from the dictionary; check its list wording.")
}
allowed_methods <- method_vocabulary |>
  str_remove_all(" \\([^()]*\\)") |>
  str_replace(", or ", ", ") |>
  str_split(", ") |>
  unlist(use.names = FALSE)

context_pairs <- studies |>
  select(record_id, participant_sex, pain_contexts) |>
  separate_longer_delim(pain_contexts, delim = "; ") |>
  rename(context_pair = pain_contexts)

measurement_pairs <- studies |>
  select(record_id, pain_measurements) |>
  separate_longer_delim(pain_measurements, delim = "; ") |>
  rename(measurement_pair = pain_measurements)

if (
  any(str_detect(studies$pain_contexts, "(^|; )NI(; |$)") & studies$pain_contexts != "NI") ||
  any(context_pairs$context_pair != "NI" &
        !str_detect(context_pairs$context_pair, "^(Induced|Pre-existing): [^:;]+$")) ||
  any(str_ends(context_pairs$context_pair, ": NI")) ||
  anyDuplicated(paste(context_pairs$record_id, context_pairs$context_pair, sep = "\r"))
) {
  stop("pain_contexts contains an invalid or repeated pair.")
}
if (
  any(
    str_detect(studies$pain_measurements, "(^|; )NI(; |$)") &
      studies$pain_measurements != "NI"
  ) ||
  any(measurement_pairs$measurement_pair != "NI" &
        !str_detect(measurement_pairs$measurement_pair, "^[^:;]+: [^:;]+$")) ||
  any(str_starts(measurement_pairs$measurement_pair, "NI: ")) ||
  anyDuplicated(paste(measurement_pairs$record_id, measurement_pairs$measurement_pair, sep = "\r"))
) {
  stop("pain_measurements contains an invalid or repeated pair.")
}

if (any(context_pairs$context_pair != "NI" &
        !str_remove(context_pairs$context_pair, "^[^:]+: ") %in% allowed_contexts)) {
  stop("pain_contexts contains a label absent from the dictionary.")
}
if (any(measurement_pairs$measurement_pair != "NI" &
        !str_remove(measurement_pairs$measurement_pair, "^[^:]+: ") %in% allowed_methods)) {
  stop("pain_measurements contains a label absent from the dictionary.")
}

# Split the paired cells in memory. Method NI is retained here for type counts.
contexts <- context_pairs |>
  filter(context_pair != "NI") |>
  separate_wider_delim(
    context_pair,
    delim = ": ",
    names = c("pain_origin", "pain_context")
  )

measurements <- measurement_pairs |>
  filter(measurement_pair != "NI") |>
  separate_wider_delim(
    measurement_pair,
    delim = ": ",
    names = c("measurement_type", "measurement_method")
  )

method_memberships <- measurements |>
  filter(measurement_method != "NI") |>
  distinct(record_id, measurement_method)

context_memberships <- contexts |>
  distinct(record_id, pain_origin, pain_context, participant_sex)

# Derive the summaries once so the figure and text use the same counts.
design_counts <- studies |>
  count(study_design, name = "studies") |>
  arrange(desc(studies), study_design, .locale = "en") |>
  mutate(
    study_design = factor(
      study_design,
      levels = rev(study_design)
    )
  )

method_counts <- method_memberships |>
  count(measurement_method, name = "studies") |>
  arrange(desc(studies), measurement_method, .locale = "en") |>
  mutate(
    measurement_method = factor(
      measurement_method,
      levels = rev(measurement_method)
    )
  )

context_by_sex <- context_memberships |>
  count(pain_origin, pain_context, participant_sex, name = "studies")

context_totals <- context_memberships |>
  count(pain_origin, pain_context, name = "studies")

induced_order <- context_totals |>
  filter(pain_origin == "Induced") |>
  arrange(desc(studies), pain_context, .locale = "en") |>
  pull(pain_context)

pre_existing_order <- context_totals |>
  filter(pain_origin == "Pre-existing") |>
  arrange(desc(studies), pain_context, .locale = "en") |>
  pull(pain_context)

induced_counts <- context_by_sex |>
  filter(pain_origin == "Induced") |>
  mutate(
    pain_context = factor(pain_context, levels = rev(induced_order)),
    participant_sex = factor(participant_sex, levels = sex_levels)
  )

pre_existing_counts <- context_by_sex |>
  filter(pain_origin == "Pre-existing") |>
  mutate(
    pain_context = factor(pain_context, levels = rev(pre_existing_order)),
    participant_sex = factor(participant_sex, levels = sex_levels)
  )

vas_ids <- method_memberships |>
  filter(measurement_method == "VAS") |>
  pull(record_id) |>
  unique()

nrs_ids <- method_memberships |>
  filter(measurement_method == "NRS") |>
  pull(record_id) |>
  unique()

intensity_ids <- measurements |>
  filter(measurement_type == "Intensity") |>
  pull(record_id) |>
  unique()

threshold_ids <- measurements |>
  filter(measurement_type == "Threshold") |>
  pull(record_id) |>
  unique()

threshold_labels <- studies |>
  filter(record_id %in% threshold_ids) |>
  arrange(first_author, publication_year, record_id, .locale = "en") |>
  transmute(label = paste0(first_author, " (", publication_year, ")")) |>
  pull(label) |>
  str_c(collapse = "; ")

if (length(threshold_ids) == 0L) threshold_labels <- "None"

induced_totals <- context_totals |>
  filter(pain_origin == "Induced")
pre_existing_totals <- context_totals |>
  filter(pain_origin == "Pre-existing")

most_common_induced <- induced_totals |>
  slice_max(studies, n = 1, with_ties = TRUE) |>
  arrange(pain_context, .locale = "en")
most_common_pre_existing <- pre_existing_totals |>
  slice_max(studies, n = 1, with_ties = TRUE) |>
  arrange(pain_context, .locale = "en")

in_text_results <- tibble(
  result = c(
    "included_studies",
    "parallel_group_studies",
    "cross_over_studies",
    "pre_post_studies",
    "single_arm_studies",
    "case_report_studies",
    "parallel_cross_over_or_pre_post_studies",
    "distinct_pain_measurement_methods",
    "vas_studies",
    "nrs_studies",
    "vas_or_nrs_studies",
    "vas_or_nrs_percent",
    "intensity_assessment_studies",
    "threshold_assessment_studies",
    "intensity_and_threshold_studies",
    "threshold_assessment_study_labels",
    "induced_pain_studies",
    "pre_existing_pain_studies",
    "most_common_induced_context",
    "most_common_induced_context_studies",
    "most_common_pre_existing_context",
    "most_common_pre_existing_context_studies"
  ),
  value = as.character(c(
    nrow(studies),
    sum(studies$study_design == "Parallel groups"),
    sum(studies$study_design == "Cross-over"),
    sum(studies$study_design == "Pre-post"),
    sum(studies$study_design == "Single arm"),
    sum(studies$study_design == "Case report"),
    sum(studies$study_design %in% c("Parallel groups", "Cross-over", "Pre-post")),
    n_distinct(method_memberships$measurement_method),
    length(vas_ids),
    length(nrs_ids),
    length(union(vas_ids, nrs_ids)),
    round(100 * length(union(vas_ids, nrs_ids)) / nrow(studies)),
    length(intensity_ids),
    length(threshold_ids),
    length(intersect(intensity_ids, threshold_ids)),
    threshold_labels,
    n_distinct(context_memberships$record_id[context_memberships$pain_origin == "Induced"]),
    n_distinct(context_memberships$record_id[context_memberships$pain_origin == "Pre-existing"]),
    if (nrow(most_common_induced)) str_c(most_common_induced$pain_context, collapse = "; ") else "N/A",
    if (nrow(most_common_induced)) first(most_common_induced$studies) else "N/A",
    if (nrow(most_common_pre_existing)) str_c(most_common_pre_existing$pain_context, collapse = "; ") else "N/A",
    if (nrow(most_common_pre_existing)) first(most_common_pre_existing$studies) else "N/A"
  ))
)

if (
  nrow(in_text_results) != 22L ||
  anyDuplicated(in_text_results$result) ||
  any(is.na(in_text_results$value)) ||
  any(in_text_results$value == "")
) {
  stop("The Section 02 in-text output is incomplete.")
}

# Build the four manuscript panels and collect one shared sex legend.
study_count_breaks <- function(limits) {
  breaks <- unique(round(scales::breaks_pretty(n = 5)(limits)))
  breaks[breaks >= 0]
}

panel_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(face = "bold", size = 12)
  )

sex_colours <- c(
  Female = "#D8806E",
  Male = "#7691A1",
  Mixed = "#3E5571",
  NI = "#999999"
)
present_sexes <- sex_legend_order[
  sex_legend_order %in% context_memberships$participant_sex
]

panel_a <- ggplot(design_counts, aes(x = studies, y = study_design)) +
  geom_col(width = 0.7, fill = "#74AF8D") +
  geom_text(aes(label = studies), hjust = -0.25, size = 3.5) +
  scale_x_continuous(breaks = study_count_breaks, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Study designs", x = "Number of studies") +
  panel_theme

panel_b <- ggplot(method_counts, aes(x = studies, y = measurement_method)) +
  geom_col(width = 0.7, fill = "#74AF8D") +
  geom_text(aes(label = studies), hjust = -0.25, size = 3.5) +
  scale_x_continuous(breaks = study_count_breaks, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Pain measurement methods", x = "Number of studies") +
  panel_theme
if (nrow(method_counts) == 0L) {
  panel_b <- ggplot() +
    annotate("text", x = 0, y = 0, label = "No identified pain measurement methods") +
    labs(title = "Pain measurement methods") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 12))
}

panel_c <- ggplot(
  induced_counts,
  aes(x = studies, y = pain_context, fill = participant_sex)
) +
  geom_col(width = 0.7, show.legend = TRUE) +
  scale_fill_manual(
    values = sex_colours,
    limits = present_sexes,
    breaks = present_sexes,
    drop = FALSE,
    name = "Participant sex"
  ) +
  scale_x_continuous(breaks = study_count_breaks, expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Experimentally induced pain", x = "Number of studies") +
  guides(fill = if (nrow(pre_existing_counts) == 0L) "legend" else "none") +
  panel_theme
if (nrow(induced_counts) == 0L) {
  panel_c <- ggplot() +
    annotate("text", x = 0, y = 0, label = "No identified induced pain contexts") +
    labs(title = "Experimentally induced pain") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 12))
}

panel_d <- ggplot(
  pre_existing_counts,
  aes(x = studies, y = pain_context, fill = participant_sex)
) +
  geom_col(width = 0.7, show.legend = TRUE) +
  scale_fill_manual(
    values = sex_colours,
    limits = present_sexes,
    breaks = present_sexes,
    drop = FALSE,
    name = "Participant sex"
  ) +
  scale_x_continuous(breaks = study_count_breaks, expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Pre-existing pain", x = "Number of studies") +
  panel_theme
if (nrow(pre_existing_counts) == 0L) {
  panel_d <- ggplot() +
    annotate("text", x = 0, y = 0, label = "No identified pre-existing pain contexts") +
    labs(title = "Pre-existing pain") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 12))
}

figure_2 <- wrap_plots(
  panel_a, panel_b, panel_c, panel_d,
  ncol = 2,
  guides = "collect"
) +
  plot_annotation(tag_levels = "a", tag_suffix = ".") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(face = "bold")
  )

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(in_text_results, file.path(output_dir, "in_text_results.csv"))
ggsave(
  filename = file.path(output_dir, "figure_2_study_designs.png"),
  plot = figure_2,
  device = ragg::agg_png,
  width = 12,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

message("Section 02 outputs written to: ", output_dir)
