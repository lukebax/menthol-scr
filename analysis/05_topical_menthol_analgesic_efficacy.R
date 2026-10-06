# Open menthol-scr.Rproj, then run this whole script.
# It describes confirmed reported comparisons and descriptive case-report pain
# findings, availability within the counted inventory, and registration.
# Figure 5 and the in-text quantities are saved in the Section 05 output folder.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})

# Category lists have labelled lines in the dictionary; explanatory prose is not parsed.
dictionary <- read_csv(
  file.path("data", "data_dictionary.csv"),
  col_types = cols(.default = col_character()),
  na = character(), show_col_types = FALSE
)
stop_for_problems(dictionary)
dictionary_values <- function(field, label = "Values") {
  if (!all(c("column_name", "format_or_allowed_values") %in% names(dictionary))) {
    stop("The dictionary is missing its field-name or allowed-values column.")
  }
  definition <- dictionary$format_or_allowed_values[dictionary$column_name == field]
  if (length(definition) != 1L || is.na(definition)) {
    stop("The dictionary must contain exactly one row for ", field, ".")
  }
  prefix <- paste0(label, ": ")
  lines <- strsplit(definition, "\n", fixed = TRUE)[[1]]
  list_line <- lines[startsWith(lines, prefix)]
  if (length(list_line) != 1L) {
    stop("The dictionary needs one '", label, "' list for ", field, ".")
  }
  values <- strsplit(substring(list_line, nchar(prefix) + 1L), "; ", fixed = TRUE)[[1]]
  if (!length(values) || any(!nzchar(values)) || any(values != trimws(values)) ||
      anyDuplicated(values) || any(grepl(";", values, fixed = TRUE))) {
    stop("The dictionary contains an invalid or repeated ", label, " entry for ", field, ".")
  }
  values
}

output_dir <- file.path("outputs", "05_topical_menthol_analgesic_efficacy")
count_fields <- c(
  "efficacy_result_count", "efficacy_effect_estimate_count",
  "efficacy_effect_estimate_with_precision_count"
)
required_columns <- c(
  "record_id", "first_author", "publication_year", "study_design",
  count_fields, "study_registration"
)
studies <- read_csv(
  file.path("data", "data_extraction_form.csv"),
  col_types = cols(.default = col_character()),
  na = character(), show_col_types = FALSE
)
stop_for_problems(studies)
if (!all(required_columns %in% names(studies))) {
  stop("The extraction form is missing a column needed for Section 05.")
}
studies <- studies |> select(all_of(required_columns))

# Check the few input rules that directly affect these summaries.
if (nrow(studies) == 0L || anyDuplicated(studies$record_id)) {
  stop("The extraction form must contain at least one study row and unique record_id values.")
}
if (any(is.na(studies)) || any(trimws(as.matrix(studies)) == "")) {
  stop("The extraction form contains a blank value.")
}
if (any(!grepl("^[0-9]{4}$", studies$publication_year)) ||
    any(studies$first_author %in% c("NI", "N/A"))) {
  stop("Each study needs a first-author name and a four-digit publication year.")
}
if (any(!grepl("^R([0-9]{3}|[1-9][0-9]{3,})$", studies$record_id)) ||
    any(studies$record_id == "R000")) {
  stop("record_id must use R001-style study identifiers.")
}
if (any(!grepl("^([0-9]+|NI)$", studies$efficacy_result_count)) ||
    any(!grepl("^([0-9]+|NI|N/A)$", studies$efficacy_effect_estimate_count)) ||
    any(!grepl("^([0-9]+|NI|N/A)$", studies$efficacy_effect_estimate_with_precision_count))) {
  stop("The efficacy counts must follow the integer, NI, and N/A dictionary rules.")
}
if (any(!studies$study_design %in% dictionary_values("study_design"))) {
  stop("study_design contains an unexpected value.")
}
case_report <- studies$study_design == "Case report"
if (any((studies$efficacy_effect_estimate_count == "N/A") != case_report) ||
    any((studies$efficacy_effect_estimate_with_precision_count == "N/A") != case_report)) {
  stop("Use paired N/A for descriptive case reports, independently of their first count.")
}
allowed_registration <- dictionary_values("study_registration")
registration_levels <- c(
  "Prospective", "Retrospective",
  "No sufficiently matched public registry record located", "Unresolved"
)
if (!setequal(allowed_registration, c(registration_levels[1:3], "NI"))) {
  stop("The registration categories have changed; update the Section 05 summaries and legend.")
}
if (any(!studies$study_registration %in% allowed_registration)) {
  stop("study_registration contains an unexpected value.")
}

studies <- studies |>
  mutate(
    across(all_of(count_fields), ~ as.double(na_if(na_if(.x, "NI"), "N/A"))),
    study_registration = if_else(study_registration == "NI", "Unresolved", study_registration),
    study_label = paste0(first_author, " (", publication_year, ") [", record_id, "]")
  )
if (any(!is.finite(as.matrix(studies[count_fields])) &
        !is.na(as.matrix(studies[count_fields])))) {
  stop("Efficacy counts must be finite whole numbers or permitted missingness values.")
}
if (any(studies$efficacy_effect_estimate_count > studies$efficacy_result_count, na.rm = TRUE) ||
    any(studies$efficacy_effect_estimate_with_precision_count >
          studies$efficacy_effect_estimate_count, na.rm = TRUE) ||
    any(studies$efficacy_effect_estimate_with_precision_count >
          studies$efficacy_result_count, na.rm = TRUE)) {
  stop("Counts must satisfy: with precision <= with estimate <= all results.")
}
non_cases <- studies |> filter(study_design != "Case report")
if (any(
  non_cases$efficacy_result_count == 0 &
    (is.na(non_cases$efficacy_effect_estimate_count) |
       non_cases$efficacy_effect_estimate_count != 0 |
       is.na(non_cases$efficacy_effect_estimate_with_precision_count) |
       non_cases$efficacy_effect_estimate_with_precision_count != 0), na.rm = TRUE
) || any(
  non_cases$efficacy_effect_estimate_count == 0 &
    (is.na(non_cases$efficacy_effect_estimate_with_precision_count) |
       non_cases$efficacy_effect_estimate_with_precision_count != 0), na.rm = TRUE
)) {
  stop("Zero comparisons require zero availability; zero estimates require zero precision.")
}
# NI does not cascade. Confirmed availability nevertheless establishes an
# available estimate and a counted comparison, so their counts cannot be NI.
if (any(
  is.na(non_cases$efficacy_result_count) &
    (non_cases$efficacy_effect_estimate_count > 0 |
       non_cases$efficacy_effect_estimate_with_precision_count > 0), na.rm = TRUE
) || any(
  is.na(non_cases$efficacy_effect_estimate_count) &
    non_cases$efficacy_effect_estimate_with_precision_count > 0, na.rm = TRUE
)) {
  stop("A confirmed available estimate or precision measure requires a positive count of its eligible comparisons.")
}
result_counts <- studies$efficacy_result_count[!is.na(studies$efficacy_result_count)]

# Coverage describes only the confirmed counted comparison inventory. A zero
# denominator is undefined, not All or None; case reports remain N/A.
coverage_levels <- c("All", "Some", "None", "No counted comparisons", "N/A", "Unresolved")
coverage_counts <- studies |>
  select(record_id, study_design, all_of(count_fields)) |>
  pivot_longer(all_of(count_fields[2:3]), names_to = "reporting_field", values_to = "available") |>
  mutate(
    reporting_field = factor(reporting_field, levels = count_fields[2:3]),
    coverage = case_when(
      study_design == "Case report" ~ "N/A",
      efficacy_result_count == 0 ~ "No counted comparisons",
      is.na(efficacy_result_count) | is.na(available) ~ "Unresolved",
      available == efficacy_result_count ~ "All",
      available == 0 ~ "None",
      TRUE ~ "Some"
    ),
    coverage = factor(coverage, levels = coverage_levels)
  ) |>
  count(reporting_field, coverage, name = "studies", .drop = FALSE)

registration_counts <- studies |>
  mutate(study_registration = factor(study_registration, levels = registration_levels)) |>
  count(study_registration, name = "studies", .drop = FALSE)
registration_labels <- studies |>
  filter(study_registration %in% c("Prospective", "Retrospective")) |>
  mutate(study_registration = factor(study_registration, levels = c("Prospective", "Retrospective"))) |>
  arrange(first_author, publication_year, record_id, .locale = "en") |>
  group_by(study_registration, .drop = FALSE) |>
  summarise(labels = if (n() == 0L) "None" else paste(study_label, collapse = "; "), .groups = "drop")

in_text_results <- bind_rows(
  tibble(
    result = c(
      "included_studies", "studies_with_unresolved_efficacy_result_count",
      "studies_with_multiple_efficacy_results", "median_efficacy_results_per_study",
      "mean_efficacy_results_per_study", "efficacy_results_per_study_iqr",
      "efficacy_results_per_study_range"
    ),
    value = as.character(c(
      nrow(studies), sum(is.na(studies$efficacy_result_count)), sum(result_counts > 1),
      if (length(result_counts)) median(result_counts) else "NI",
      if (length(result_counts)) sprintf("%.1f", mean(result_counts)) else "NI",
      if (length(result_counts)) {
        paste(quantile(result_counts, c(0.25, 0.75), names = FALSE), collapse = "–")
      } else "NI",
      if (length(result_counts)) paste(range(result_counts), collapse = "–") else "NI"
    ))
  ),
  coverage_counts |>
    transmute(
      result = paste0(
        if_else(reporting_field == count_fields[2], "effect_estimate", "precision"),
        "_coverage_",
        if_else(coverage == "N/A", "not_applicable",
                gsub(" ", "_", tolower(as.character(coverage)))),
        "_studies"
      ),
      value = as.character(studies)
    ),
  tibble(
    result = c(
      "prospectively_registered_studies", "retrospectively_registered_studies",
      "studies_with_no_sufficiently_matched_public_registry_record",
      "studies_with_unresolved_registration", "prospectively_registered_study_labels",
      "retrospectively_registered_study_labels"
    ),
    value = c(as.character(registration_counts$studies), registration_labels$labels)
  )
)

# A uses the natural numerical order; B uses coverage order; C uses frequency.
study_count_breaks <- function(limits) {
  breaks <- unique(round(scales::breaks_pretty(n = 5)(limits)))
  breaks[breaks >= 0]
}
coverage_colours <- c(
  All = "#74AF8D", Some = "#EEC979", None = "#D8806E",
  `No counted comparisons` = "#8BA3AA", `N/A` = "#3E5571", Unresolved = "#B7B7B7"
)
figure_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.tag = element_text(face = "bold"),
    plot.margin = margin(8, 12, 8, 8)
  )
frequency_counts <- studies |>
  filter(!is.na(efficacy_result_count)) |>
  count(efficacy_result_count, name = "studies")
panel_a <- ggplot(frequency_counts, aes(efficacy_result_count, studies)) +
  geom_col(fill = coverage_colours["All"], width = 0.7) +
  geom_text(aes(label = studies), vjust = -0.4, size = 3.5) +
  scale_x_continuous(breaks = frequency_counts$efficacy_result_count) +
  scale_y_continuous(
    breaks = scales::breaks_width(1), expand = expansion(mult = c(0, 0.15))
  ) +
  labs(x = "Confirmed reported comparisons or\ncase-report pain findings per study", y = "Number of studies") +
  figure_theme + theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )
if (length(result_counts) == 0L) {
  panel_a <- ggplot() +
    annotate("text", x = 0, y = 0, label = "No resolved counts of comparisons or case-report pain findings") +
    theme_void(base_size = 11) + theme(plot.margin = margin(8, 12, 8, 8))
}

coverage_plot_data <- coverage_counts |>
  filter(coverage != "Unresolved" | any(studies[coverage == "Unresolved"] > 0)) |>
  filter(coverage != "No counted comparisons" | any(studies[coverage == "No counted comparisons"] > 0)) |>
  mutate(
    coverage = factor(coverage, levels = rev(coverage_levels)),
    reporting = factor(
      reporting_field, levels = count_fields[2:3],
      labels = c("Effect estimate", "Effect estimate with precision")
    )
  )
panel_b <- ggplot(coverage_plot_data, aes(studies, coverage, fill = coverage, alpha = reporting)) +
  geom_col(position = position_dodge(width = 0.8, reverse = TRUE), width = 0.65, orientation = "y") +
  geom_text(
    aes(label = studies, group = reporting), position = position_dodge(width = 0.8, reverse = TRUE),
    hjust = -0.3, size = 3.5, alpha = 1
  ) +
  scale_fill_manual(values = coverage_colours, guide = "none") +
  scale_alpha_manual(values = c(1, 0.5)) +
  scale_x_continuous(breaks = study_count_breaks, expand = expansion(mult = c(0, 0.2))) +
  scale_y_discrete(labels = function(x) ifelse(
    x == "No counted comparisons", "No counted\ncomparisons", x
  )) +
  labs(title = "Availability within counted comparisons", x = "Number of studies", y = NULL, alpha = NULL) +
  figure_theme +
  theme(panel.grid.major.y = element_blank(), legend.position = "bottom") +
  guides(alpha = guide_legend(ncol = 1, override.aes = list(fill = "#555555")))

registration_plot_data <- registration_counts |>
  filter(study_registration != "Unresolved" | studies > 0) |>
  mutate(study_registration = as.character(study_registration)) |>
  arrange(desc(studies), study_registration, .locale = "en") |>
  mutate(study_registration = factor(study_registration, levels = rev(study_registration)))
panel_c <- ggplot(registration_plot_data, aes(studies, study_registration, fill = study_registration)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = studies), hjust = -0.3, size = 3.5) +
  scale_fill_manual(values = setNames(
    unname(coverage_colours[c("All", "Some", "None", "Unresolved")]), registration_levels
  ), guide = "none") +
  scale_y_discrete(labels = c(
    Prospective = "Prospective", Retrospective = "Retrospective",
    `No sufficiently matched public registry record located` = "No sufficiently matched\npublic registry record\nlocated",
    Unresolved = "Unresolved"
  )) +
  scale_x_continuous(breaks = study_count_breaks, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Study registration", x = "Number of studies", y = NULL) +
  figure_theme + theme(panel.grid.major.y = element_blank())

# Keep one combined figure, with A above equally sized B and C panels.
figure_5 <- (panel_a / (panel_b | panel_c)) +
  plot_layout(heights = c(1, 1.2)) +
  plot_annotation(
    tag_levels = "a", tag_suffix = ".",
    caption = paste0(
      "Counts are confirmed minima; availability describes only the counted comparison inventory.\n",
      "Zero comparisons have no coverage denominator; N/A denotes descriptive case reports."
    ),
    theme = theme(plot.caption = element_text(hjust = 0, size = 9))
  )
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(in_text_results, file.path(output_dir, "in_text_results.csv"))
ggsave(
  file.path(output_dir, "figure_5_topical_menthol_analgesic_efficacy.png"), figure_5,
  device = ragg::agg_png, width = 10, height = 7.5, units = "in", dpi = 300,
  bg = "white"
)
