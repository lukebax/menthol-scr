# Open menthol-scr.Rproj, then run this whole script.
# It creates Figure 4 and the in-text results for Section 04.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(patchwork)
  library(ggrain)
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

input_path <- file.path("data", "data_extraction_form.csv")
output_dir <- file.path("outputs", "04_interventions_cointerventions_comparators")

studies <- read_csv(
  input_path,
  col_types = cols(.default = col_character()),
  na = character(),
  show_col_types = FALSE
)
stop_for_problems(studies)

# Check the inputs needed by this subsection.
required_columns <- c(
  "record_id", "first_author", "publication_year", "pharmaceutical_form",
  "application_site", "co_intervention", "comparator",
  "menthol_concentration"
)
if (!all(required_columns %in% names(studies))) {
  stop("The extraction form is missing a column needed for Section 04.")
}
if (nrow(studies) == 0L || anyDuplicated(studies$record_id)) {
  stop("The extraction form must contain at least one study row and unique record_id values.")
}
if (any(!grepl("^R([0-9]{3}|[1-9][0-9]{3,})$", studies$record_id)) ||
    any(studies$record_id == "R000")) {
  stop("record_id must use R001-style study identifiers.")
}
if (any(is.na(studies)) || any(trimws(as.matrix(studies)) == "")) {
  stop("The extraction form contains a blank value.")
}
if (any(!grepl("^[0-9]{4}$", studies$publication_year)) ||
    any(studies$first_author %in% c("NI", "N/A"))) {
  stop("Each study needs a first-author name and a four-digit publication year.")
}

studies <- studies |>
  mutate(
    study_label = paste0(
      first_author, " (", publication_year, ") [", record_id, "]"
    )
  )

# Split the plural study fields only on the documented literal delimiter.
form_entries <- studies |>
  select(record_id, pharmaceutical_form) |>
  separate_longer_delim(pharmaceutical_form, delim = "; ") |>
  rename(entry = pharmaceutical_form)

site_entries <- studies |>
  select(record_id, application_site) |>
  separate_longer_delim(application_site, delim = "; ") |>
  rename(entry = application_site)

co_intervention_entries <- studies |>
  select(record_id, co_intervention) |>
  separate_longer_delim(co_intervention, delim = "; ") |>
  rename(entry = co_intervention)

comparator_entries <- studies |>
  select(record_id, comparator) |>
  separate_longer_delim(comparator, delim = "; ") |>
  rename(entry = comparator)

concentration_entries <- studies |>
  select(record_id, menthol_concentration) |>
  separate_longer_delim(menthol_concentration, delim = "; ") |>
  rename(entry = menthol_concentration)

plural_entries <- list(
  pharmaceutical_form = form_entries,
  application_site = site_entries,
  co_intervention = co_intervention_entries,
  comparator = comparator_entries,
  menthol_concentration = concentration_entries
)
for (field_name in names(plural_entries)) {
  field_entries <- plural_entries[[field_name]]
  if (
    any(field_entries$entry == "") ||
      any(field_entries$entry != str_trim(field_entries$entry)) ||
      any(str_detect(field_entries$entry, fixed(";"))) ||
      anyDuplicated(paste(field_entries$record_id, field_entries$entry, sep = "\r"))
  ) {
    stop(field_name, " needs distinct non-empty entries separated by exactly '; ', without surrounding spaces.")
  }
}

for (field_name in c("pharmaceutical_form", "application_site")) {
  if (any(str_detect(studies[[field_name]], "(^|; )NI(; |$)") &
          studies[[field_name]] != "NI")) {
    stop(field_name, " needs scoped NI for an unresolved component alongside known entries.")
  }
}

# Validate the compact field grammars before deriving any counts.
allowed_forms <- dictionary_values("pharmaceutical_form")
allowed_purposes <- dictionary_values("menthol_concentration", "Purpose labels")
allowed_bases <- dictionary_values("menthol_concentration", "Bases")
unresolved_pattern <- "^NI(?: \\([^;()]+\\))?$"
if (any(!form_entries$entry %in% allowed_forms &
        !str_detect(form_entries$entry, unresolved_pattern))) {
  stop("pharmaceutical_form contains an unexpected value.")
}
if (any(site_entries$entry == "N/A")) {
  stop("application_site cannot use N/A.")
}
if (
  any(str_starts(site_entries$entry, "NI") &
        !str_detect(site_entries$entry, unresolved_pattern)) ||
  any(str_detect(studies$co_intervention, "(^|; )N/A(; |$)") &
        studies$co_intervention != "N/A") ||
    any(str_starts(co_intervention_entries$entry, "NI") &
          !str_detect(co_intervention_entries$entry, unresolved_pattern))
) {
  stop("application_site or co_intervention contains invalid missingness.")
}

# Points, ranges, and product percentages retain their reported qualifications.
# Only point percentages explicitly describing menthol enter the concentration plot.
point_pattern <- paste0(
  "^(approximately )?([0-9]+(?:\\.[0-9]+)?)% menthol ",
  "\\(([^(),]+)(?:, (.+))?\\)$"
)
range_pattern <- paste0(
  "^(approximately )?([0-9]+(?:\\.[0-9]+)?)-([0-9]+(?:\\.[0-9]+)?)% menthol ",
  "\\(([^(),]+)(?:, (.+))?\\)$"
)
product_pattern <- paste0(
  "^(approximately )?([0-9]+(?:\\.[0-9]+)?)(?:-([0-9]+(?:\\.[0-9]+)?))?% ",
  "([^;\\[\\]]+) \\((?:([^(),]+), )?",
  "menthol concentration not established(?:, (.+))?\\)$"
)
purpose_pattern <- " \\[Purpose: ([^]]+)\\]$"
purpose_parts <- str_match(concentration_entries$entry, purpose_pattern)

concentration_entries <- concentration_entries |>
  mutate(
    concentration_entry = str_remove(entry, purpose_pattern),
    concentration_purpose = purpose_parts[, 2],
    is_point = str_detect(concentration_entry, point_pattern),
    is_range = str_detect(concentration_entry, range_pattern),
    is_product_percentage = str_detect(concentration_entry, product_pattern),
    is_unresolved = str_detect(concentration_entry, unresolved_pattern)
  )

if (any(rowSums(select(
  concentration_entries,
  is_point, is_range, is_product_percentage, is_unresolved
)) != 1L)) {
  stop("menthol_concentration contains an invalid entry.")
}
if (any(
  concentration_entries$is_unresolved !=
    is.na(concentration_entries$concentration_purpose)
)) {
  stop("Each reported concentration needs one purpose tag; bare or scoped NI must remain untagged.")
}
if (anyDuplicated(paste(
  concentration_entries$record_id,
  concentration_entries$concentration_entry,
  sep = "\r"
))) {
  stop("A concentration entry is repeated within a study; combine its reported purposes in one tag.")
}

range_parts <- str_match(concentration_entries$concentration_entry, range_pattern)
point_parts <- str_match(concentration_entries$concentration_entry, point_pattern)
product_parts <- str_match(concentration_entries$concentration_entry, product_pattern)
reported_bases <- c(point_parts[, 4], range_parts[, 5], product_parts[, 6])
reported_qualifications <- c(point_parts[, 5], range_parts[, 6], product_parts[, 7])
reported_labels <- c(reported_qualifications, product_parts[, 5])
reported_labels <- reported_labels[!is.na(reported_labels)]
reported_percentages <- as.double(c(
  point_parts[, 3], range_parts[, 3], range_parts[, 4],
  product_parts[, 3], product_parts[, 4]
))
if (any(!is.finite(reported_percentages[!is.na(reported_percentages)]))) {
  stop("Concentration percentages must be finite numbers.")
}
if (any(!nzchar(str_trim(reported_labels))) ||
    any(reported_labels != str_trim(reported_labels))) {
  stop("A concentration qualification or product label is empty or has surrounding spaces.")
}
if (any(!reported_bases[!is.na(reported_bases)] %in% allowed_bases) ||
    any(!concentration_entries$concentration_purpose[
      !is.na(concentration_entries$concentration_purpose)
    ] %in% allowed_purposes)) {
  stop("A concentration basis or purpose label is absent from the dictionary.")
}
if (any(as.double(range_parts[, 3]) > as.double(range_parts[, 4]), na.rm = TRUE) ||
    any(as.double(product_parts[, 3]) > as.double(product_parts[, 4]), na.rm = TRUE)) {
  stop("A concentration range has descending endpoints.")
}
if (any(str_trim(product_parts[, 5]) == "menthol", na.rm = TRUE)) {
  stop("Use the menthol point or range format for a menthol percentage, not the product-percentage format.")
}

# Parse point concentrations while retaining basis and qualification for
# within-study deduplication. Ranges and product percentages remain
# informative extraction values but are not plotted as point estimates.
concentration_points <- concentration_entries |>
  mutate(
    point_approximate = point_parts[, 2],
    point_percent = as.double(point_parts[, 3]),
    point_basis = point_parts[, 4],
    point_qualification = point_parts[, 5]
  ) |>
  filter(is_point) |>
  transmute(
    record_id,
    concentration_percent = point_percent,
    concentration_basis = point_basis,
    concentration_purpose,
    qualification = case_when(
      !is.na(point_approximate) & !is.na(point_qualification) ~
        paste0("approximately; ", point_qualification),
      !is.na(point_approximate) ~ "approximately",
      !is.na(point_qualification) ~ point_qualification,
      TRUE ~ "direct"
    )
  ) |>
  distinct()

# Purpose annotates the existing point unit; it must not create extra points.
if (anyDuplicated(select(
  concentration_points,
  record_id, concentration_percent, concentration_basis, qualification
))) {
  stop("Equivalent concentration points have conflicting purpose tags; resolve their combined purpose before plotting.")
}

# Comparator profiles keep the source-near description, reported role, and
# controlled types together. Ingredients alone do not determine the role.
# Bare or scoped NI retains unresolved comparator information.
if (any(str_detect(studies$comparator, "(^|; )N/A(; |$)") &
        studies$comparator != "N/A")) {
  stop("N/A must stand alone in comparator.")
}
if (any(str_detect(studies$comparator, "(^|; )NI(; |$)") &
        studies$comparator != "NI")) {
  stop("Bare NI must stand alone in comparator; use scoped NI for an unresolved regimen alongside known profiles.")
}

profile_pattern <- "^([^\\[\\]]+) \\[(Therapeutic|Control): ([^\\[\\]]+)\\]$"
comparator_profiles <- comparator_entries |>
  filter(entry != "N/A", !str_detect(entry, unresolved_pattern))
profile_parts <- str_match(comparator_profiles$entry, profile_pattern)
if (any(is.na(profile_parts[, 1]))) {
  stop("comparator contains an invalid profile.")
}

comparator_profiles <- comparator_profiles |>
  transmute(
    record_id,
    entry,
    comparator_description = str_trim(profile_parts[, 2]),
    comparator_role = profile_parts[, 3],
    comparator_types = profile_parts[, 4]
  )

if (any(comparator_profiles$comparator_description == "")) {
  stop("A comparator profile has an empty description.")
}
profile_type_keys <- vapply(
  strsplit(comparator_profiles$comparator_types, " + ", fixed = TRUE),
  function(types) paste(sort(types), collapse = " + "),
  character(1)
)
if (anyDuplicated(paste(
  comparator_profiles$record_id,
  comparator_profiles$comparator_description,
  comparator_profiles$comparator_role,
  profile_type_keys,
  sep = "\r"
))) {
  stop("A comparator profile repeats the same regimen, role, and types, including reordered types.")
}

comparator_types <- comparator_profiles |>
  separate_longer_delim(comparator_types, delim = " + ") |>
  rename(comparator_type = comparator_types)

if (anyDuplicated(paste(
  comparator_types$record_id, comparator_types$entry,
  comparator_types$comparator_type, sep = "\r"
))) {
  stop("A comparator profile repeats a controlled type.")
}

control_types <- dictionary_values("comparator", "Control types")
therapeutic_types <- dictionary_values("comparator", "Therapeutic types")
if (
  any(comparator_types$comparator_role == "Control" &
        !comparator_types$comparator_type %in% c(control_types, "NI")) ||
    any(comparator_types$comparator_role == "Therapeutic" &
          !comparator_types$comparator_type %in% c(therapeutic_types, "NI")) ||
    any(
      comparator_types$comparator_type == "NI" &
        ave(
          rep(1L, nrow(comparator_types)),
          comparator_types$record_id,
          comparator_types$entry,
          FUN = length
        ) > 1L
    )
) {
  stop("A comparator type does not match its reported role.")
}

# Derive one membership table per result so all counts are study based.
form_memberships <- form_entries |>
  filter(!str_detect(entry, unresolved_pattern)) |>
  distinct(record_id, pharmaceutical_form = entry)

form_counts <- form_memberships |>
  count(pharmaceutical_form, name = "studies") |>
  arrange(desc(studies), pharmaceutical_form, .locale = "en")

most_common_forms <- form_counts |>
  filter(studies == max(c(0L, studies)))

named_co_interventions <- co_intervention_entries |>
  filter(entry != "N/A", !str_detect(entry, unresolved_pattern)) |>
  distinct(record_id, co_intervention = entry)

comparator_role_memberships <- comparator_profiles |>
  distinct(record_id, comparator_role)

comparator_type_memberships <- comparator_types |>
  filter(comparator_type != "NI") |>
  distinct(record_id, comparator_role, comparator_type)

control_ids <- comparator_role_memberships |>
  filter(comparator_role == "Control") |>
  pull(record_id)
therapeutic_ids <- comparator_role_memberships |>
  filter(comparator_role == "Therapeutic") |>
  pull(record_id)
both_comparator_ids <- intersect(control_ids, therapeutic_ids)

unresolved_comparator_ids <- union(
  comparator_entries$record_id[
    str_detect(comparator_entries$entry, unresolved_pattern)
  ],
  union(
    comparator_profiles$record_id[
      comparator_profiles$comparator_description == "NI"
    ],
    comparator_types$record_id[comparator_types$comparator_type == "NI"]
  )
)

# Attach bibliographic labels only after all scientific memberships are fixed.
study_labels <- studies |>
  select(record_id, first_author, publication_year, study_label)

ground_peppermint_labels <- form_memberships |>
  filter(pharmaceutical_form == "Ground peppermint") |>
  left_join(study_labels, by = "record_id") |>
  arrange(first_author, publication_year, record_id, .locale = "en") |>
  pull(study_label) |>
  str_c(collapse = "; ")

maximum_concentration <- if (nrow(concentration_points) > 0L) {
  max(concentration_points$concentration_percent)
} else {
  NA_real_
}
maximum_concentration_labels <- concentration_points |>
  filter(concentration_percent == maximum_concentration) |>
  distinct(record_id) |>
  left_join(study_labels, by = "record_id") |>
  arrange(first_author, publication_year, record_id, .locale = "en") |>
  pull(study_label) |>
  str_c(collapse = "; ")

co_intervention_summary <- named_co_interventions |>
  left_join(study_labels, by = "record_id") |>
  group_by(co_intervention) |>
  arrange(first_author, publication_year, record_id, .by_group = TRUE, .locale = "en") |>
  summarise(
    studies = n_distinct(record_id),
    study_labels = str_c(study_label, collapse = "; "),
    .groups = "drop"
  ) |>
  arrange(desc(studies), co_intervention, .locale = "en") |>
  transmute(summary = paste0(
    co_intervention, " (", studies, "): ", study_labels
  )) |>
  pull(summary) |>
  str_c(collapse = " | ")

comparator_type_summary <- comparator_type_memberships |>
  left_join(study_labels, by = "record_id") |>
  group_by(comparator_type, comparator_role) |>
  arrange(first_author, publication_year, record_id, .by_group = TRUE, .locale = "en") |>
  summarise(
    studies = n_distinct(record_id),
    study_labels = str_c(study_label, collapse = "; "),
    .groups = "drop"
  ) |>
  arrange(desc(studies), comparator_type, .locale = "en") |>
  transmute(summary = paste0(
    comparator_type, " [", comparator_role, "] (", studies, "): ",
    study_labels
  )) |>
  pull(summary) |>
  str_c(collapse = " | ")

both_comparator_labels <- study_labels |>
  filter(record_id %in% both_comparator_ids) |>
  arrange(first_author, publication_year, record_id, .locale = "en") |>
  pull(study_label) |>
  str_c(collapse = "; ")

point_study_ids <- unique(concentration_points$record_id)
unresolved_form_ids <- form_entries |>
  filter(str_detect(entry, unresolved_pattern)) |>
  pull(record_id) |>
  unique()
unresolved_concentration_ids <- concentration_entries |>
  filter(is_unresolved) |>
  pull(record_id) |>
  unique()
unresolved_co_intervention_ids <- co_intervention_entries |>
  filter(str_detect(entry, unresolved_pattern)) |>
  pull(record_id) |>
  unique()

in_text_results <- tibble(
  result = c(
    "included_studies",
    "studies_with_identified_pharmaceutical_form",
    "studies_with_unresolved_pharmaceutical_form_information",
    "pharmaceutical_form_summary",
    "most_common_pharmaceutical_form",
    "most_common_pharmaceutical_form_studies",
    "ground_peppermint_studies",
    "ground_peppermint_study_labels",
    "studies_with_usable_menthol_concentration_point",
    "studies_without_usable_menthol_concentration_point",
    "studies_with_unresolved_menthol_concentration_information",
    "usable_menthol_concentration_points",
    "minimum_menthol_concentration_percent",
    "maximum_menthol_concentration_percent",
    "maximum_menthol_concentration_studies",
    "maximum_menthol_concentration_study_labels",
    "studies_with_named_co_intervention",
    "studies_with_unresolved_co_intervention_information",
    "studies_with_fully_absent_co_intervention",
    "named_co_intervention_summary",
    "studies_with_classified_eligible_comparator",
    "studies_with_no_eligible_comparator",
    "studies_with_unresolved_comparator_information",
    "control_comparator_studies",
    "therapeutic_comparator_studies",
    "therapeutic_and_control_comparator_studies",
    "therapeutic_and_control_comparator_study_labels",
    "comparator_type_summary"
  ),
  value = as.character(c(
    nrow(studies),
    n_distinct(form_memberships$record_id),
    length(unresolved_form_ids),
    if (nrow(form_counts) > 0L) {
      str_c(
        paste0(form_counts$pharmaceutical_form, " (", form_counts$studies, ")"),
        collapse = " | "
      )
    } else "None",
    if (nrow(form_counts) > 0L) {
      str_c(most_common_forms$pharmaceutical_form, collapse = "; ")
    } else "NI",
    if (nrow(form_counts) > 0L) max(form_counts$studies) else "NI",
    sum(form_counts$studies[form_counts$pharmaceutical_form == "Ground peppermint"]),
    ground_peppermint_labels,
    length(point_study_ids),
    nrow(studies) - length(point_study_ids),
    length(unresolved_concentration_ids),
    nrow(concentration_points),
    if (nrow(concentration_points) > 0L) {
      min(concentration_points$concentration_percent)
    } else "NI",
    if (nrow(concentration_points) > 0L) maximum_concentration else "NI",
    n_distinct(
      concentration_points$record_id[
        !is.na(maximum_concentration) &
          concentration_points$concentration_percent == maximum_concentration
      ]
    ),
    maximum_concentration_labels,
    n_distinct(named_co_interventions$record_id),
    length(unresolved_co_intervention_ids),
    sum(studies$co_intervention == "N/A"),
    co_intervention_summary,
    n_distinct(comparator_profiles$record_id),
    sum(studies$comparator == "N/A"),
    length(unresolved_comparator_ids),
    length(unique(control_ids)),
    length(unique(therapeutic_ids)),
    length(both_comparator_ids),
    both_comparator_labels,
    comparator_type_summary
  ))
)

# An empty citation or named-category summary has no matching entries to list.
in_text_results$value[in_text_results$value == ""] <- "None"

if (
  nrow(in_text_results) != 28L ||
    anyDuplicated(in_text_results$result) ||
    any(is.na(in_text_results$value)) ||
    any(in_text_results$value == "")
) {
  stop("The Section 04 in-text output is incomplete.")
}

# Build all four panels from the same membership tables as the saved results.
form_colours <- c(
  Gel = "#D8806E",
  Solution = "#3E5571",
  Cream = "#EEC979",
  `Ground peppermint` = "#7691A1",
  Ointment = "#8D6E8B",
  Lotion = "#80A6A3",
  Foam = "#B7B7B7",
  Spray = "#9A8061",
  `Ground menthol` = "#5F8570",
  `Peppermint oil` = "#B18C52"
)

# Give any additional permitted forms a neutral grey colour.
additional_forms <- setdiff(allowed_forms, names(form_colours))
form_colours <- c(
  form_colours,
  setNames(rep("#808080", length(additional_forms)), additional_forms)
)

form_plot_data <- form_counts |>
  mutate(
    pharmaceutical_form = factor(
      pharmaceutical_form,
      levels = rev(pharmaceutical_form)
    )
  )

bar_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    plot.tag = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12)
  )

# Keep study-count ticks whole, including for a one-study subset.
study_count_breaks <- function(limits) {
  breaks <- unique(round(scales::breaks_pretty(n = 5)(limits)))
  breaks[breaks >= 0]
}

panel_a <- ggplot(
  form_plot_data,
  aes(x = studies, y = pharmaceutical_form, fill = pharmaceutical_form)
) +
  geom_col(width = 0.7) +
  geom_text(aes(label = studies), hjust = -0.25, size = 3.5) +
  scale_fill_manual(values = form_colours) +
  scale_x_continuous(
    breaks = study_count_breaks,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Pharmaceutical form",
    x = "Number of studies",
    tag = "a."
  ) +
  guides(fill = "none") +
  bar_theme

if (nrow(form_counts) == 0L) {
  panel_a <- ggplot() +
    annotate("text", x = 1, y = 1, label = "No identified pharmaceutical form") +
    labs(title = "Pharmaceutical form", tag = "a.") +
    theme_void(base_size = 11) +
    theme(plot.tag = element_text(face = "bold"))
}

rain_colour <- "#808080"
purpose_colours <- c(
  Therapeutic = "#0072B2",
  `Masking/control` = "#D55E00",
  Both = "#CC79A7",
  NI = "#555555"
)
purpose_labels <- c(
  Therapeutic = "Therapeutic intent",
  `Masking/control` = "Masking/nominal\nplacebo",
  Both = "Both purposes",
  NI = "Purpose unclear"
)
if (!setequal(allowed_purposes, names(purpose_colours))) {
  stop("The concentration-purpose categories have changed; update the Section 04 legend.")
}
concentration_points <- concentration_points |>
  mutate(concentration_purpose = factor(
    concentration_purpose, levels = names(purpose_colours)
  ))

# Colour only the dots. The box and violin summarise all retained point values.
# Reported purpose is distinct from comparator role or demonstrated efficacy.
panel_b <- ggplot(
  concentration_points,
  aes(x = factor(""), y = concentration_percent)
) +
  {if (nrow(concentration_points) > 1L) geom_rain(
    seed = 1,
    rain.side = "r",
    cov = "concentration_purpose",
    point.args = list(size = 2.5, alpha = 0.9),
    point.args.pos = list(
      position = ggpp::position_jitternudge(
        width = 0.065,
        height = 0,
        seed = 1,
        x = -0.4,
        nudge.from = "jittered",
        kept.origin = "none"
      )
    ),
    boxplot.args = list(
      fill = scales::alpha(rain_colour, 0.5),
      colour = rain_colour,
      linewidth = 1,
      outlier.shape = NA
    ),
    boxplot.args.pos = list(
      width = 0.1,
      position = position_nudge(x = -0.15)
    ),
    violin.args = list(
      fill = rain_colour,
      colour = rain_colour,
      linewidth = 1,
      alpha = 0.5,
      adjust = 1
    ),
    violin.args.pos = list(
      side = "r",
      width = 0.7,
      quantiles = NULL,
      position = position_nudge(x = 0)
    )
  ) else geom_point(aes(colour = concentration_purpose), size = 2.5, alpha = 0.9)} +
  scale_colour_manual(
    name = "Reported purpose",
    values = purpose_colours,
    breaks = names(purpose_colours),
    labels = purpose_labels,
    drop = TRUE
  ) +
  scale_x_discrete(expand = expansion(add = c(0.55, 0.35))) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::breaks_pretty(n = 5),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    x = NULL,
    y = "Menthol concentration (%)",
    tag = "b."
  ) +
  guides(colour = guide_legend(ncol = 2, byrow = TRUE)) +
  theme_classic(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12, margin = margin(r = 3)),
    plot.tag = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

if (nrow(concentration_points) == 0L) {
  panel_b <- ggplot() +
    annotate("text", x = 1, y = 1, label = "No usable menthol concentration points") +
    labs(tag = "b.") +
    theme_void(base_size = 11) +
    theme(plot.tag = element_text(face = "bold"))
}

broad_comparator_counts <- tibble(
  section = "Comparator roles",
  category = c(
    "Control/reference", "Therapeutic comparator",
    "No eligible comparator"
  ),
  studies = c(
    length(unique(control_ids)),
    length(unique(therapeutic_ids)),
    sum(studies$comparator == "N/A")
  ),
  colour_group = c("Control", "Therapeutic", "No comparator")
)

type_comparator_counts <- comparator_type_memberships |>
  count(comparator_role, comparator_type, name = "studies") |>
  transmute(
    section = "Comparator types",
    category = comparator_type,
    studies,
    colour_group = comparator_role
  )

broad_plot_data <- broad_comparator_counts |>
  arrange(desc(studies), category, .locale = "en") |>
  mutate(
    category = factor(category, levels = rev(category))
  )

type_plot_data <- type_comparator_counts |>
  arrange(desc(studies), category, .locale = "en") |>
  mutate(
    category = factor(category, levels = rev(category))
  )

comparator_colours <- c(
  Control = "#ABABAB",
  Therapeutic = "#D8806E",
  `No comparator` = "#3E5571"
)

panel_c <- ggplot(
  broad_plot_data,
  aes(x = studies, y = category, fill = colour_group)
) +
  geom_col(width = 0.7) +
  geom_text(aes(label = studies), hjust = -0.25, size = 3.5) +
  scale_fill_manual(values = comparator_colours) +
  scale_x_continuous(
    breaks = study_count_breaks,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Comparator roles",
    x = "Number of studies",
    tag = "c."
  ) +
  guides(fill = "none") +
  bar_theme

if (sum(broad_comparator_counts$studies) == 0L) {
  panel_c <- ggplot() +
    annotate("text", x = 1, y = 1, label = "No classified comparator roles") +
    labs(title = "Comparator roles", tag = "c.") +
    theme_void(base_size = 11) +
    theme(plot.tag = element_text(face = "bold"))
}

panel_d <- ggplot(
  type_plot_data,
  aes(x = studies, y = category, fill = colour_group)
) +
  geom_col(width = 0.7) +
  geom_text(aes(label = studies), hjust = -0.25, size = 3.5) +
  scale_fill_manual(values = comparator_colours) +
  scale_x_continuous(
    breaks = study_count_breaks,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Comparator types",
    x = "Number of studies",
    tag = "d."
  ) +
  guides(fill = "none") +
  bar_theme

if (nrow(type_comparator_counts) == 0L) {
  panel_d <- ggplot() +
    annotate("text", x = 1, y = 1, label = "No identified comparator types") +
    labs(title = "Comparator types", tag = "d.") +
    theme_void(base_size = 11) +
    theme(plot.tag = element_text(face = "bold"))
}

figure_4 <- wrap_plots(
  panel_a,
  free(panel_b, type = "label", side = "l"),
  panel_c,
  panel_d,
  ncol = 2
) +
  plot_annotation(
    caption = paste0(
      "Studies may contribute to multiple forms, concentrations, and comparator categories.\n",
      "Concentration points retain their reported percentage bases and qualifications; ranges and product percentages are excluded."
    ),
    theme = theme(plot.caption = element_text(hjust = 0, size = 9))
  )

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(in_text_results, file.path(output_dir, "in_text_results.csv"))
ggsave(
  filename = file.path(
    output_dir,
    "figure_4_interventions_cointerventions_comparators.png"
  ),
  plot = figure_4,
  device = ragg::agg_png,
  width = 12,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

message("Section 04 outputs written to: ", output_dir)
