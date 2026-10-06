# Open menthol-scr.Rproj, then run this whole script.
# It creates Table 1 and the participant-count result for Section 01.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(flextable)
  library(officer)
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
output_dir <- file.path("outputs", "01_study_selection_and_characteristics")

studies <- read_csv(
  input_path,
  col_types = cols(.default = col_character()),
  na = character(),
  show_col_types = FALSE
)
stop_for_problems(studies)

# Check the inputs needed by this subsection.
required_columns <- c(
  "record_id", "first_author", "publication_year", "study_design", "sample_size",
  "pain_type", "pharmaceutical_form", "application_site", "co_intervention", "comparator"
)
if (!all(required_columns %in% names(studies))) {
  stop("The extraction form is missing a column needed for Section 01.")
}
if (nrow(studies) == 0L || anyDuplicated(studies$record_id)) {
  stop("The extraction form must contain at least one study row with unique record IDs.")
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
# Arrange studies in dictionary design order, then alphabetically by author.
design_order <- dictionary_values("study_design")
if (any(!studies$study_design %in% design_order)) {
  stop("study_design contains an unexpected value.")
}
if (any(!grepl("^(0|[1-9][0-9]*|NI)$", studies$sample_size))) {
  stop("sample_size must be a whole number or NI.")
}

# Table 1 displays the comparator descriptions without the role/type labels.
format_comparator_for_table <- function(x) {
  vapply(
    strsplit(x, "; ", fixed = TRUE),
    function(profiles) {
      descriptions <- sub(
        " \\[(?:Therapeutic|Control): [^]]+\\]$",
        "",
        profiles,
        perl = TRUE
      )
      paste(unique(descriptions), collapse = "; ")
    },
    character(1)
  )
}

table_1 <- studies |>
  mutate(
    study = paste0(first_author, " (", publication_year, ")"),
    design_order = match(study_design, design_order)
  ) |>
  arrange(
    design_order,
    tolower(first_author),
    publication_year,
    record_id
  ) |>
  transmute(
    study,
    study_design,
    sample_size,
    pain_type,
    pharmaceutical_form,
    application_site,
    co_intervention,
    comparator = format_comparator_for_table(comparator)
  )

# Sum each available study-level sample size once.
known_sample <- studies$sample_size != "NI"

in_text_results <- tibble(
  result = paste0(
    "known_participant_count_", sum(known_sample), "_of_", nrow(studies), "_studies"
  ),
  value = sum(as.integer(studies$sample_size[known_sample]))
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(table_1, file.path(output_dir, "table_1_study_characteristics.csv"))
write_csv(in_text_results, file.path(output_dir, "in_text_results.csv"))

# Format a manuscript-usable landscape Word version of the same table.
table_word <- flextable(table_1) |>
  set_header_labels(
    study = "Study",
    study_design = "Study design",
    sample_size = "Sample size",
    pain_type = "Pain type",
    pharmaceutical_form = "Pharmaceutical form",
    application_site = "Application site",
    co_intervention = "Co-intervention",
    comparator = "Comparator"
  ) |>
  theme_vanilla() |>
  font(fontname = "Arial", part = "all") |>
  fontsize(size = 8, part = "all") |>
  padding(padding = 2, part = "all") |>
  bg(bg = "#1F4E78", part = "header") |>
  color(color = "white", part = "header") |>
  bold(part = "header") |>
  valign(valign = "center", part = "all") |>
  width(j = "study", width = 0.9) |>
  width(j = "study_design", width = 0.7) |>
  width(j = "sample_size", width = 0.7) |>
  width(j = "pain_type", width = 1.2) |>
  width(j = "pharmaceutical_form", width = 1.05) |>
  width(j = "application_site", width = 0.95) |>
  width(j = "co_intervention", width = 2.15) |>
  width(j = "comparator", width = 2.15) |>
  add_footer_lines(
    values = "NI, no information identified; N/A, not applicable."
  ) |>
  set_caption(caption = "Table 1. Study characteristics") |>
  set_table_properties(
    layout = "fixed",
    opts_word = list(split = FALSE, repeat_headers = TRUE)
  )

landscape_page <- prop_section(
  page_size = page_size(orient = "landscape"),
  page_margins = page_mar(top = 0.35, bottom = 0.35, left = 0.4, right = 0.4)
)

save_as_docx(
  values = list(table_word),
  path = file.path(output_dir, "table_1_study_characteristics.docx"),
  pr_section = landscape_page
)

message("Section 01 outputs written to: ", output_dir)
