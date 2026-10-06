# Open menthol-scr.Rproj, then run this whole script.
# It maps adverse-event reporting in Table 2 and saves the Section 06 counts.

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

output_dir <- file.path("outputs", "06_topical_menthol_safety")
studies <- read_csv(
  file.path("data", "data_extraction_form.csv"),
  col_types = cols(.default = col_character()),
  na = character(),
  show_col_types = FALSE
)
stop_for_problems(studies)

reporting_order <- dictionary_values("adverse_event_reporting")
if (!identical(reporting_order, c(
  "Events reported", "Explicitly no events reported",
  "Limited or unclear information", "NI"
))) {
  stop("The adverse-event categories have changed; update the Section 06 result labels and ordering.")
}
required_columns <- c(
  "record_id", "first_author", "publication_year",
  "adverse_event_reporting", "adverse_event_details"
)
if (!all(required_columns %in% names(studies))) {
  stop("The extraction form is missing a required Section 06 column.")
}
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
if (any(!studies$adverse_event_reporting %in% reporting_order) ||
    any(studies$adverse_event_details == "N/A") ||
    any((studies$adverse_event_reporting == "NI") !=
        (studies$adverse_event_details == "NI"))) {
  stop("Check the adverse-event reporting labels and matching details.")
}

# Count each study once. Keep all four categories, including any zero counts.
reporting_counts <- studies |>
  mutate(adverse_event_reporting = factor(
    adverse_event_reporting, levels = reporting_order
  )) |>
  count(adverse_event_reporting, .drop = FALSE)

in_text_results <- tibble(
  result = c(
    "included_studies",
    "studies_with_events_reported",
    "studies_with_explicitly_no_events_reported",
    "studies_with_limited_or_unclear_information",
    "studies_with_no_adverse_event_information_identified",
    "studies_with_any_adverse_event_information"
  ),
  value = c(
    nrow(studies), reporting_counts$n,
    sum(studies$adverse_event_reporting != "NI")
  )
)

# Display the extracted account without interpreting or pooling its event counts.
# Add the record ID only when author and year alone would be ambiguous.
study_labels <- paste0(studies$first_author, " (", studies$publication_year, ")")
duplicate_labels <- duplicated(study_labels) | duplicated(study_labels, fromLast = TRUE)
study_labels[duplicate_labels] <- paste0(
  study_labels[duplicate_labels], " [", studies$record_id[duplicate_labels], "]"
)
table_2 <- studies |>
  mutate(study = study_labels) |>
  filter(adverse_event_reporting != "NI") |>
  arrange(
    match(adverse_event_reporting, reporting_order),
    first_author, publication_year, record_id, .locale = "en"
  ) |>
  select(study, adverse_event_reporting, adverse_event_details)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(in_text_results, file.path(output_dir, "in_text_results.csv"))
write_csv(table_2, file.path(output_dir, "table_2_adverse_event_reporting.csv"))

# Give the narrative column most of the page width and repeat headers on each page.
table_word <- flextable(table_2) |>
  set_header_labels(
    study = "Study",
    adverse_event_reporting = "Adverse-event reporting",
    adverse_event_details = "Reported adverse-event information"
  ) |>
  theme_vanilla() |>
  font(fontname = "Arial", part = "all") |>
  fontsize(size = 10.5, part = "all") |>
  padding(padding = 4, part = "all") |>
  bg(i = seq_len(nrow(table_2)) %% 2 == 0, bg = "#F2F4F6", part = "body") |>
  bg(bg = "#1F4E78", part = "header") |>
  color(color = "white", part = "header") |>
  bold(part = "header") |>
  border_outer(border = fp_border(color = "#D9D9D9", width = 0.5), part = "all") |>
  border_inner(border = fp_border(color = "#D9D9D9", width = 0.5), part = "all") |>
  valign(valign = "center", part = "all") |>
  width(j = "study", width = 1.05) |>
  width(j = "adverse_event_reporting", width = 1.55) |>
  width(j = "adverse_event_details", width = 4.35) |>
  add_footer_lines(values = paste(
    if (nrow(table_2) == 0L) {
      "No relevant adverse-event information was identified in any included study."
    } else {
      "Studies with no relevant adverse-event information identified (NI) are omitted."
    },
    "Limited or unclear reporting is not an explicit report of zero events.",
    "These reporting categories do not establish causation or adverse-event incidence."
  )) |>
  fontsize(size = 9, part = "footer") |>
  set_caption(caption = "Table 2. Adverse-event reporting") |>
  set_table_properties(
    layout = "fixed",
    opts_word = list(split = FALSE, repeat_headers = TRUE)
  )

portrait_page <- prop_section(
  page_size = page_size(width = 8.27, height = 11.69),
  page_margins = page_mar(top = 0.6, bottom = 0.6, left = 0.6, right = 0.6)
)
save_as_docx(
  values = list(table_word),
  path = file.path(output_dir, "table_2_adverse_event_reporting.docx"),
  pr_section = portrait_page
)

message("Section 06 outputs written to: ", output_dir)
