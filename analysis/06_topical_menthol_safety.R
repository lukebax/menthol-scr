# Open menthol-scr.Rproj, then run this whole script.
# It maps adverse-event reporting in Table 2 and saves the Section 06 counts.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(flextable)
  library(officer)
})

output_dir <- file.path("outputs", "06_topical_menthol_safety")
studies <- read_csv(
  file.path("data", "data_extraction_form.csv"),
  col_types = cols(.default = col_character()),
  na = character(),
  show_col_types = FALSE
)

reporting_order <- c(
  "Events reported", "Explicitly no events reported",
  "Limited or unclear information", "NI"
)
required_columns <- c(
  "record_id", "first_author", "publication_year",
  "adverse_event_reporting", "adverse_event_details"
)
if (!all(required_columns %in% names(studies))) {
  stop("The extraction form is missing a required Section 06 column.")
}
if (nrow(studies) == 0L || anyDuplicated(studies$record_id) ||
    any(is.na(studies)) || any(trimws(as.matrix(studies)) == "")) {
  stop("The extraction form must contain at least one completed study row with unique record IDs.")
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
    tolower(first_author), publication_year, record_id
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
    "Studies with no relevant adverse-event information identified (NI) are omitted.",
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
