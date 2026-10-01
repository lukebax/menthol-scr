# Open menthol-scr.Rproj, then run this whole script.
# It creates Figure 3 and the in-text results for Section 03.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(patchwork)
  library(ggrain)
  library(sf)
  library(countrycode)
})

input_path <- file.path("data", "data_extraction_form.csv")
output_dir <- file.path("outputs", "03_study_populations")

studies <- read_csv(
  input_path,
  col_types = cols(.default = col_character()),
  na = character(),
  show_col_types = FALSE
)

# Keep checks short and tied to the requested outputs.
required_columns <- c(
  "record_id", "first_author", "publication_year", "study_design",
  "sample_size", "sample_size_basis", "pain_contexts", "participant_sex",
  "country", "who_region"
)
if (!all(required_columns %in% names(studies))) {
  stop("The extraction form is missing a column needed for Section 03.")
}
if (nrow(studies) == 0L || anyDuplicated(studies$record_id)) {
  stop("The extraction form must contain at least one study row and unique record_id values.")
}
if (any(is.na(studies)) || any(studies == "")) {
  stop("The extraction form contains a blank value.")
}

allowed_designs <- c(
  "Parallel groups", "Cross-over", "Pre-post", "Single arm", "Case report", "NI"
)
allowed_sample_bases <- c(
  "randomised", "allocated_or_enrolled",
  "started_eligible_menthol_intervention", "persons_described"
)
allowed_sexes <- c("Female", "Male", "Mixed", "NI")
allowed_regions <- c("AFR", "AMR", "SEAR", "EUR", "EMR", "WPR")

if (any(!studies$study_design %in% allowed_designs)) {
  stop("study_design contains an unexpected value.")
}
if (any(!studies$sample_size_basis %in% allowed_sample_bases)) {
  stop("sample_size_basis contains an unexpected value.")
}
if (any(!studies$participant_sex %in% allowed_sexes)) {
  stop("participant_sex contains an unexpected value.")
}
if (any(!grepl("^(0|[1-9][0-9]*|NI)$", studies$sample_size))) {
  stop("sample_size must be a whole number or NI.")
}
if (any((studies$country == "NI") != (studies$who_region == "NI")) ||
    any(str_detect(studies$country, "(^|; )NI(; |$)") !=
        str_detect(studies$who_region, "(^|; )NI(; |$)"))) {
  stop("Missing country and WHO region values do not correspond.")
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

# Keep preferred English names in the dictionary, alongside their stable ISO identity.
country_format <- dictionary$format_or_allowed_values[dictionary$column_name == "country"]
country_override_clause <- str_match(
  country_format,
  "Preferred English short-name overrides \\(ISO3\\): ([^.]+)\\."
)[, 2]
if (length(country_override_clause) != 1L || is.na(country_override_clause) ||
    !nzchar(country_override_clause) ||
    str_count(country_format, fixed("Preferred English short-name overrides (ISO3): ")) != 1L) {
  stop("Cannot read the preferred country names from the dictionary; check its override clause.")
}
country_override_pairs <- str_match(
  str_split(country_override_clause, " \\| ")[[1]],
  "^([A-Z]{3})=([^|=;]+)$"
)
if (anyNA(country_override_pairs) ||
    any(country_override_pairs[, 3] != str_trim(country_override_pairs[, 3])) ||
    anyDuplicated(country_override_pairs[, 2]) || anyDuplicated(country_override_pairs[, 3])) {
  stop("Preferred country-name overrides must contain valid, unique ISO3 codes and names.")
}
preferred_country_names <- setNames(country_override_pairs[, 3], country_override_pairs[, 2])

# Split plural study characteristics in memory and confirm unique membership.
country_memberships <- studies |>
  select(record_id, country) |>
  separate_longer_delim(country, delim = "; ")

region_memberships <- studies |>
  select(record_id, who_region) |>
  separate_longer_delim(who_region, delim = "; ")

context_pairs <- studies |>
  select(
    record_id, first_author, publication_year, study_design,
    participant_sex, pain_contexts
  ) |>
  separate_longer_delim(pain_contexts, delim = "; ") |>
  rename(context_pair = pain_contexts)

if (
  anyDuplicated(paste(country_memberships$record_id, country_memberships$country)) ||
  anyDuplicated(paste(region_memberships$record_id, region_memberships$who_region)) ||
  anyDuplicated(paste(context_pairs$record_id, context_pairs$context_pair))
) {
  stop("A plural study characteristic contains a repeated value.")
}
if (any(region_memberships$who_region != "NI" &
        !region_memberships$who_region %in% allowed_regions)) {
  stop("who_region contains an unexpected value.")
}
if (any(str_detect(studies$pain_contexts, "(^|; )NI(; |$)") & studies$pain_contexts != "NI") ||
    any(str_ends(context_pairs$context_pair, ": NI")) ||
    any(context_pairs$context_pair != "NI" &
        !str_detect(context_pairs$context_pair, "^(Induced|Pre-existing): [^:;]+$"))) {
  stop("pain_contexts contains an invalid pair.")
}

if (any(context_pairs$context_pair != "NI" &
        !str_remove(context_pairs$context_pair, "^[^:]+: ") %in% allowed_contexts)) {
  stop("pain_contexts contains a label absent from the dictionary.")
}

country_memberships <- country_memberships |>
  filter(country != "NI") |>
  mutate(
    iso3 = countrycode(country, origin = "country.name", destination = "iso3c"),
    iso2 = countrycode(country, origin = "country.name", destination = "iso2c")
  ) |>
  distinct(record_id, country, iso3, iso2)

if (any(is.na(country_memberships$iso3)) || any(is.na(country_memberships$iso2))) {
  stop("A country could not be converted to an ISO code.")
}

# Country-name variants share one study-country membership and map polygon.
# Apply the dictionary's preferred display names without changing extraction text.
country_memberships <- country_memberships |>
  mutate(country = coalesce(
    unname(preferred_country_names[iso3]),
    countrycode(iso3, origin = "iso3c", destination = "country.name")
  )) |>
  distinct(record_id, country, iso3, iso2)

region_memberships <- region_memberships |>
  filter(who_region != "NI") |>
  distinct(record_id, who_region)

contexts <- context_pairs |>
  filter(context_pair != "NI") |>
  separate_wider_delim(
    context_pair,
    delim = ": ",
    names = c("pain_origin", "pain_context")
  ) |>
  distinct(
    record_id, first_author, publication_year, study_design,
    participant_sex, pain_origin, pain_context
  )

# Derive geography, sex, context, and sample-size summaries.
country_counts <- country_memberships |>
  count(country, iso3, iso2, name = "studies") |>
  arrange(desc(studies), country, .locale = "en") |>
  mutate(country_rank = dense_rank(desc(studies)))

region_names <- c(
  AFR = "African Region",
  AMR = "Region of the Americas",
  SEAR = "South-East Asia Region",
  EUR = "European Region",
  EMR = "Eastern Mediterranean Region",
  WPR = "Western Pacific Region"
)

region_counts <- region_memberships |>
  count(who_region, name = "studies") |>
  mutate(region_name = unname(region_names[who_region])) |>
  arrange(desc(studies), region_name, .locale = "en")

most_common_region <- region_counts |>
  filter(studies == max(c(0L, studies))) |>
  arrange(region_name, .locale = "en")

country_ranks <- country_counts |>
  filter(country_rank <= 3L) |>
  group_by(country_rank) |>
  summarise(
    countries = str_c(country, collapse = "; "),
    studies = first(studies),
    .groups = "drop"
  ) |>
  complete(country_rank = 1:3) |>
  mutate(
    countries = coalesce(countries, "N/A"),
    studies = coalesce(as.character(studies), "N/A")
  )

sex_counts <- studies |>
  count(participant_sex, name = "studies")

case_report_labels <- studies |>
  filter(study_design == "Case report") |>
  arrange(first_author, publication_year, record_id, .locale = "en") |>
  mutate(label = paste0(first_author, " (", publication_year, ")"))

single_sex_group_studies <- studies |>
  filter(study_design != "Case report", participant_sex %in% c("Female", "Male"))

# Include record_id in citation-ready summaries to distinguish studies that
# share the same first author and publication year.
group_contexts <- contexts |>
  filter(study_design != "Case report", participant_sex %in% c("Female", "Male")) |>
  mutate(
    context_pair = paste0(pain_origin, ": ", pain_context),
    study_label = paste0(
      first_author, " (", publication_year, ") [", record_id, "]"
    )
  )

context_summaries <- group_contexts |>
  group_by(participant_sex, context_pair) |>
  arrange(first_author, publication_year, record_id, .by_group = TRUE, .locale = "en") |>
  summarise(
    studies = n_distinct(record_id),
    study_labels = str_c(study_label, collapse = "; "),
    .groups = "drop"
  ) |>
  arrange(participant_sex, desc(studies), context_pair, .locale = "en") |>
  mutate(context_summary = paste0(context_pair, " (", studies, "): ", study_labels)) |>
  summarise(
    context_summary = str_c(context_summary, collapse = " | "),
    .by = participant_sex
  )

numeric_sample_sizes <- studies |>
  filter(sample_size != "NI") |>
  transmute(
    record_id,
    first_author,
    publication_year,
    sample_size = as.integer(sample_size)
  )

missing_sample_labels <- studies |>
  filter(sample_size == "NI") |>
  arrange(first_author, publication_year, record_id, .locale = "en") |>
  transmute(label = paste0(first_author, " (", publication_year, ")")) |>
  pull(label) |>
  str_c(collapse = "; ")

maximum_sample_labels <- numeric_sample_sizes |>
  filter(sample_size == max(c(0L, sample_size))) |>
  arrange(first_author, publication_year, record_id, .locale = "en") |>
  transmute(label = paste0(first_author, " (", publication_year, ")")) |>
  pull(label) |>
  str_c(collapse = "; ")

in_text_results <- tibble(
  result = c(
    "included_studies",
    "most_common_who_region",
    "most_common_who_region_studies",
    "african_region_studies",
    "country_rank_1",
    "country_rank_1_studies",
    "country_rank_2",
    "country_rank_2_studies",
    "country_rank_3",
    "country_rank_3_studies",
    "mixed_sex_studies",
    "female_only_studies",
    "male_only_studies",
    "single_sex_studies",
    "single_sex_percent",
    "female_case_report_study_labels",
    "male_case_report_study_labels",
    "female_only_group_studies",
    "female_only_group_pre_existing_studies",
    "female_only_group_context_summary",
    "male_only_group_studies",
    "male_only_group_induced_studies",
    "male_only_group_context_summary",
    "numeric_sample_size_studies",
    "missing_sample_size_studies",
    "missing_sample_size_study_labels",
    "minimum_sample_size",
    "maximum_sample_size",
    "maximum_sample_size_study_labels"
  ),
  value = as.character(c(
    nrow(studies),
    if (nrow(most_common_region) > 0L) {
      str_c(most_common_region$region_name, collapse = "; ")
    } else "NI",
    if (nrow(most_common_region) > 0L) max(most_common_region$studies) else "NI",
    sum(region_memberships$who_region == "AFR"),
    country_ranks$countries[country_ranks$country_rank == 1L],
    country_ranks$studies[country_ranks$country_rank == 1L],
    country_ranks$countries[country_ranks$country_rank == 2L],
    country_ranks$studies[country_ranks$country_rank == 2L],
    country_ranks$countries[country_ranks$country_rank == 3L],
    country_ranks$studies[country_ranks$country_rank == 3L],
    sum(studies$participant_sex == "Mixed"),
    sum(studies$participant_sex == "Female"),
    sum(studies$participant_sex == "Male"),
    sum(studies$participant_sex %in% c("Female", "Male")),
    round(100 * sum(studies$participant_sex %in% c("Female", "Male")) / nrow(studies)),
    str_c(
      case_report_labels$label[case_report_labels$participant_sex == "Female"],
      collapse = "; "
    ),
    str_c(
      case_report_labels$label[case_report_labels$participant_sex == "Male"],
      collapse = "; "
    ),
    sum(single_sex_group_studies$participant_sex == "Female"),
    n_distinct(group_contexts$record_id[
      group_contexts$participant_sex == "Female" &
        group_contexts$pain_origin == "Pre-existing"
    ]),
    first(context_summaries$context_summary[context_summaries$participant_sex == "Female"], default = "None"),
    sum(single_sex_group_studies$participant_sex == "Male"),
    n_distinct(group_contexts$record_id[
      group_contexts$participant_sex == "Male" &
        group_contexts$pain_origin == "Induced"
    ]),
    first(context_summaries$context_summary[context_summaries$participant_sex == "Male"], default = "None"),
    nrow(numeric_sample_sizes),
    sum(studies$sample_size == "NI"),
    missing_sample_labels,
    if (nrow(numeric_sample_sizes) > 0L) min(numeric_sample_sizes$sample_size) else "NI",
    if (nrow(numeric_sample_sizes) > 0L) max(numeric_sample_sizes$sample_size) else "NI",
    maximum_sample_labels
  ))
)

# An empty citation list means no matching study, not a missing calculation.
in_text_results$value[in_text_results$value == ""] <- "None"

if (
  nrow(in_text_results) != 29L ||
  anyDuplicated(in_text_results$result) ||
  any(is.na(in_text_results$value)) ||
  any(in_text_results$value == "")
) {
  stop("The Section 03 in-text output is incomplete.")
}

# Build the study-population figure from the same derived counts.
world <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf") |>
  filter(admin != "Antarctica")

if (anyDuplicated(world$adm0_a3)) {
  stop("The Natural Earth map contains a repeated country join code.")
}

world_counts <- world |>
  left_join(country_counts, by = c("adm0_a3" = "iso3"), relationship = "one-to-one") |>
  mutate(
    study_count = factor(
      studies,
      levels = as.character(seq_len(max(c(0L, country_counts$studies))))
    )
  )

if (sum(!is.na(world_counts$studies)) != nrow(country_counts)) {
  stop("A study country did not join exactly once to the Natural Earth map.")
}

studied_countries <- world_counts |>
  filter(!is.na(studies), studies >= 1)

# Calculate country-label positions on a projected map, then return them to
# longitude and latitude. This keeps labels inside their country polygons.
country_label_geometry <- studied_countries |>
  st_geometry() |>
  st_transform("ESRI:54030") |>
  st_point_on_surface() |>
  st_transform(st_crs(world_counts))

country_labels <- studied_countries |>
  st_drop_geometry()
country_labels <- if (nrow(country_labels) > 0L) {
  bind_cols(country_labels, as.data.frame(st_coordinates(country_label_geometry)))
} else {
  mutate(country_labels, X = numeric(), Y = numeric())
}

base_country_colours <- c(
  `1` = "#DBEEC8", `2` = "#BFDEBA", `3` = "#B2D6B3",
  `4` = "#A4CEAB", `5` = "#8BBF9D", `6` = "#75B08E"
)
maximum_country_count <- max(c(0L, country_counts$studies))
country_colours <- if (maximum_country_count <= length(base_country_colours)) {
  base_country_colours[seq_len(maximum_country_count)]
} else {
  setNames(
    grDevices::colorRampPalette(base_country_colours)(maximum_country_count),
    seq_len(maximum_country_count)
  )
}

region_plot_data <- tibble(who_region = allowed_regions) |>
  left_join(
    region_counts |> select(who_region, studies),
    by = "who_region"
  ) |>
  mutate(
    studies = replace_na(studies, 0L),
    region_name = unname(region_names[who_region]),
    region_label = region_name |>
      str_remove("^Region of the ") |>
      str_remove(" Region$")
  ) |>
  arrange(desc(studies), region_name, .locale = "en") |>
  mutate(region_label = factor(region_label, levels = rev(region_label)))

# Keep study-count ticks whole, including for a one-study subset.
study_count_breaks <- function(limits) {
  breaks <- unique(round(scales::breaks_pretty(n = 5)(limits)))
  breaks[breaks >= 0]
}

panel_b <- ggplot(region_plot_data, aes(x = studies, y = region_label)) +
  geom_col(width = 0.72, fill = "#476384") +
  scale_x_continuous(
    breaks = study_count_breaks,
    expand = expansion(mult = c(0, 0.03))
  ) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 22)) +
  labs(
    x = "Number of studies",
    y = NULL,
    tag = "b."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.tag = element_text(face = "bold"),
    axis.title.x = element_text(size = 11),
    axis.text.y = element_text(size = 8.5)
  )

map_plot <- ggplot() +
  geom_sf(
    data = world_counts,
    fill = "#CCCCCC",
    colour = "white",
    linewidth = 0.12
  ) +
  geom_sf(
    data = studied_countries,
    aes(fill = study_count),
    colour = "white",
    linewidth = 0.12,
    show.legend = TRUE
  ) +
  geom_text(
    data = country_labels,
    aes(x = X, y = Y, label = iso2),
    size = 3,
    colour = "black"
  ) +
  {if (maximum_country_count > 0L) scale_fill_manual(
    values = country_colours,
    drop = FALSE,
    na.translate = FALSE,
    name = "Number of studies"
  )} +
  coord_sf(xlim = c(-170, 180), ylim = c(-58, 85), expand = FALSE) +
  labs(tag = "a.") +
  theme_void(base_size = 11) +
  theme(
    plot.tag = element_text(face = "bold"),
    # Keep the tag and legend on the author-approved vertical guides.
    plot.tag.position = c(0.059, 1),
    legend.position = "inside",
    legend.position.inside = c(0.008, 0.25),
    legend.justification = c(0, 0.5),
    legend.direction = "horizontal",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.width = grid::unit(0.45, "cm"),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  guides(fill = guide_legend(title.position = "top", nrow = 1, byrow = TRUE))

if (maximum_country_count == 0L) {
  map_plot <- map_plot +
    annotate("text", x = 0, y = 0, label = "No identified study countries")
}

sex_colours <- c(
  Mixed = "#3E5571",
  Female = "#D8806E",
  Male = "#7691A1",
  NI = "#999999"
)

sex_plot_data <- sex_counts |>
  filter(participant_sex %in% names(sex_colours)) |>
  arrange(desc(studies), participant_sex, .locale = "en") |>
  mutate(participant_sex = factor(participant_sex, levels = rev(participant_sex)))

panel_c <- ggplot(
  sex_plot_data,
  aes(x = studies, y = participant_sex, fill = participant_sex)
) +
  geom_col(width = 0.62) +
  scale_fill_manual(values = sex_colours) +
  scale_x_continuous(
    breaks = study_count_breaks,
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = "Number of studies", y = NULL, tag = "c.") +
  guides(fill = "none") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.tag = element_text(face = "bold"),
    axis.title.x = element_text(size = 11)
  )

rain_colour <- "#1B9E77"
sample_size_breaks <- scales::breaks_pretty(n = 5)(
  c(0, max(c(1L, numeric_sample_sizes$sample_size)))
)
sample_size_upper_limit <- max(sample_size_breaks)

panel_d <- ggplot(
  numeric_sample_sizes,
  aes(x = factor(""), y = sample_size)
) +
  {if (nrow(numeric_sample_sizes) > 1L) geom_rain(
    seed = 1,
    rain.side = "r",
    point.args = list(colour = rain_colour, size = 2.5, alpha = 0.5),
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
  ) else geom_point(colour = rain_colour, size = 2.5, alpha = 0.5)} +
  scale_x_discrete(expand = expansion(add = c(0.55, 0.35))) +
  scale_y_continuous(
    limits = c(0, sample_size_upper_limit),
    breaks = sample_size_breaks,
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(x = NULL, y = "Sample size", tag = "d.") +
  theme_classic(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    plot.tag = element_text(face = "bold")
  )

if (nrow(numeric_sample_sizes) == 0L) {
  panel_d <- ggplot() +
    annotate("text", x = 1, y = 1, label = "No usable sample sizes") +
    labs(tag = "d.") +
    theme_void(base_size = 11) +
    theme(plot.tag = element_text(face = "bold"))
}

# Equal panel widths and small outer spacers align the lower row with the map.
lower_row <- (plot_spacer() | panel_b | panel_c | panel_d | plot_spacer()) +
  plot_layout(widths = c(0.2, 1, 1, 1, 0.2))

figure_3 <- map_plot / lower_row +
  plot_layout(heights = c(1, 0.85))

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(in_text_results, file.path(output_dir, "in_text_results.csv"))
ggsave(
  filename = file.path(output_dir, "figure_3_study_populations.png"),
  plot = figure_3,
  device = ragg::agg_png,
  width = 12,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)

message("Section 03 outputs written to: ", output_dir)
