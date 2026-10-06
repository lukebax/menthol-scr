# Topical menthol scoping review

Extracted study data and reproducible R analyses for a scoping review of topical menthol as an analgesic. The analyses describe study designs, populations, interventions, comparators, efficacy reporting, and adverse-event reporting across 31 studies. They do not estimate a pooled treatment effect or establish efficacy or safety.

## Repository layout

- `analysis/`: six numbered R scripts, one for each Results subsection.
- `data/data_extraction_form.csv`: study-level extracted data.
- `data/data_dictionary.csv`: definitions, extraction rules, and permitted values for every data column.
- `data/extraction_instructions.md`: the extraction procedure and source-checking instructions.
- `data/who_country_regions.csv`: the dated country-to-WHO-region reference used during extraction.
- `menthol-scr.Rproj`: the RStudio project.
- `LICENSE`: MIT licence.

`NI` means applicable information was not identified; `N/A` means the field does not apply under its dictionary rule.

## Extracting study data

The complete extraction package is the four files in [`data/`](data/). Start with [extraction_instructions.md](data/extraction_instructions.md), then use the [dictionary](data/data_dictionary.csv) to complete the [form](data/data_extraction_form.csv). The instructions include guidance for opening and saving the CSV files in a spreadsheet application. The package can be used independently with the relevant articles and supporting sources; running the R scripts is not required for extraction.

Use one row per included study. For a newly included study, assign the next unused `R001`-style identifier and extract the bibliographic information from its report. Keep existing identifiers stable when sorting or revisiting the form. Record the EPPI-Reviewer identifier when supplied, or `N/A` if no such record exists. Follow the field-specific rules for participant counts, multiple entries, and missing information, and retain the supporting source locations in one evidence note per study.

For an independent re-extraction, begin with a blank proposed row and the study identifiers, check it against the sources, and reconcile it with the existing row before replacing any data. Preserve the evidence for any correction. An unfinished blank is not equivalent to `NI`, `N/A`, or zero. Run the analyses only when all included rows are complete.

The dictionary defines the permitted categories. If a new study requires an additional category, resolve its definition and update the dictionary and affected analysis checks, labels, ordering, and colours together before including it in the results. The [country-region lookup](data/who_country_regions.csv) preserves the classification dated in the extraction instructions; do not silently replace it with a later classification.

## Running the analyses

1. Clone this repository or download and extract its ZIP file anywhere on your computer.
2. Open `menthol-scr.Rproj` in RStudio. This sets the project folder as the working directory; no file paths need editing.
3. If needed, install the required R packages once in the RStudio Console:

   ```r
   install.packages(c(
     "readr", "dplyr", "tidyr", "stringr", "ggplot2", "patchwork",
     "ggrain", "sf", "countrycode", "rnaturalearth", "flextable",
     "officer", "ragg", "scales", "ggpp", "gtable"
   ), repos = "https://cloud.r-project.org")
   ```

4. Open each script in `analysis/`, in order from `01` to `06`, and click **Source** to run the whole script. Each script reads the data directly and can also run independently.

Tested with R 4.6.1 and RStudio 2026.09.0+174 on macOS. R and the listed packages must be installed; no other project folders, source PDFs, or external data files are needed. Tables use Arial, so appearance may vary if that font is unavailable.

## Outputs

The scripts create `outputs/` automatically, with one subfolder per subsection:

| Script | Main output |
| --- | --- |
| 01 | Table 1: study characteristics, as CSV and Word files |
| 02 | Figure 2: study designs and pain measurements/contexts |
| 03 | Figure 3: study populations |
| 04 | Figure 4: interventions, co-interventions, and comparators |
| 05 | Figure 5: efficacy-result counts, numerical reporting, and registration |
| 06 | Table 2: adverse-event reporting, as CSV and Word files |

Each script also writes an `in_text_results.csv` containing named summary values. Together, the scripts produce four PNG figures, two CSV/Word table pairs, and six summary CSVs: 14 files. Re-running a script replaces its generated files.

Countries not separately represented in the coarse world map are listed with their study counts beneath Figure 3 and remain included in the country and regional summaries.

In Figure 4, studies can contribute to multiple forms, concentrations, and comparator categories. Concentration points retain the reported percentage bases and qualifications; ranges and product percentages are excluded. The therapeutic category includes clinical investigations of effects on existing pain and does not imply intended relief.

Figure 5 counts are confirmed minima of reported comparisons or descriptive case-report pain findings. Estimate and precision availability applies only to the counted comparisons. Zero-comparison studies have no coverage denominator; `N/A` denotes descriptive case reports.

The `outputs/` folder is intentionally excluded from Git. Generated results are reproduced locally from the supplied data and scripts.

## Licence

This repository is available under the [MIT licence](LICENSE).
