# Topical menthol scoping review

Extracted study data and reproducible R analyses for a scoping review of topical menthol as an analgesic. The analyses describe study designs, populations, interventions, comparators, efficacy reporting, and adverse-event reporting across 31 studies. They do not estimate a pooled treatment effect or establish efficacy or safety.

## Repository layout

- `analysis/`: six numbered R scripts, one for each Results subsection.
- `data/data_extraction_form.csv`: study-level extracted data.
- `data/data_dictionary.csv`: definitions, extraction rules, and permitted values for every data column.
- `menthol-scr.Rproj`: the RStudio project.
- `LICENSE`: MIT licence.

`NI` means applicable information was not identified; `N/A` means the field does not apply under its dictionary rule.

## Running the analyses

1. Clone this repository or download and extract its ZIP file anywhere on your computer.
2. Open `menthol-scr.Rproj` in RStudio. This sets the project folder as the working directory; no file paths need editing.
3. If needed, install the required R packages once in the RStudio Console:

   ```r
   install.packages(c(
     "readr", "dplyr", "tidyr", "stringr", "ggplot2", "patchwork",
     "ggrain", "sf", "countrycode", "rnaturalearth", "flextable",
     "officer", "ragg", "scales", "ggpp"
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
| 05 | Figure 5: efficacy-result counts and numerical reporting |
| 06 | Table 2: adverse-event reporting, as CSV and Word files |

Each script also writes an `in_text_results.csv` containing named summary values. Together, the scripts produce four PNG figures, two CSV/Word table pairs, and six summary CSVs: 14 files. Re-running a script replaces its generated files.

The `outputs/` folder is intentionally excluded from Git. Generated results are reproduced locally from the supplied data and scripts.

## Licence

This repository is available under the [MIT licence](LICENSE).
