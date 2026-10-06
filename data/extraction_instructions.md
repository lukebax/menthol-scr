# Extracting study data

Version: 2026-10-06

Use these instructions with `data_dictionary.csv` to extract one study already included in the review. Produce one proposed row with the 25 headings in `data_extraction_form.csv` and one evidence note using the template below. Read the sources independently, check the proposed row, and integrate it into the canonical form only after review. For an existing study, keep its identifier and record any accepted corrections. For a new study, assign the next unused identifier and extract its bibliographic metadata from the article.

The reusable package consists of four files in this folder: the form, the dictionary, these instructions, and `who_country_regions.csv`. The dictionary defines each field and its permitted values. These instructions explain procedures shared across fields and the detailed efficacy-counting method. The WHO table provides the dated country-to-region classification. Article PDFs, formal supplements, and any targeted supporting sources are research evidence supplied for each study. One combined evidence note per study is the extraction work product; no additional scientific rule files are required.

This package describes study characteristics and the reporting of results. Its counts are not an assessment of whether menthol is effective or safe, and statistical significance is not used for vote counting.

## 1. Sources and independent reading

Confirm the study identity using title, byline, publication version, and DOI or another bibliographic identifier. Author and year alone may not distinguish reports. A study can have companion reports without acquiring a new study identifier. Preserve compound surnames and diacritics, and use the version-of-record year. Do not derive metadata from a PDF filename. EPPI-Reviewer identifiers come from the review author; use `N/A` only for a genuinely absent record, and do not fabricate a missing identifier.

Read the primary article and its formal supplements in full, including tables, figures, captions, footnotes, methods, and withdrawal information. The reporting scope for efficacy counts, estimate and precision availability, and adverse-event reporting is the primary article plus its formal supplements. A registry, protocol, statistical analysis plan, thesis, or clinical study report may clarify an identified identity or methods question, but findings appearing only there do not enlarge this reporting scope. A protocol establishes planned methods, not what occurred. Record contradictions rather than silently choosing a convenient source.

Begin from the source article, without consulting previous extracted scientific answers, worked article examples, or manuscript conclusions. Save the independent proposed row and supporting evidence before a focused comparison with earlier evidence from that same study. Prior values are not numerical targets. Reuse a previous registration search only with its actual search date and limitations; check any new identifier or concrete contradiction without describing reused evidence as a fresh search.

Text extraction can help navigation, but inspect the page image for tables, figures, scans, symbols, and ambiguous layouts. Confirm printed labels and footnotes. Use translations as aids while checking numbers and structure against the original. Do not digitise graphs or estimate values from their positions. Preserve sources unchanged, and describe any inaccessible material or translation limitation precisely.

For every retained value, including `NI` and `N/A`, record a source locator or the relevant checking scope and reporting limitation. Identify sources by full citation and filename or URL, then use short source labels in repeated references. Give PDF page and printed page where available, plus table, figure, section, row, or column. Distinguish source facts, transparent calculations, external metadata, and unresolved interpretation. Shared evidence can support several fields.

## 2. Investigation boundary and row conventions

Describe the complete eligible investigation and its relevant comparison conditions. A separable component using menthol as a pain-model reference remains in the evidence note, outside the core field values. Establish that separation from its reported purpose and context, independently of dose or observed response. Preserve its participants, exposures, purpose, and all findings, including null or unexpectedly beneficial findings. Do not force it into a therapeutic or nominal-placebo category, add its reference-only participants to sample size, or count its standalone provocation findings.

This boundary does not automatically exclude healthy volunteers, induced-pain analgesia experiments, ordinary relevant controls, or mechanism-only cohorts. An eligible participant who also supplies a reference site remains one eligible participant. Retain otherwise eligible clinical investigations of menthol's effects on existing pain or pain sensitivity even when the original hypothesis was exacerbation. For these clinical uses, the concentration tag `Therapeutic` denotes the eligible pain-effect investigation; preserve the actual hypothesis rather than implying intended or demonstrated relief. A concentration shared with an excluded reference component is retained once for its eligible use, without changing its purpose tag because of the reference use. Apply this boundary before comparison identification and grouping, equally to favourable, null, and adverse-direction findings. If components cannot be separated, preserve the limitation rather than inventing values.

These are already included studies. An unexpected apparent eligibility conflict needs a documented review decision; extraction alone does not authorise removing the study.

Use one value for genuine whole-study properties and all distinct eligible values for plural fields. Follow source order and remove duplicates. In fields with multiple entries, reserve the exact separator `; ` for separating entries, using commas within an entry, including parentheses. Keep construct-method, preparation-purpose, and comparator-role/type relationships together. Do not expand the form with outcome-specific columns; record repeated outcomes, regimens, calculations, and other detailed evidence in the study's combined note.

The CSV files use UTF-8 text encoding. When using a spreadsheet application, import all columns as text to preserve identifiers, diacritics, missingness labels, and structured entries exactly. Save the completed form as a UTF-8 CSV with the same 25 headings in the same order, one header row, and no extra blank rows or columns. Check the exported file before replacing the form.

`NI` means the concept applies but usable information could not be established after the relevant check. `N/A` means the evidence and field rules establish non-applicability. A blank completed-data cell is not a missingness category: it identifies unfinished work. Do not conceal an unresolved scientific decision as `NI`. Use only the missingness forms permitted for that field; `NI (description)` identifies an unresolved component only where scoped missingness is allowed. Resolve an unlisted category by reviewing the dictionary and affected analysis together, rather than adding an unexplained label or forcing it into an unsuitable category.

Determine design and denominator from the full eligible allocation structure. Randomisation of one subset of conditions does not make the complete investigation randomised. Prefer the supported randomised count, including a count established from participant flow or complete arm accounting. Only when it cannot be established may the explicitly reported enrolled count for the same investigation be used, labelled `allocated_or_enrolled`. For other designs, follow the corresponding denominator basis in the dictionary. Do not substitute screened, analysed, or completer totals, or duplicate people across experiments or periods.

## 3. Descriptive measurements, preparations, and comparators

Record every eligible pain-related construct and measurement family, whether or not it supplies a countable efficacy comparison. The narrower outcome rules in Section 4 do not remove valid descriptive measurements. For example, a pain-related mixed composite, PCS cognition, or LANSS screening can remain descriptive while its total score is excluded from analgesic efficacy counts. ODI and RMDQ require their source-supported pain-related disability construct; neither instrument name alone establishes comparison eligibility. WOMAC's identified pain subscale can qualify independently of other subscales. Explicitly pain-free mouth opening differs from generic mouth opening. Ruler measurement can describe the former; an inseparable OHIP-14 total remains a mixed composite.

The stored method abbreviations mean:

| Label | Meaning |
| --- | --- |
| BPI | Brief Pain Inventory |
| LANSS | Leeds Assessment of Neuropathic Symptoms and Signs |
| LMS | Labelled magnitude scale |
| McGill PQ | McGill Pain Questionnaire |
| NPSI | Neuropathic Pain Symptom Inventory |
| NRS | Numeric rating scale |
| ODI | Oswestry Disability Index |
| OHIP-14 | Oral Health Impact Profile |
| PCS | Pain Catastrophising Scale |
| RMDQ | Roland-Morris Disability Questionnaire |
| VAS | Visual analogue scale |
| WOMAC | Western Ontario and McMaster Universities Arthritis Index |

Other stored method labels are written out. Use the source-supported response method when the instrument name is unknown, and retain that limitation. For threshold procedures, the participant's response method, such as a verbal report or button press, is distinct from the stimulus apparatus. Force measurement is retained for an explicitly force-defined pain-tolerance endpoint, not inferred from every algometer reading.

Pharmaceutical-form terminology is adapted from Barnes and colleagues (2021), Table 1 and Sections 4.1–4.7. Prefer the reported form supported by its formulation description. The following descriptions make the classification usable without requiring the source review:

| Form | Practical interpretation |
| --- | --- |
| Ointment | Semisolid preparation with a predominantly greasy base, ordinarily low in water and volatile components. |
| Cream | Semisolid emulsion containing aqueous and oily phases. |
| Gel | Semisolid solution or dispersion stiffened by a gelling agent. |
| Lotion | Liquid emulsion or suspension, generally with a substantial aqueous component. |
| Solution | Homogeneous liquid preparation; includes peppermint oil diluted in a separate oil carrier. |
| Foam | Preparation that forms a foam on release, commonly from a pressurised container. |
| Spray | Preparation delivered as a spray; describe the reported formulation in the evidence note. |
| Ground peppermint | Ground plant material, not an inferred extract. |
| Ground menthol | Menthol applied in its reported ground solid form. |
| Peppermint oil | Explicitly neat peppermint oil without a separate carrier. |

The last three are source-based non-vehicle categories. Do not infer a formulation solely from its product name, an assumed composition, or the site of application. If formulation evidence is insufficient, use the dictionary's `NI` rule; a genuinely new form requires review.

For concentration, retain preparation-to-value-to-purpose links before aggregating. Preserve all reported qualifiers and percentage bases. `w/w`, `w/v`, `v/v`, and `v/w` mean mass per mass, mass per volume, volume per volume, and volume per mass, respectively. A conversion needs all required source inputs and a recorded formula. A peppermint or product percentage is not a menthol percentage. Do not infer typical menthol content. Retain masking concentrations descriptively even when a standalone finding is excluded.

Keep five judgements distinct: menthol's purpose in the actual preparation, the whole comparator regimen's intended role, comparison eligibility, estimate availability, and precision availability. Dose magnitude, non-zero menthol, or a nominal control label cannot settle them. Another therapeutic ingredient does not confer therapeutic purpose on masking menthol. Multiple purposes may be aggregated under one concentration tag, but efficacy eligibility still requires preparation-specific evidence.

Shared substantive co-interventions differ from additions confined to one arm. Remove the focal menthol and comparator components before deciding whether another exposure is shared. Do not infer unreported ingredients. A shared trace masking component whose possible outcome contribution is expressly raised by the report remains documented with its effect uncertain; masking does not prove inertness. Keep distinct comparator regimens separate, and a therapeutic combination together with its applicable types. Repetition across measurements or occasions does not alone create a new regimen.

## 4. Identify and group reported efficacy findings

First map the reported pain evidence across the primary article and formal supplements: measures, treatment arms or conditions, populations, occasions, and relevant tables, figures, and text. For each item, document whether it contributes a confirmed finding, repeats an existing finding, is excluded, or remains unresolved. This checks omissions as well as support.

**Outcome and treatment scope.** Include pain-only composites, direct pain-attributed interference, total BPI, and their reported response rates. Explicitly pain-free mouth opening is eligible; generic mouth opening is not. Exclude inseparable pain/non-pain composites, generic function, mood, cognition, and diagnostic screening. Retain a separately reported eligible pain component without inventing unreported component content.

A counted finding must address menthol or peppermint intended or investigated as a therapeutic component in at least one compared condition, within the investigation boundary above. Exclude findings confined to preparations where menthol or peppermint serves solely masking/control purposes, including their standalone before/after responses. Therapeutic menthol versus masking control can qualify under the remaining rules. Preserve preparation-specific uncertainty rather than deciding from aggregated concentration tags. Masking-only exclusions do not establish biological inactivity or study ineligibility, and do not remove valid descriptive preparation data.

Comparisons may use a control/reference condition, an intended therapeutic alternative that replaces menthol, or a pre-intervention condition. Exclude planned-only outcomes, unrelated populations, standalone pre-exposure differences, arbitrary cross-time subtraction, analyses principally about another treatment, and comparisons where another outcome-relevant component changes beyond the intended contrast unless genuinely shared. For example, massage plus menthol versus the same massage alone can isolate the added menthol contrast; change after simultaneously starting both treatments does not.

Exclude general aggregate or heterogeneity analyses that do not estimate the topical-menthol comparison. An omnibus label alone neither includes nor excludes a finding: assess the actual contrast. A general equality test jointly comparing menthol and two non-menthol groups does not itself identify a menthol-specific contrast. Assess a separately reported focal menthol contrast on its own. An explicit within-menthol time-profile finding or direct menthol-versus-comparator effect across occasions may qualify.

**Explicit reporting.** Count a comparison only when the authors communicate a comparative study finding and the report identifies the compared conditions and eligible pain outcome. Wording, table labels, figure annotations, or statistical results may establish this. Context can clarify identity; study design or separate arm/condition values alone cannot create a comparison. Qualitative or scoped p-value-only findings, including p > 0.05, may qualify without an estimate, statistical significance, or a separately printed test. A detached p value cannot identify or enumerate comparisons. A clearly labelled graphical contrast can establish reporting; separate arm plots alone cannot.

Preserve broad or unspecified scope without inventing detail or requiring exact timing. Do not expand an omnibus result into unreported pairs, a broad statement into every occasion, or two within-arm before/after findings into a between-arm change comparison. Retain the full scope of a condition effect spanning pre- and post-treatment; do not relabel it post-treatment-only or baseline-adjusted.

**Grouping.** Identify findings before grouping them. Describe each confirmed comparison by treatment contrast/regimen, shared co-intervention, target population, pain measure/outcome definition, task/site, and occasion or scope. Retain genuinely distinct reported comparators, occasions, instruments, response thresholds, and other scope differences. Group repeated presentations and alternative analyses of the same comparison, preserving their sources. A unit conversion adds no comparison. Adjusted/unadjusted or intention-to-treat/per-protocol labels alone do not establish sameness; equal estimates do not prove duplication and different estimates do not prove distinctness.

Group a between-arm final-score comparison with its baseline-to-that-visit change comparison only when both address the same treatment effect, pain measure, target population, and follow-up occasion. This can apply in randomised trials but is not automatic across designs, intervals, or populations. A within-menthol change differs from a between-treatment comparison. A defined cumulative or time-profile finding differs from a recap of already counted visits. Do not arbitrarily assign an unclear narrative to endpoint or change, or count a possible additional finding until confirmed distinct. These are grouping conventions, not a formal estimand count; do not demand or invent unreported estimand attributes.

**Descriptive case reports.** Count distinct explicitly reported pain responses or assessments after eligible therapeutic topical menthol or peppermint, even without a comparator or numerical change. Apply the other source, outcome, treatment-component, and grouping rules. Treat improvement, no change, and worsening alike. Do not assign one automatic finding per patient/report, infer change from a lone post-treatment rating, or expand a broad response into unreported occasions. Apply the exception to actual descriptive design, not sample size alone: a comparative single-patient trial follows its actual design. Both availability-count fields are `N/A`, even if the result count is 0 or `NI`; numerical observations may still be recorded in evidence.

## 5. Assess estimate and precision availability

Settle the confirmed grouped inventory before assessing availability. A calculation can quantify an already identified comparison but cannot create another one. Any permitted usable representation within a grouped comparison can establish an estimate; count the comparison once. Preserve the representation, inputs, units, population, occasion, formula, and source. A numerical effect of zero is available. A p value, test statistic, direction, or qualitative finding alone is not an effect estimate. Clearly labelled graphical contrast estimates count as reported estimates without digitisation or printed numerical labels; recognising presence does not require reading off magnitude.

Count precision only when a permitted representation supplies its estimate and matching, design-appropriate confidence interval, standard error, or variance. Match the effect, analysis, population, occasion, and design. Never combine a final-score estimate with change-score precision, or an adjusted estimate with an incompatible unadjusted interval. No modelling, imputation, graph digitisation, or assumptions about unreported denominators, inputs, or correlations are permitted. Separate arm/time-point standard deviations alone do not establish paired-change or crossover precision. A multi-group p value does not establish pairwise precision. An explicitly reported participant-level summary, such as individual area under the curve, can supply arm means and standard deviations without reconstructing time-series covariance; verify what the summary and variability represent.

An interval with an invalid documented method is not usable merely because it is printed. Keep that limitation without removing a qualifying comparison or its available estimate. This is a reporting-availability assessment, not general risk-of-bias assessment or selection of a preferred analysis for synthesis. Missing precision does not remove an estimate; missing estimates or precision do not remove a qualifying comparison.

Reconcile all three counts to the inventory. Four confirmed comparisons remain 4 when a possible fifth is unresolved or material is missing; two confirmed estimates and one matching precision remain 2 and 1 despite uncertain additional availability. Preserve possible additions, source limitations, and proportionate recovery attempts. These minimum-count rules are not permission to skip available sources.

Use 0 only after adequate checking establishes none for that field, and `NI` only when neither a positive confirmed count nor a supported zero is established. Assess fields independently; do not propagate `NI` automatically. Where numeric, precision ≤ estimates ≤ results. Every precision-qualified comparison also has an available estimate. For a non-case study, result count 0 requires both availability counts 0, and estimate count 0 requires precision count 0. A positive precision count requires positive confirmed estimate and result counts. Descriptive cases retain both `N/A` values independently of their result count. A zero effect, zero available estimates, and zero reported comparisons are different concepts.

For each inventory entry, record availability as available, unavailable after checking, unresolved, or not applicable for a descriptive case. Precision available requires estimate available; estimate unavailable requires precision unavailable; estimate unresolved can coexist with precision unavailable. Confirmed aggregate counts can coexist with unresolved entries. An empty inventory requires a focused source-and-rule check, but a supported zero is valid. If further reading establishes an omission, revise the inventory transparently before recounting.

Any later availability percentage uses the counted comparison inventory, not only comparisons with estimates. A 0/0 percentage is undefined. Percentages do not establish complete ascertainment and need not be minimum percentages for the entire study.

## 6. Registration and geography

For a study without reusable dated registration evidence, inspect the article, supplements, and any matched protocol for identifiers and registration statements. Check reported identifiers in official registries. If no identifier resolves the question, make a bounded, study-specific search of the WHO International Clinical Trials Registry Platform and relevant official registries, such as ClinicalTrials.gov or the named national registry, using title fragments and appropriate combinations of intervention, investigator, site, dates, and sample characteristics. Do not treat a similar title alone as a match. The search is for this study's registration, not a literature-search update.

Record the search date, registries, exact queries, candidate records and their disposition, URLs, record version inspected, match rationale, first-submission date, first-enrolment date, and limitations. Stop after the article identifiers and pertinent study-specific combinations have been checked in the relevant accessible registries; retain any access failures or uncertainty. The qualified no-match category means that no sufficiently matched public record was located within that documented search. It is not proof of non-registration. Use `NI` where the evidence cannot support a classification. Do not invent a historical search date or expand a reused search's stated coverage.

Use the first submission date where available to assess timing. Registration must precede the supported first-enrolment period to be prospective. Registration after enrolment began, or explicitly while recruiting, is retrospective. Overlapping partial dates may leave timing unresolved; do not invent exact dates or call any located registry entry prospective without timing evidence.

For geography, follow the dictionary's conduct-location hierarchy. A targeted authoritative institution lookup may resolve a named site's country; record the inference and URL. Use preferred country names from `who_country_regions.csv`, including Iran, Turkey, United Kingdom, and United States. Retain source wording in the evidence note. The lookup's `country_code` contains WHO country/territory identifiers, which are not uniformly ISO 3166-1 alpha-3 codes. Do not infer a region from the code alone.

The lookup preserves the WHO Global Health Observatory country-to-region response accessed on **2026-08-04**, with **228 mapped entries**. `source_country_name` retains the WHO wording; `country` gives the form's preferred name; `aliases` preserves additional known source aliases. Use `country` in completed rows. Region membership is preserved from that dated response, while region names are expanded for readability:

| Code | Region |
| --- | --- |
| AFR | African Region |
| AMR | Region of the Americas |
| SEAR | South-East Asia Region |
| EUR | European Region |
| EMR | Eastern Mediterranean Region |
| WPR | Western Pacific Region |

Six entries in that response lacked one of these six parent-region codes and are not assigned a region in the lookup: CHI (Channel islands), HKG (China, Hong Kong Special Administrative Region), MAC (China, Macao Special Administrative Region), ME1 (The former state union Serbia and Montenegro), PRS (Pristina), and XKX (Kosovo, in accordance with UN Security Council resolution 1244 (1999)). These exclusions describe limitations of the dated response, not a judgement about geography. If a known study location cannot be matched, retain its source evidence and review the mapping explicitly before completing the country/region fields. Do not turn a known country into `NI` solely because the lookup lacks it, guess a region, or silently replace the dated classification with a newer one. An actually unresolved conduct country is `NI`, mirrored once in the derived region field.

## 7. Adverse-event reporting and final check

Read adverse-event, tolerability, and relevant withdrawal or treatment-change information across every eligible menthol exposure, including combinations. Apply the dictionary's status precedence: a reported event overrides negative statements. Restrict negative statements to their documented groups, periods, and event scope. Tolerability-only or treatment-related-only negatives are not clear overall zeros. Ordinary planned pain outcomes and general pharmacological background do not automatically supply adverse-event reporting. A withdrawal or non-adherence statement without a health-related reason is not itself adverse-event information. Preserve event numbers, units, denominators, timing, and investigator attribution without inventing them; sample size is not a substitute for an unstated safety denominator.

Make one focused source-to-row check, including study identity, all 25 fields, permitted values and separators, denominator basis, all distinct eligible preparations and measurements, comparison support and omissions, count relationships, WHO derivation, and adverse-event status/details agreement. Check both directions: each retained value needs support, and each relevant pain question needs a recorded disposition. Correct a reading or formatting error and repeat the affected check. Do not replace a genuine unresolved scientific issue with a guessed category or repeated general audits. Preserve unaffected values and state the evidence, options, and recommended resolution for a substantive ambiguity.

A completed proposed row has no blank cells and no unresolved scientific decision disguised as missingness. Source-reporting limitations already handled by the permitted minimum-count or `NI` rules may remain, with their scope documented. A reviewer should compare the row with the source-based note before integrating it into the form. If a category or rule changes, update the dictionary, these instructions where relevant, and affected scripts together; check existing rows and new-study examples before regenerating results.

## Evidence-note template

Use one note per study, with tables only where they improve clarity. Related fields may share one evidence entry. Record enough detail to reproduce judgements and supported calculations without copying whole passages.

1. **Study and sources:** record identifier; full citation; primary article, formal supplements, and supporting sources with filenames/URLs; their roles; pages/material inspected; access or translation limitations; extraction and review dates.
2. **Field evidence:** field or related fields; proposed value; exact source locator; concise support; calculation or inference if used. Include the complete eligible investigation boundary, allocation and denominator, all preparation/concentration/purpose links, comparator composition and intended role, and justification for `NI`/`N/A`.
3. **Confirmed comparison inventory:** one numbered entry per distinct included comparison or descriptive case finding. Record treatment contrast, shared co-intervention, population, outcome and method, task/site, occasion or honest unspecified scope, reporting locator, and grouping rationale. For non-cases, record estimate and precision availability separately, the supporting compatible analysis representation, and any calculation with inputs, units, formula, and limitations. Do not invent a comparator, numerical effect, or exact time just to fill the note.
4. **Exclusions, repeated presentations, and unresolved additions:** source item; disposition; linked inventory entry if repeated; source-based reason. Include masking-only findings, separate pain-model reference components, non-eligible outcomes or treatment contrasts, and possible additional findings whose distinctness is unresolved.
5. **Coverage and reconciliation:** each relevant pain table, figure, and section with its inventory/exclusion disposition; the three derived counts; confirmation that omissions as well as support were checked. Record an unavailable source and recovery attempts rather than pretending it was inspected.
6. **Registration and location evidence:** actual search dates and queries or explicitly dated reused evidence; candidate decisions; timing and matching rationale; targeted site lookup if needed; country-to-region derivation.
7. **Outstanding work and review:** genuine unfinished fields or decisions, options and recommendation, or a statement that none remains. Distinguish completed extraction with documented source limitations from unfinished work. Record the reviewer's source check and accepted corrections before integration.

## Methodological sources

These references inform the classifications; the operational rules above and in the dictionary are this review's adaptations, not verbatim external taxonomies.

- Parker RA and Berman NG (2016). *Planning Clinical Research*. Cambridge University Press. Chapter 4, *Overview of Study Designs*, pp. 47–61, especially pp. 47–54; and Chapter 10, *Selecting a Design*, pp. 119–133, especially pp. 124–127. [Overview](https://doi.org/10.1017/CBO9781139024716.005); [Selecting a Design](https://doi.org/10.1017/CBO9781139024716.011). Design definitions are supplied in the dictionary.
- Barnes TM, Mijaljica D, Townley JP, Spada F, and Harrison IP (2021). Vehicles for Drug Delivery and Cosmetic Moisturizers: Review and Comparison. *Pharmaceutics*, 13, 2012. [Article](https://doi.org/10.3390/pharmaceutics13122012), Table 1 and Sections 4.1–4.7. Practical form definitions are supplied above.
- McKenzie JE, Brennan SE, Ryan RE, Thomson HJ, Johnston RV, and Thomas J. *Defining the criteria for including studies and how they will be grouped for the synthesis*, last updated August 2023. In: *Cochrane Handbook for Systematic Reviews of Interventions*, version 6.5 (2024), Chapter 3, especially Sections 3.2.2 and 3.2.3.1, on intervention, comparator, and co-intervention boundaries. [Chapter 3](https://www.cochrane.org/authors/handbooks-and-manuals/handbook/current/chapter-03). The present labels describe reported intended roles and do not establish efficacy or inactivity.
- Peryer G, Golder S, Junqueira D, Vohra S, and Loke YK. *Adverse effects*, last updated October 2019. In: *Cochrane Handbook for Systematic Reviews of Interventions*, version 6.5 (2024), Chapter 19, especially Sections 19.1.1, 19.2.4, and 19.5.2, on terminology, withdrawal information, and zero-event interpretation. [Chapter 19](https://www.cochrane.org/authors/handbooks-and-manuals/handbook/current/chapter-19). The dictionary supplies the review's reporting categories and precedence rules.
- World Health Organization, Global Health Observatory, country dimension response accessed 2026-08-04. [Source API](https://ghoapi.azureedge.net/api/DIMENSION/COUNTRY/DimensionValues?$format=json). The supplied CSV preserves that dated membership and the limitations described above; the live source may subsequently change.
- Official registration sources: [WHO International Clinical Trials Registry Platform](https://www.who.int/clinical-trials-registry-platform) and [ClinicalTrials.gov](https://clinicaltrials.gov/), supplemented by the relevant official national registry for an individual study. Record the actual registry and search evidence used.
