# LC Injection List Builder

This project contains the first local Shiny implementation of the LC injection-list workflow currently maintained in the Excel template `Injection lists LC_ver2_1.xltx`.

## Workbook Summary

The LC workbook is organized around three sequence parts:

1. `Pre block`
2. sample stream from `Samples`
3. `Post block`

The final `Injection list` sheet concatenates those parts into one run order and then builds strict export sheets for `POS`, `NEG`, and `PAN`.

### Confirmed Logic From The Workbook

- Mandatory batch inputs on `Batch info`:
  `Project number`, `Batch number`, `Version number`, `Injection number offset`, `Column ID`, `Method`, `Instrument`, `Start date`, `Injection volume samples`, `Standard/non-standard`, and `Plate/Vials`.
- Controlled vocabularies:
  `Method` comes from `RP`, `HILIC`, `BA`, `Other`.
  `Instrument` comes from `XXX`, `LCDK1`, `LCDK2`, `LCDK3`, `OEGT1`, `OEGT2`.
  `Standard/non-standard` comes from `Standard`, `Nonstandard`.
  `Plate color` on `Pre block` and `Post block` comes from `R`, `B`, `G`, `Y`.
  `Plate/Vials` comes from `Plate`, `Vial`.
- Defaults visible in the workbook:
  `Samples between QC = 6`, `Batch number = 1`, `Version number = 1`, `Injection offset = 0`, `Column ID = CYYY`, `Method = RP`, `Instrument = XXX`, `Injection volume = 5`, `Standard = Standard`.
- Sample ordering:
  the workbook randomizes sample rows using `RAND()` and inserts a `QC` marker every `n` samples.
- Name rules:
  `QC` becomes sequential `QC_001`, `QC_002`, ...
  `Blank` becomes sequential `Blank_001`, `Blank_002`, ...
  `SST1` and `SST2` become `SST1_<Column ID>` and `SST2_<Column ID>`.
  regular samples beginning with `SA` export the trimmed ID prefix in the filename.
- Position rules:
  sample positions are taken from the sample name suffix in the workbook.
  QC positions are looked up from the `QC Positions` sheet.
  pre/post positions come directly from the `Pre block` and `Post block` sheets.
- Export format:
  the workbook export sheets begin with a leading line `Bracket Type=4`.
  row 2 headers are:
  `Sample Type`, `File Name`, `Sample ID`, `Path`, `Instrument Method`, `Process Method`, `Calibration File`, `Position`, `Inj Vol`, `Level`, `Sample Wt`, `Sample Vol`, `ISTD Amt`, `Dil Factor`, `L1 Study`, `L2 Client`, `L3 Laboratory`, `L4 Company`, `L5 Phone`, `Comment`.
- File-name pattern inside export rows:
  `<yymmdd>-<instrument>-<mode>-<batch>-<3-digit injection number>-<sample name>`
- Default export path pattern:
  `D:\Projects\<project>\instrument_files\B<batch>V<version>`

### Assumptions In This First Shiny Version

- The workbook contains many method-path mappings in `Do not edit`. The app keeps the strict export columns but asks for the instrument method path explicitly instead of hard-coding every Excel lookup.
- The workbook exposes multiple specialized QC and calibration patterns. The first app version implements configurable washes, blanks, SSTs, pooled QCs, and start calibrations as the stable baseline.
- The workbook has separate `POS`, `NEG`, and `PAN` export sheets. The app exports one CSV at a time using a selected mode.
- Hidden-sheet dropdown logic was reproduced as app controls rather than workbook validations.
- The workbook mentions AquireX, but the README explicitly says it does not work in this version. AquireX is not implemented in the app.

## Project Structure

```text
app.R
R/
  mod_batch_info.R
  mod_sample_input.R
  mod_plate_map.R
  mod_sequence_settings.R
  mod_sequence_preview.R
  mod_validation_panel.R
  sequence_builder.R
  validation.R
  export_csv.R
  utils.R
tests/
  testthat/
data/
  example_sample_list.csv
```

## Run Locally

Install the requested packages first, then run:

```r
Rscript -e "shiny::runApp()"
```

Run tests with:

```r
Rscript -e "testthat::test_dir('tests/testthat')"
```

## Current Scope

- LC only
- local Shiny app
- paste, CSV upload, or Sample Submission Excel upload
- sample randomization with seed
- configurable QC, blank, SST, wash, and calibration insertion
- preview, validation, and CSV export with timestamped file names

## Not Yet Implemented

- GC workflow
- full Excel-equivalent method lookup matrix
- Excel template import during normal operation
