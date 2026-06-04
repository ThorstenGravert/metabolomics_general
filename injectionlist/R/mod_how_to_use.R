mod_how_to_use_ui <- function(id) {
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 8,
        shiny::tags$h3("How to use the LC Injection List Builder"),
        shiny::tags$p("This app turns sample manifests or pasted sample tables into a validated LC-MS injection sequence."),
        shiny::tags$h4("Recommended workflow"),
        shiny::tags$ol(
          shiny::tags$li("Import your samples from the Sample Submission workbook, a CSV file, or a pasted table copied from Excel."),
          shiny::tags$li("Enter batch metadata such as project number, method, instrument, date, and acquisition mode."),
          shiny::tags$li("Adjust QC, blank, calibration, wash, and SST settings to match the run design."),
          shiny::tags$li("Inspect the sample plate and support plate layouts in the plate map view."),
          shiny::tags$li("Review the generated sequence and resolve any validation errors before export."),
          shiny::tags$li("Export the final CSV for instrument import.")
        ),
        shiny::tags$h4("Input formats"),
        shiny::tags$ul(
          shiny::tags$li("Sample Submission workbook: only rows with internal IDs starting with `SA` or `PB` are imported."),
          shiny::tags$li("Pasted or uploaded flat tables: use columns such as `Sample ID`, `Sample type`, `Plate`, `Well`, `Batch`, and `Comment`."),
          shiny::tags$li("Container formats: select `Plate` for 96-well plates or `Vial` for 6x9 vial racks.")
        )
      ),
      shiny::column(
        width = 4,
        shiny::wellPanel(
          shiny::tags$h4("Practical notes"),
          shiny::tags$ul(
            shiny::tags$li("Only real samples are randomized. QC, blanks, calibrations, washes, and SSTs are inserted afterward."),
            shiny::tags$li("Support injections are placed on the support plate by type-specific starting columns."),
            shiny::tags$li("The export button stays disabled when blocking validation errors are present."),
            shiny::tags$li("Use the plate map to confirm positions before exporting.")
          )
        ),
        shiny::wellPanel(
          shiny::tags$h4("Key terms"),
          shiny::tags$p(shiny::tags$b("Support plate:"), " the plate or rack used for QC, blanks, calibration standards, washes, and SSTs."),
          shiny::tags$p(shiny::tags$b("Acquisition mode:"), " the instrument export mode, such as `PAN`, `POS`, or `NEG`."),
          shiny::tags$p(shiny::tags$b("Injection offset:"), " adds numbering when extra injections are appended to an existing batch.")
        )
      )
    )
  )
}
