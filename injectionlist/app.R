required_packages <- c(
  "shiny", "bslib", "DT", "readr", "dplyr", "tidyr", "stringr",
  "purrr", "tibble", "lubridate", "janitor", "shinyvalidate"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    paste(
      "Missing required packages:",
      paste(missing_packages, collapse = ", "),
      "\nInstall them before running the app."
    ),
    call. = FALSE
  )
}

invisible(lapply(list.files("R", pattern = "\\.[Rr]$", full.names = TRUE), source))

ui <- bslib::page_navbar(
  bslib::nav_panel(
    "Sample input",
    mod_sample_input_ui("sample_input")
  ),
  bslib::nav_panel(
    "Batch info",
    mod_batch_info_ui("batch_info")
  ),
  bslib::nav_panel(
    "Sequence settings",
    mod_sequence_settings_ui("sequence_settings")
  ),
  bslib::nav_panel(
    "Plate map / visual checks",
    mod_plate_map_ui("plate_map")
  ),
  bslib::nav_panel(
    "Sequence preview",
    mod_sequence_preview_ui("sequence_preview")
  ),
  bslib::nav_panel(
    "Validation and export",
    mod_validation_panel_ui("validation_panel")
  ),
  title = "LC Injection List Builder",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly")
)

server <- function(input, output, session) {
  sample_input <- mod_sample_input_server("sample_input")
  batch_info <- mod_batch_info_server("batch_info")
  sequence_settings <- mod_sequence_settings_server("sequence_settings")

  normalized_samples <- shiny::reactive({
    raw_samples <- sample_input$samples()
    shiny::req(raw_samples)
    standardize_sample_table(raw_samples)
  })

  sequence_data <- shiny::reactive({
    build_injection_sequence(
      samples = normalized_samples(),
      batch_info = batch_info(),
      qc_settings = sequence_settings()$qc_settings,
      blank_settings = sequence_settings()$blank_settings,
      calibration_settings = sequence_settings()$calibration_settings,
      start_settings = sequence_settings()$start_settings,
      end_settings = sequence_settings()$end_settings,
      support_layout_settings = sequence_settings()$support_layout_settings,
      randomize_samples = isTRUE(sequence_settings()$randomize_samples),
      random_seed = sequence_settings()$random_seed
    )
  })

  validation_messages <- shiny::reactive({
    validate_injection_workflow(
      samples = normalized_samples(),
      sequence = sequence_data(),
      batch_info = batch_info(),
      settings = sequence_settings()
    )
  })

  export_ready <- shiny::reactive({
    !any(validation_messages()$severity == "Error")
  })

  mod_plate_map_server(
    "plate_map",
    samples = normalized_samples,
    sequence = sequence_data,
    batch_info = batch_info
  )

  mod_sequence_preview_server(
    "sequence_preview",
    sequence = sequence_data
  )

  mod_validation_panel_server(
    "validation_panel",
    validations = validation_messages,
    sequence = sequence_data,
    batch_info = batch_info,
    export_ready = export_ready
  )
}

shiny::shinyApp(ui, server)
