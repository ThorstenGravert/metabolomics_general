mod_sequence_settings_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::wellPanel(
          shiny::tags$h4("Sample order"),
          shiny::checkboxInput(ns("randomize_samples"), "Randomize samples", value = TRUE),
          shiny::numericInput(ns("random_seed"), "Random seed", value = 1, min = 1, step = 1)
        ),
        shiny::wellPanel(
          shiny::tags$h4("QC settings"),
          shiny::checkboxInput(ns("qc_enabled"), "Insert pooled QCs", value = TRUE),
          shiny::numericInput(ns("qc_frequency"), "Samples between QCs", value = 6, min = 1, step = 1),
          shiny::numericInput(ns("qc_inj_vol"), "QC injection volume", value = 5, min = 0),
          shiny::numericInput(ns("qc_start_column"), "QC start column", value = 1, min = 1, max = 12, step = 1)
        )
      ),
      shiny::column(
        width = 4,
        shiny::wellPanel(
          shiny::tags$h4("Blank settings"),
          shiny::checkboxInput(ns("blank_enabled"), "Insert blanks", value = TRUE),
          shiny::numericInput(ns("blank_frequency"), "Samples between blanks", value = 12, min = 1, step = 1),
          shiny::numericInput(ns("blank_inj_vol"), "Blank injection volume", value = 5, min = 0),
          shiny::numericInput(ns("start_blank_count"), "Start blanks", value = 1, min = 0, step = 1),
          shiny::numericInput(ns("end_blank_count"), "End blanks", value = 1, min = 0, step = 1),
          shiny::numericInput(ns("blank_start_column"), "Blank start column", value = 3, min = 1, max = 12, step = 1)
        ),
        shiny::wellPanel(
          shiny::tags$h4("Calibration settings"),
          shiny::numericInput(ns("start_calibration_count"), "Calibration standards at start", value = 0, min = 0, step = 1),
          shiny::numericInput(ns("calibration_inj_vol"), "Calibration injection volume", value = 5, min = 0),
          shiny::numericInput(ns("calibration_start_column"), "Calibration start column", value = 2, min = 1, max = 12, step = 1)
        )
      ),
      shiny::column(
        width = 4,
        shiny::wellPanel(
          shiny::tags$h4("Run blocks"),
          shiny::numericInput(ns("start_wash_count"), "Start washes", value = 2, min = 0, step = 1),
          shiny::numericInput(ns("end_wash_count"), "End washes", value = 1, min = 0, step = 1),
          shiny::numericInput(ns("start_qc_count"), "Start QCs", value = 1, min = 0, step = 1),
          shiny::numericInput(ns("end_qc_count"), "End QCs", value = 1, min = 0, step = 1),
          shiny::numericInput(ns("start_sst_count"), "Start SSTs", value = 1, min = 0, step = 1),
          shiny::numericInput(ns("end_sst_count"), "End SSTs", value = 1, min = 0, step = 1)
        ),
        shiny::wellPanel(
          shiny::tags$h4("Support plate layout"),
          shiny::textInput(ns("support_plate"), "Support plate ID", value = "G"),
          shiny::numericInput(ns("sst_start_column"), "SST start column", value = 4, min = 1, max = 12, step = 1),
          shiny::numericInput(ns("wash_start_column"), "Wash start column", value = 5, min = 1, max = 12, step = 1)
        )
      )
    )
  )
}

mod_sequence_settings_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      list(
        randomize_samples = isTRUE(input$randomize_samples),
        random_seed = as.integer(input$random_seed),
        qc_settings = list(
          enabled = isTRUE(input$qc_enabled),
          frequency = as.integer(input$qc_frequency),
          inj_volume = as.numeric(input$qc_inj_vol),
          start_column = as.integer(input$qc_start_column)
        ),
        blank_settings = list(
          enabled = isTRUE(input$blank_enabled),
          frequency = as.integer(input$blank_frequency),
          inj_volume = as.numeric(input$blank_inj_vol),
          start_column = as.integer(input$blank_start_column)
        ),
        calibration_settings = list(
          start_count = as.integer(input$start_calibration_count),
          inj_volume = as.numeric(input$calibration_inj_vol),
          start_column = as.integer(input$calibration_start_column)
        ),
        start_settings = list(
          wash_count = as.integer(input$start_wash_count),
          blank_count = as.integer(input$start_blank_count),
          qc_count = as.integer(input$start_qc_count),
          sst_count = as.integer(input$start_sst_count),
          wash_start_column = as.integer(input$wash_start_column),
          sst_start_column = as.integer(input$sst_start_column)
        ),
        end_settings = list(
          wash_count = as.integer(input$end_wash_count),
          blank_count = as.integer(input$end_blank_count),
          qc_count = as.integer(input$end_qc_count),
          sst_count = as.integer(input$end_sst_count)
        ),
        support_layout_settings = list(
          plate = stringr::str_to_upper(stringr::str_trim(input$support_plate))
        )
      )
    })
  })
}
