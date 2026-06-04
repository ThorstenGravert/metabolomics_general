mod_sample_input_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::textAreaInput(
          ns("pasted_samples"),
          "Paste sample table",
          rows = 14,
          placeholder = "Paste a table copied from Excel here"
        ),
        shiny::actionButton(ns("parse_paste"), "Parse pasted table")
      ),
      shiny::column(
        width = 6,
        shiny::fileInput(ns("sample_file"), "Upload sample file", accept = c(".csv", ".xlsx", ".xls")),
        shiny::helpText("Supported inputs: simple CSV tables and the Sample Submission Example workbook format.")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        DT::DTOutput(ns("sample_preview"))
      )
    )
  )
}

mod_sample_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    parsed_paste <- shiny::eventReactive(input$parse_paste, {
      parse_pasted_samples(input$pasted_samples)
    }, ignoreNULL = FALSE)

    uploaded_samples <- shiny::reactive({
      shiny::req(input$sample_file)
      read_sample_input_file(input$sample_file$datapath)
    })

    combined_samples <- shiny::reactive({
      if (!is.null(input$sample_file)) {
        uploaded_samples()
      } else {
        parsed_paste()
      }
    })

    output$sample_preview <- DT::renderDT({
      DT::datatable(combined_samples(), options = list(pageLength = 10, scrollX = TRUE))
    })

    list(samples = combined_samples)
  })
}
