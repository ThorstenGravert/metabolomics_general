mod_validation_panel_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::uiOutput(ns("export_controls")),
    DT::DTOutput(ns("validation_table"))
  )
}

mod_validation_panel_server <- function(id, validations, sequence, batch_info, export_ready) {
  shiny::moduleServer(id, function(input, output, session) {
    output$validation_table <- DT::renderDT({
      DT::datatable(validations(), options = list(dom = "tip", pageLength = 20), rownames = FALSE)
    })

    output$export_controls <- shiny::renderUI({
      if (isTRUE(export_ready())) {
        shiny::tagList(
          shiny::downloadButton(session$ns("download_csv"), "Export CSV"),
          shiny::helpText("Export uses the LC template column order and includes the leading 'Bracket Type=4' line.")
        )
      } else {
        shiny::helpText("Export is blocked until all errors are resolved.")
      }
    })

    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        make_timestamped_filename(batch_info()$project_number)
      },
      content = function(file) {
        shiny::req(export_ready())
        write_injection_csv(sequence(), batch_info(), file)
      }
    )
  })
}
