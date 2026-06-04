mod_sequence_preview_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    DT::DTOutput(ns("sequence_table"))
  )
}

mod_sequence_preview_server <- function(id, sequence) {
  shiny::moduleServer(id, function(input, output, session) {
    output$sequence_table <- DT::renderDT({
      DT::datatable(
        sequence(),
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
}
