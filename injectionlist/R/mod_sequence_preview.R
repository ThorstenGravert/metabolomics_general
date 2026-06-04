mod_sequence_preview_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::tagList(
          shiny::helpText("You can edit `final_sample_name`, `position`, `inj_vol`, and `comment` directly in the table. Regenerating the sequence resets manual edits."),
          shiny::actionButton(ns("reset_sequence_edits"), "Reset manual edits")
        )
      )
    ),
    DT::DTOutput(ns("sequence_table"))
  )
}

mod_sequence_preview_server <- function(id, sequence, batch_info) {
  shiny::moduleServer(id, function(input, output, session) {
    editable_columns <- c("final_sample_name", "position", "inj_vol", "comment")
    edited_sequence <- shiny::reactiveVal(NULL)
    table_proxy <- DT::dataTableProxy(session$ns("sequence_table"))

    shiny::observeEvent(sequence(), {
      edited_sequence(sequence())
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$reset_sequence_edits, {
      edited_sequence(sequence())
    })

    shiny::observeEvent(input$sequence_table_cell_edit, {
      info <- input$sequence_table_cell_edit
      current <- edited_sequence()
      shiny::req(current)

      column_name <- names(current)[info$col + 1L]
      if (!column_name %in% editable_columns) {
        return()
      }

      old_value <- current[[column_name]][info$row]
      new_value <- DT::coerceValue(info$value, old_value)
      current[[column_name]][info$row] <- new_value

      if (identical(column_name, "final_sample_name") || identical(column_name, "inj_vol")) {
        current <- finalize_sequence_fields(current, batch_info())
      } else {
        current <- finalize_sequence_fields(current, batch_info())
      }

      edited_sequence(current)
      DT::replaceData(table_proxy, edited_sequence(), resetPaging = FALSE, rownames = FALSE)
    })

    output$sequence_table <- DT::renderDT({
      current <- edited_sequence()
      shiny::req(current)

      DT::datatable(
        current,
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = which(!names(current) %in% editable_columns) - 1L))
      )
    })

    shiny::reactive(edited_sequence())
  })
}
