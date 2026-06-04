mod_batch_info_ui <- function(id) {
  ns <- shiny::NS(id)
  vocab <- lc_controlled_vocabularies()

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::textInput(ns("project_number"), "Project number", value = "CMDXXXXXX"),
        shiny::numericInput(ns("batch_number"), "Batch number", value = 1, min = 1, step = 1),
        shiny::numericInput(ns("version_number"), "Version number", value = 1, min = 1, step = 1),
        shiny::numericInput(ns("injection_offset"), "Injection number offset", value = 0, min = 0, step = 1),
        shiny::textInput(ns("column_id"), "Column ID", value = "CYYY")
      ),
      shiny::column(
        width = 4,
        shiny::selectInput(ns("method_family"), "Method", choices = vocab$method, selected = "RP"),
        shiny::selectInput(ns("instrument"), "Instrument", choices = vocab$instrument, selected = "XXX"),
        shiny::dateInput(ns("start_date"), "Start date", value = Sys.Date()),
        shiny::numericInput(ns("sample_inj_vol"), "Injection volume for samples", value = 5, min = 0),
        shiny::selectInput(ns("standard_status"), "Standard / non-standard", choices = vocab$standard_status, selected = "Standard")
      ),
      shiny::column(
        width = 4,
        shiny::selectInput(ns("autosampler"), "Plate / vials", choices = vocab$autosampler, selected = "Plate"),
        shiny::selectInput(ns("acquisition_mode"), "Export mode", choices = vocab$acquisition_mode, selected = "PAN"),
        shiny::textInput(ns("instrument_method"), "Instrument method path", value = ""),
        shiny::textInput(ns("client_name"), "Client", value = ""),
        shiny::textInput(ns("laboratory_name"), "Laboratory", value = "Metabolomics")
      )
    )
  )
}

mod_batch_info_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      list(
        project_number = input$project_number,
        batch_number = as.integer(input$batch_number),
        version_number = as.integer(input$version_number),
        injection_offset = as.integer(input$injection_offset),
        column_id = input$column_id,
        method_family = input$method_family,
        instrument = input$instrument,
        start_date = input$start_date,
        start_date_code = format(as.Date(input$start_date), "%y%m%d"),
        sample_inj_vol = as.numeric(input$sample_inj_vol),
        standard_status = input$standard_status,
        autosampler = input$autosampler,
        acquisition_mode = input$acquisition_mode,
        instrument_method = input$instrument_method,
        export_path = make_project_path(list(
          project_number = input$project_number,
          batch_number = as.integer(input$batch_number),
          version_number = as.integer(input$version_number)
        )),
        client_name = input$client_name,
        laboratory_name = input$laboratory_name,
        company_name = "",
        phone = ""
      )
    })
  })
}
