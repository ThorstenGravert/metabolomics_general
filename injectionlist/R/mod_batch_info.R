mod_batch_info_ui <- function(id) {
  ns <- shiny::NS(id)
  vocab <- lc_controlled_vocabularies()

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::wellPanel(
          shiny::tags$h4("Project identifiers"),
          ui_labeled_control(
            shiny::textInput(ns("project_number"), "Project number", value = "CMDXXXXXX"),
            "Used in the export filename and the default instrument data path."
          ),
          ui_labeled_control(
            shiny::numericInput(ns("batch_number"), "Batch number", value = 1, min = 1, step = 1),
            "Use one file per analytical batch."
          ),
          ui_labeled_control(
            shiny::numericInput(ns("version_number"), "Version number", value = 1, min = 1, step = 1),
            "Increase this when a batch is rerun but should keep the same batch number."
          ),
          ui_labeled_control(
            shiny::numericInput(ns("injection_offset"), "Injection number offset", value = 0, min = 0, step = 1),
            "Use this when adding injections after a previous partial run."
          ),
          ui_labeled_control(
            shiny::textInput(ns("column_id"), "Column ID", value = "CYYY"),
            "This label is appended to SST names in the exported sequence."
          )
        )
      ),
      shiny::column(
        width = 4,
        shiny::wellPanel(
          shiny::tags$h4("Acquisition setup"),
          ui_labeled_control(
            shiny::selectInput(ns("method_family"), "Method", choices = vocab$method, selected = "RP"),
            "Controlled vocabulary matching the LC method family."
          ),
          ui_labeled_control(
            shiny::selectInput(ns("instrument"), "Instrument", choices = vocab$instrument, selected = "XXX"),
            "Used in file naming and method lookup."
          ),
          ui_labeled_control(
            shiny::dateInput(ns("start_date"), "Start date", value = Sys.Date()),
            "Export filenames use the compact date code derived from this date."
          ),
          ui_labeled_control(
            shiny::numericInput(ns("sample_inj_vol"), "Injection volume (μl) for samples", value = 5, min = 0),
            "Default injection volume for study samples."
          ),
          ui_labeled_control(
            shiny::selectInput(ns("standard_status"), "Standard / non-standard", choices = vocab$standard_status, selected = "Standard"),
            "Non-standard disables method auto-fill in the export."
          )
        )
      ),
      shiny::column(
        width = 4,
        shiny::wellPanel(
          shiny::tags$h4("Container and export"),
          ui_labeled_control(
            shiny::selectInput(ns("autosampler"), "Plate / vials", choices = vocab$autosampler, selected = "Plate"),
            "Select `Plate` for 96-well plates or `Vial` for 6x9 vial racks. This affects validation and the plate map."
          ),
          ui_labeled_control(
            shiny::selectInput(ns("acquisition_mode"), "Export mode", choices = vocab$acquisition_mode, selected = "PAN"),
            "Sets the export naming mode used for the final instrument file."
          ),
          ui_labeled_control(
            shiny::textInput(ns("instrument_method"), "Instrument method path", value = ""),
            "Optional explicit path for the instrument method written into the export file."
          ),
          ui_labeled_control(
            shiny::textInput(ns("client_name"), "Client", value = ""),
            "Written into the export metadata columns."
          ),
          ui_labeled_control(
            shiny::textInput(ns("laboratory_name"), "Laboratory", value = "MPS"),
            "Written into the export metadata columns."
          )
        )
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
