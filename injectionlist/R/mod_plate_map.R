mod_plate_map_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::uiOutput(ns("plate_map")),
    shiny::tableOutput(ns("summary_counts"))
  )
}

mod_plate_map_server <- function(id, samples, sequence, batch_info) {
  shiny::moduleServer(id, function(input, output, session) {
    output$plate_map <- shiny::renderUI({
      container_type <- batch_info()$autosampler %||% "Plate"
      spec <- get_container_spec(container_type)

      sample_map <- samples() |>
        standardize_sample_table() |>
        build_plate_map_data(container_type = container_type)

      support_map <- sequence() |>
        dplyr::filter(!sample_category %in% "sample") |>
        dplyr::transmute(
          sample_id = final_sample_name,
          sample_type = dplyr::case_when(
            sample_category %in% c("qc", "blank", "calibration", "sst") ~ sample_category,
            TRUE ~ "other"
          ),
          plate = stringr::str_extract(position, "^[^:]+"),
          well = stringr::str_replace(position, "^[^:]+:", "")
        ) |>
        build_plate_map_data(container_type = container_type)

      sample_map <- dplyr::bind_rows(sample_map, support_map)

      if (nrow(sample_map) == 0) {
        return(shiny::helpText("No plate/well coordinates are available yet."))
      }

      palette <- c(
        sample = "#d7efe4",
        qc = "#f8d6ae",
        blank = "#f4c6c6",
        calibration = "#d7d8ff",
        sst = "#f2ebc9",
        other = "#e6e6e6"
      )

      plate_tables <- lapply(sort(unique(sample_map$plate)), function(plate_name) {
        plate_data <- sample_map |>
          dplyr::filter(plate == plate_name)

        grid <- tidyr::expand_grid(
          plate = plate_name,
          row = spec$rows,
          column = spec$columns
        ) |>
          dplyr::left_join(plate_data, by = c("plate", "row", "column"))

        header <- shiny::tags$tr(
          shiny::tags$th(""),
          lapply(spec$columns, function(x) shiny::tags$th(x))
        )

        body <- lapply(spec$rows, function(r) {
          row_data <- grid |>
            dplyr::filter(row == r) |>
            dplyr::arrange(column)

          shiny::tags$tr(
            shiny::tags$th(r),
            lapply(seq_len(nrow(row_data)), function(i) {
              sample_type <- row_data$sample_type[[i]] %||% "other"
              fill <- if (!is.null(sample_type) && sample_type %in% names(palette)) palette[[sample_type]] else palette[["other"]]
              label <- row_data$sample_id[[i]] %||% ""
              if (is.na(label) || identical(label, "NA")) {
                label <- ""
              }
              shiny::tags$td(style = paste("background:", fill, "; min-width: 90px; color: #f8f9fa;"), label)
            })
          )
        })

        shiny::tagList(
          shiny::tags$h4(paste("Plate", plate_name, "-", spec$label)),
          shiny::tags$table(class = "table table-bordered table-sm", header, body)
        )
      })

      shiny::tagList(plate_tables)
    })

    output$summary_counts <- shiny::renderTable({
      sequence() |>
        dplyr::count(sample_category, name = "n")
    })
  })
}
