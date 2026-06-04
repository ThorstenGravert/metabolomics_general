validate_injection_workflow <- function(samples, sequence, batch_info, settings) {
  vocab <- lc_controlled_vocabularies()

  messages <- tibble::tribble(
    ~severity, ~field, ~message
  )

  add_message <- function(severity, field, message) {
    tibble::tibble(severity = severity, field = field, message = message)
  }

  if (!nzchar(batch_info$project_number %||% "")) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "project_number", "Project number is required."))
  }

  required_batch_fields <- c("batch_number", "version_number", "column_id", "method_family", "instrument", "start_date_code", "acquisition_mode")
  for (field in required_batch_fields) {
    value <- batch_info[[field]]
    if (is.null(value) || (is.character(value) && !nzchar(value))) {
      messages <- dplyr::bind_rows(messages, add_message("Error", field, paste(field, "is required.")))
    }
  }

  if (!(batch_info$method_family %||% "") %in% vocab$method) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "method_family", "Method must match the controlled vocabulary."))
  }

  if (!(batch_info$instrument %||% "") %in% vocab$instrument) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "instrument", "Instrument must match the controlled vocabulary."))
  }

  if (!(batch_info$standard_status %||% "") %in% vocab$standard_status) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "standard_status", "Standard/non-standard must match the controlled vocabulary."))
  }

  if (!(batch_info$autosampler %||% "") %in% vocab$autosampler) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "autosampler", "Autosampler type must match the controlled vocabulary."))
  }

  if (!(batch_info$acquisition_mode %||% "") %in% vocab$acquisition_mode) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "acquisition_mode", "Acquisition mode must be PAN, POS, or NEG."))
  }

  if (nrow(samples) == 0) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "samples", "No samples were provided after parsing."))
  }

  if (any(is.na(samples$sample_id) | !nzchar(samples$sample_id))) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "sample_id", "Sample IDs are required for every row."))
  }

  duplicate_ids <- samples |>
    dplyr::filter(!is.na(sample_id), nzchar(sample_id)) |>
    dplyr::count(sample_id) |>
    dplyr::filter(n > 1)

  if (nrow(duplicate_ids) > 0) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "sample_id", "Duplicate sample IDs were found."))
  }

  invalid_types <- samples |>
    dplyr::filter(!sample_type %in% vocab$sample_type)

  if (nrow(invalid_types) > 0) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "sample_type", "Invalid sample types were found in the imported table."))
  }

  invalid_wells <- samples |>
    dplyr::filter(!is.na(well), nzchar(well)) |>
    dplyr::filter(!vapply(well, is_valid_well, logical(1), container_type = batch_info$autosampler %||% "Plate"))

  if (nrow(invalid_wells) > 0) {
    spec <- get_container_spec(batch_info$autosampler %||% "Plate")
    example_end <- paste0(tail(spec$rows, 1), tail(spec$columns, 1))
    messages <- dplyr::bind_rows(messages, add_message("Error", "well", paste("Invalid well positions were found. Expected values like", paste0(spec$rows[[1]], spec$columns[[1]]), "to", example_end, "for the selected format.")))
  }

  if (isTRUE(settings$qc_settings$enabled) && settings$qc_settings$frequency < 1) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "qc_frequency", "QC frequency must be at least 1 when QC insertion is enabled."))
  }

  if (isTRUE(settings$blank_settings$enabled) && settings$blank_settings$frequency < 1) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "blank_frequency", "Blank frequency must be at least 1 when blank insertion is enabled."))
  }

  if (nrow(sequence) == 0) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "sequence", "The generated injection sequence is empty."))
  }

  if (nrow(sequence) > 20000) {
    messages <- dplyr::bind_rows(messages, add_message("Warning", "sequence", "The generated sequence is very large and may exceed practical instrument limits."))
  }

  invalid_names <- sequence |>
    dplyr::filter(stringr::str_detect(final_sample_name, "[\\\\/:*?\"<>|]"))

  if (nrow(invalid_names) > 0) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "final_sample_name", "Forbidden characters were found in generated sample names."))
  }

  empty_rows <- sequence |>
    dplyr::filter(is.na(position) | !nzchar(position) | is.na(final_sample_name) | !nzchar(final_sample_name))

  if (nrow(empty_rows) > 0) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "sequence", "Generated sequence contains empty critical fields."))
  }

  invalid_positions <- sequence |>
    dplyr::filter(!vapply(position, is_valid_position, logical(1), container_type = batch_info$autosampler %||% "Plate"))

  if (nrow(invalid_positions) > 0) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "position", "One or more sequence positions are invalid for the selected container format."))
  }

  duplicate_final_names <- sequence |>
    dplyr::filter(!is.na(final_sample_name), nzchar(final_sample_name)) |>
    dplyr::count(final_sample_name) |>
    dplyr::filter(n > 1)

  if (nrow(duplicate_final_names) > 0) {
    messages <- dplyr::bind_rows(messages, add_message("Warning", "final_sample_name", "Duplicate final sample names were found in the sequence preview."))
  }

  export_table <- build_export_table(sequence, batch_info)
  if (!identical(names(export_table), export_column_names)) {
    messages <- dplyr::bind_rows(messages, add_message("Error", "export", "Export column names or order do not match the LC template."))
  }

  if (identical(batch_info$standard_status, "Standard") && !nzchar(batch_info$instrument_method %||% "")) {
    messages <- dplyr::bind_rows(messages, add_message("Warning", "instrument_method", "Standard mode is selected, but no instrument method path was provided for export."))
  }

  if (identical(batch_info$autosampler, "Vial")) {
    messages <- dplyr::bind_rows(messages, add_message("Info", "autosampler", "Vial mode uses a 6x9 rack layout (rows A-F, columns 1-9)."))
  }

  messages |>
    dplyr::distinct()
}
