export_column_names <- c(
  "Sample Type", "File Name", "Sample ID", "Path", "Instrument Method",
  "Process Method", "Calibration File", "Position", "Inj Vol", "Level",
  "Sample Wt", "Sample Vol", "ISTD Amt", "Dil Factor", "L1 Study",
  "L2 Client", "L3 Laboratory", "L4 Company", "L5 Phone", "Comment"
)

lc_controlled_vocabularies <- function() {
  list(
    method = c("RP", "HILIC", "BA", "Other"),
    instrument = c("XXX", "LCDK1", "LCDK2", "LCDK3", "OEGT1", "OEGT2"),
    standard_status = c("Standard", "Nonstandard"),
    autosampler = c("Plate", "Vial"),
    acquisition_mode = c("PAN", "POS", "NEG"),
    plate_color = c("R", "Y", "B", "G"),
    sample_type = c("sample", "qc", "blank", "calibration", "sst", "system_suitability", "other")
  )
}

get_container_spec <- function(container_type = "Plate") {
  if (identical(container_type, "Vial")) {
    return(list(rows = LETTERS[1:6], columns = 1:9, label = "6x9 vial rack"))
  }

  list(rows = LETTERS[1:8], columns = 1:12, label = "96-well plate")
}

default_support_positions <- function(plate = "G", n = 500) {
  wells <- tidyr::expand_grid(row = LETTERS[1:8], column = 1:12) |>
    dplyr::mutate(position = paste0(plate, ":", row, column))

  rep_len(wells$position, n)
}

make_plate_well <- function(column_index, row_index) {
  paste0(LETTERS[[row_index]], column_index)
}

build_support_layout <- function(
  plate = "G",
  container_type = "Plate",
  qc_start_column = 1L,
  calibration_start_column = 2L,
  blank_start_column = 3L,
  sst_start_column = 4L,
  wash_start_column = 5L,
  other_start_column = 6L
) {
  spec <- get_container_spec(container_type)

  list(
    plate = plate,
    rows = spec$rows,
    columns = spec$columns,
    next_index = c(
      qc = 0L,
      calibration = 0L,
      blank = 0L,
      sst = 0L,
      wash = 0L,
      other = 0L
    ),
    start_column = c(
      qc = as.integer(qc_start_column),
      calibration = as.integer(calibration_start_column),
      blank = as.integer(blank_start_column),
      sst = as.integer(sst_start_column),
      wash = as.integer(wash_start_column),
      other = as.integer(other_start_column)
    )
  )
}

next_support_position <- function(layout, type) {
  type <- if (type %in% names(layout$next_index)) type else "other"
  layout$next_index[[type]] <- layout$next_index[[type]] + 1L
  index <- layout$next_index[[type]] - 1L
  row_count <- length(layout$rows)
  column_index <- layout$start_column[[type]] + (index %/% row_count)
  row_index <- (index %% row_count) + 1L

  list(
    layout = layout,
    position = paste0(layout$plate, ":", make_plate_well(column_index, row_index))
  )
}

build_plate_position <- function(plate, well) {
  plate <- stringr::str_trim(as.character(plate %||% ""))
  well <- stringr::str_to_upper(stringr::str_trim(as.character(well %||% "")))

  dplyr::case_when(
    nzchar(plate) & nzchar(well) ~ paste0(plate, ":", well),
    nzchar(well) ~ well,
    TRUE ~ NA_character_
  )
}

normalize_yes_no <- function(x) {
  x <- stringr::str_to_lower(stringr::str_trim(as.character(x %||% "")))
  dplyr::case_when(
    x %in% c("true", "t", "yes", "y", "1") ~ TRUE,
    x %in% c("false", "f", "no", "n", "0") ~ FALSE,
    TRUE ~ NA
  )
}

trim_sample_export_id <- function(x) {
  dplyr::if_else(stringr::str_detect(x, "^SA"), stringr::str_sub(x, 1, 8), x)
}

make_project_path <- function(batch_info) {
  paste0(
    "D:\\Projects\\", batch_info$project_number,
    "\\instrument_files\\B", batch_info$batch_number,
    "V", batch_info$version_number
  )
}

make_timestamped_filename <- function(project_number, timestamp = Sys.time()) {
  stamp <- format(timestamp, "%Y-%m-%d_%H%M")
  paste0(project_number, "_LC_injection_sequence_", stamp, ".csv")
}

parse_pasted_samples <- function(text) {
  if (is.null(text) || !nzchar(stringr::str_trim(text))) {
    return(tibble::tibble())
  }

  delimiter <- if (grepl("\t", text, fixed = TRUE)) "\t" else ","

  readr::read_delim(
    file = I(text),
    delim = delimiter,
    show_col_types = FALSE,
    trim_ws = TRUE,
    progress = FALSE
  )
}

parse_sample_manifest <- function(path, sheet = NULL) {
  if (is.null(sheet)) {
    sheet <- readxl::excel_sheets(path)[[1]]
  }

  manifest <- readxl::read_excel(
    path = path,
    sheet = sheet,
    skip = 10
  ) |>
    janitor::clean_names()

  expected <- c(
    "internal_sample_id", "sample_number", "sample_present_n",
    "discrepancy_comment", "sample_name", "plate_or_box_id",
    "position_in_plate_or_box", "sample_type", "host", "buffer_media",
    "sample_amount", "storage_temperature", "service_type", "comments"
  )

  missing <- setdiff(expected, names(manifest))
  for (column in missing) {
    manifest[[column]] <- NA_character_
  }

  manifest |>
    dplyr::mutate(
      sample_present_n = stringr::str_to_upper(stringr::str_trim(as.character(sample_present_n))),
      internal_sample_id = stringr::str_trim(as.character(internal_sample_id)),
      sample_name = stringr::str_trim(as.character(sample_name)),
      plate_or_box_id = stringr::str_trim(as.character(plate_or_box_id)),
      position_in_plate_or_box = stringr::str_to_upper(stringr::str_trim(as.character(position_in_plate_or_box))),
      comments = stringr::str_trim(as.character(comments))
    ) |>
    dplyr::filter(
      !is.na(internal_sample_id) | !is.na(sample_name),
      sample_present_n %in% c("Y", "YES", ""),
      stringr::str_detect(internal_sample_id, "^(SA|PB)")
    ) |>
    dplyr::transmute(
      sample_id = internal_sample_id,
      sample_type = dplyr::case_when(
        stringr::str_detect(internal_sample_id, "^PB") ~ "blank",
        TRUE ~ "sample"
      ),
      plate = plate_or_box_id,
      well = position_in_plate_or_box,
      batch = "",
      comment = dplyr::coalesce(comments, "")
    ) |>
    dplyr::filter(!is.na(sample_id), nzchar(sample_id))
}

read_sample_input_file <- function(path) {
  extension <- tolower(tools::file_ext(path))

  if (extension %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(path)
    if ("SampleSubmissionForm" %in% sheets) {
      return(parse_sample_manifest(path, sheet = "SampleSubmissionForm"))
    }

    return(readxl::read_excel(path))
  }

  readr::read_csv(path, show_col_types = FALSE)
}

standardize_sample_table <- function(samples) {
  if (nrow(samples) == 0) {
    return(
      tibble::tibble(
        sample_id = character(),
        sample_type = character(),
        plate = character(),
        well = character(),
        batch = character(),
        comment = character(),
        original_order = integer()
      )
    )
  }

  name_map <- c(
    "sample id" = "sample_id",
    "sample_id" = "sample_id",
    "sampleid" = "sample_id",
    "name" = "sample_id",
    "sample type" = "sample_type",
    "sample_type" = "sample_type",
    "type" = "sample_type",
    "plate" = "plate",
    "tray" = "plate",
    "well" = "well",
    "position" = "well",
    "batch" = "batch",
    "comment" = "comment",
    "comments" = "comment",
    "note" = "comment"
  )

  samples <- samples |>
    janitor::clean_names() |>
    dplyr::rename_with(
      ~ dplyr::coalesce(unname(name_map[.x]), .x)
    )

  missing_columns <- setdiff(c("sample_id", "sample_type", "plate", "well", "batch", "comment"), names(samples))
  for (column in missing_columns) {
    samples[[column]] <- NA_character_
  }

  samples |>
    dplyr::transmute(
      sample_id = stringr::str_trim(as.character(sample_id)),
      sample_type = dplyr::if_else(
        is.na(sample_type) | !nzchar(stringr::str_trim(as.character(sample_type))),
        "sample",
        stringr::str_to_lower(stringr::str_trim(as.character(sample_type)))
      ),
      plate = stringr::str_trim(as.character(plate)),
      well = stringr::str_to_upper(stringr::str_trim(as.character(well))),
      batch = stringr::str_trim(as.character(batch)),
      comment = stringr::str_trim(as.character(comment)),
      original_order = dplyr::row_number()
    )
}

build_plate_map_data <- function(samples, container_type = "Plate") {
  if (!all(c("plate", "well", "sample_type", "sample_id") %in% names(samples)) || nrow(samples) == 0) {
    return(tibble::tibble())
  }

  spec <- get_container_spec(container_type)

  samples |>
    dplyr::filter(!is.na(well), nzchar(well)) |>
    tidyr::separate(well, into = c("row", "column"), sep = "(?<=[A-Z])", remove = FALSE) |>
    dplyr::mutate(
      plate = dplyr::if_else(is.na(plate) | !nzchar(plate), "?", plate),
      column = suppressWarnings(as.integer(column)),
      sample_type = dplyr::if_else(is.na(sample_type) | !nzchar(sample_type), "other", sample_type)
    ) |>
    dplyr::filter(!is.na(column), row %in% spec$rows, column %in% spec$columns)
}

is_valid_well <- function(well, container_type = "Plate") {
  if (is.na(well) || !nzchar(well)) {
    return(FALSE)
  }

  spec <- get_container_spec(container_type)
  parsed <- tibble::tibble(well = well) |>
    tidyr::separate(well, into = c("row", "column"), sep = "(?<=[A-Z])", remove = FALSE) |>
    dplyr::mutate(column = suppressWarnings(as.integer(column)))

  parsed$row[[1]] %in% spec$rows && parsed$column[[1]] %in% spec$columns
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
