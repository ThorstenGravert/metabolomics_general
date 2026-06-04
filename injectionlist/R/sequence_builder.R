build_injection_sequence <- function(
  samples,
  batch_info,
  qc_settings,
  blank_settings,
  calibration_settings,
  start_settings,
  end_settings,
  support_layout_settings,
  randomize_samples = TRUE,
  random_seed = 1
) {
  samples <- standardize_sample_table(samples)

  real_samples <- samples |>
    dplyr::filter(sample_type == "sample")

  if (isTRUE(randomize_samples) && nrow(real_samples) > 0) {
    set.seed(random_seed)
    real_samples <- real_samples |>
      dplyr::slice_sample(prop = 1)
  } else {
    real_samples <- real_samples |>
      dplyr::arrange(original_order)
  }

  support_layout <- build_support_layout(
    plate = support_layout_settings$plate %||% "G",
    container_type = batch_info$autosampler %||% "Plate",
    qc_start_column = qc_settings$start_column %||% 1L,
    calibration_start_column = calibration_settings$start_column %||% 2L,
    blank_start_column = blank_settings$start_column %||% 3L,
    sst_start_column = start_settings$sst_start_column %||% 4L,
    wash_start_column = start_settings$wash_start_column %||% 5L,
    other_start_column = start_settings$other_start_column %||% 6L
  )

  make_support_rows <- function(label, count, volume, type, comment = NA_character_) {
    if (count <= 0) {
      return(tibble::tibble())
    }

    positions <- purrr::map_chr(seq_len(count), function(.x) {
      allocated <- next_support_position(support_layout, type)
      support_layout <<- allocated$layout
      allocated$position
    })

    tibble::tibble(
      sample_name = rep(label, count),
      sample_category = rep(type, count),
      position = positions,
      inj_vol = rep(volume, count),
      source_sample_id = rep(NA_character_, count),
      source_sample_type = rep(type, count),
      comment = rep(comment, count)
    )
  }

  make_calibration_rows <- function(count, volume) {
    if (count <= 0) {
      return(tibble::tibble())
    }

    positions <- purrr::map_chr(seq_len(count), function(.x) {
      allocated <- next_support_position(support_layout, "calibration")
      support_layout <<- allocated$layout
      allocated$position
    })

    tibble::tibble(
      sample_name = sprintf("CAL%02d_1", seq_len(count)),
      sample_category = "calibration",
      position = positions,
      inj_vol = volume,
      source_sample_id = NA_character_,
      source_sample_type = "calibration",
      comment = "Calibration standard"
    )
  }

  pre_block <- dplyr::bind_rows(
    make_support_rows("Wash", start_settings$wash_count %||% 0L, start_settings$wash_volume %||% batch_info$sample_inj_vol, "wash"),
    make_support_rows("Blank", start_settings$blank_count %||% 0L, start_settings$blank_volume %||% batch_info$sample_inj_vol, "blank"),
    make_support_rows("SST1", start_settings$sst_count %||% 0L, start_settings$sst_volume %||% batch_info$sample_inj_vol, "sst"),
    make_support_rows("QC", start_settings$qc_count %||% 0L, start_settings$qc_volume %||% batch_info$sample_inj_vol, "qc"),
    make_calibration_rows(calibration_settings$start_count %||% 0L, calibration_settings$inj_volume %||% batch_info$sample_inj_vol)
  )

  sample_rows <- purrr::imap_dfr(seq_len(nrow(real_samples)), function(idx, .ignored) {
    row <- real_samples[idx, , drop = FALSE]

    out <- tibble::tibble(
      sample_name = row$sample_id,
      sample_category = "sample",
      position = build_plate_position(row$plate, row$well),
      inj_vol = batch_info$sample_inj_vol,
      source_sample_id = row$sample_id,
      source_sample_type = row$sample_type,
      comment = row$comment
    )

    if (isTRUE(qc_settings$enabled) && qc_settings$frequency > 0 && idx %% qc_settings$frequency == 0 && idx < nrow(real_samples)) {
      out <- dplyr::bind_rows(
        out,
        make_support_rows("QC", 1L, qc_settings$inj_volume %||% batch_info$sample_inj_vol, "qc")
      )
    }

    if (isTRUE(blank_settings$enabled) && blank_settings$frequency > 0 && idx %% blank_settings$frequency == 0 && idx < nrow(real_samples)) {
      out <- dplyr::bind_rows(
        out,
        make_support_rows("Blank", 1L, blank_settings$inj_volume %||% batch_info$sample_inj_vol, "blank")
      )
    }

    out
  })

  post_block <- dplyr::bind_rows(
    make_support_rows("QC", end_settings$qc_count %||% 0L, end_settings$qc_volume %||% batch_info$sample_inj_vol, "qc"),
    make_support_rows("SST2", end_settings$sst_count %||% 0L, end_settings$sst_volume %||% batch_info$sample_inj_vol, "sst"),
    make_support_rows("Wash", end_settings$wash_count %||% 0L, end_settings$wash_volume %||% batch_info$sample_inj_vol, "wash"),
    make_support_rows("Blank", end_settings$blank_count %||% 0L, end_settings$blank_volume %||% batch_info$sample_inj_vol, "blank")
  )

  sequence <- dplyr::bind_rows(pre_block, sample_rows, post_block) |>
    dplyr::mutate(
      count = dplyr::row_number(),
      injection_number = count + (batch_info$injection_offset %||% 0L),
      injection_number_chr = stringr::str_pad(injection_number, width = 3, pad = "0"),
      qc_flag = stringr::str_detect(sample_name, "^QC"),
      blank_flag = sample_name == "Blank",
      qc_count = cumsum(qc_flag),
      blank_count = cumsum(blank_flag),
      final_sample_name = dplyr::case_when(
        sample_name %in% c("SST1", "SST2") ~ paste0(sample_name, "_", batch_info$column_id),
        sample_name == "QC" ~ sprintf("QC_%03d", qc_count),
        sample_name == "Blank" ~ sprintf("Blank_%03d", blank_count),
        TRUE ~ trim_sample_export_id(sample_name)
      ),
      method_lookup = dplyr::case_when(
        stringr::str_detect(sample_name, "MSMSincl") & batch_info$acquisition_mode == "POS" ~ paste(batch_info$method_family, batch_info$instrument, "POS-MSMSincl", sep = "-"),
        stringr::str_detect(sample_name, "MSMSincl") & batch_info$acquisition_mode == "NEG" ~ paste(batch_info$method_family, batch_info$instrument, "NEG-MSMSincl", sep = "-"),
        stringr::str_detect(sample_name, "MSMS") & batch_info$acquisition_mode == "POS" ~ paste(batch_info$method_family, batch_info$instrument, "POS-MSMS", sep = "-"),
        stringr::str_detect(sample_name, "MSMS") & batch_info$acquisition_mode == "NEG" ~ paste(batch_info$method_family, batch_info$instrument, "NEG-MSMS", sep = "-"),
        TRUE ~ paste(batch_info$method_family, batch_info$instrument, batch_info$acquisition_mode, sep = "-")
      ),
      instrument_method = dplyr::if_else(
        identical(batch_info$standard_status, "Nonstandard"),
        "",
        batch_info$instrument_method %||% ""
      ),
      file_name_prefix = paste(
        batch_info$start_date_code,
        batch_info$instrument,
        batch_info$acquisition_mode,
        batch_info$batch_number,
        injection_number_chr,
        sep = "-"
      ),
      file_name = paste0(file_name_prefix, "-", final_sample_name),
      export_path = batch_info$export_path %||% make_project_path(batch_info)
    ) |>
    dplyr::select(
      count, injection_number, injection_number_chr, sample_name, final_sample_name,
      sample_category, position, inj_vol, source_sample_id, source_sample_type,
      method_lookup, instrument_method, file_name, export_path, comment
    )

  sequence
}

build_export_table <- function(sequence, batch_info) {
  export <- sequence |>
    dplyr::transmute(
      `Sample Type` = "Unknown",
      `File Name` = file_name,
      `Sample ID` = dplyr::coalesce(source_sample_id, final_sample_name),
      Path = export_path,
      `Instrument Method` = instrument_method,
      `Process Method` = "",
      `Calibration File` = "",
      Position = position,
      `Inj Vol` = inj_vol,
      Level = "",
      `Sample Wt` = 0,
      `Sample Vol` = 0,
      `ISTD Amt` = 0,
      `Dil Factor` = 1,
      `L1 Study` = batch_info$project_number,
      `L2 Client` = batch_info$client_name %||% "",
      `L3 Laboratory` = batch_info$laboratory_name %||% "",
      `L4 Company` = batch_info$company_name %||% "",
      `L5 Phone` = batch_info$phone %||% "",
      Comment = dplyr::coalesce(comment, "")
    )

  export[, export_column_names]
}
