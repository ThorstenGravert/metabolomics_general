testthat::test_that("single sample sequence builds", {
  samples <- tibble::tibble(
    sample_id = "SA000001",
    sample_type = "sample",
    plate = "R",
    well = "A1",
    batch = "B1",
    comment = ""
  )

  batch_info <- list(
    project_number = "CMD00001",
    batch_number = 1L,
    version_number = 1L,
    injection_offset = 0L,
    column_id = "C001",
    method_family = "RP",
    instrument = "XXX",
    start_date_code = "260604",
    sample_inj_vol = 5,
    standard_status = "Standard",
    acquisition_mode = "PAN",
    instrument_method = ""
  )

  sequence <- build_injection_sequence(
    samples = samples,
    batch_info = batch_info,
    qc_settings = list(enabled = FALSE, frequency = 6L, inj_volume = 5),
    blank_settings = list(enabled = FALSE, frequency = 12L, inj_volume = 5),
    calibration_settings = list(start_count = 0L, inj_volume = 5),
    start_settings = list(wash_count = 0L, blank_count = 0L, qc_count = 0L, sst_count = 0L),
    end_settings = list(wash_count = 0L, blank_count = 0L, qc_count = 0L, sst_count = 0L),
    support_layout_settings = list(plate = "G"),
    randomize_samples = FALSE,
    random_seed = 1
  )

  testthat::expect_equal(nrow(sequence), 1)
  testthat::expect_equal(sequence$final_sample_name[[1]], "SA000001")
  testthat::expect_equal(sequence$position[[1]], "R:A1")
})

testthat::test_that("fixed seed randomization is reproducible", {
  samples <- tibble::tibble(
    sample_id = sprintf("SA%06d", 1:10),
    sample_type = "sample",
    plate = "R",
    well = paste0("A", 1:10),
    batch = "B1",
    comment = ""
  )

  batch_info <- list(
    project_number = "CMD00001",
    batch_number = 1L,
    version_number = 1L,
    injection_offset = 0L,
    column_id = "C001",
    method_family = "RP",
    instrument = "XXX",
    start_date_code = "260604",
    sample_inj_vol = 5,
    standard_status = "Standard",
    acquisition_mode = "PAN",
    instrument_method = ""
  )

  seq_a <- build_injection_sequence(
    samples, batch_info,
    list(TRUE, 100L, 5) |> stats::setNames(c("enabled", "frequency", "inj_volume")),
    list(FALSE, 100L, 5) |> stats::setNames(c("enabled", "frequency", "inj_volume")),
    list(0L, 5) |> stats::setNames(c("start_count", "inj_volume")),
    list(0L, 0L, 0L, 0L) |> stats::setNames(c("wash_count", "blank_count", "qc_count", "sst_count")),
    list(0L, 0L, 0L, 0L) |> stats::setNames(c("wash_count", "blank_count", "qc_count", "sst_count")),
    list(plate = "G"),
    TRUE,
    42
  )

  seq_b <- build_injection_sequence(
    samples, batch_info,
    list(TRUE, 100L, 5) |> stats::setNames(c("enabled", "frequency", "inj_volume")),
    list(FALSE, 100L, 5) |> stats::setNames(c("enabled", "frequency", "inj_volume")),
    list(0L, 5) |> stats::setNames(c("start_count", "inj_volume")),
    list(0L, 0L, 0L, 0L) |> stats::setNames(c("wash_count", "blank_count", "qc_count", "sst_count")),
    list(0L, 0L, 0L, 0L) |> stats::setNames(c("wash_count", "blank_count", "qc_count", "sst_count")),
    list(plate = "G"),
    TRUE,
    42
  )

  testthat::expect_identical(seq_a$source_sample_id, seq_b$source_sample_id)
})

testthat::test_that("non-randomized mode preserves input order", {
  samples <- tibble::tibble(
    sample_id = c("SA000003", "SA000001", "SA000002"),
    sample_type = "sample",
    plate = "R",
    well = c("A3", "A1", "A2"),
    batch = "B1",
    comment = ""
  )

  batch_info <- list(
    project_number = "CMD00001",
    batch_number = 1L,
    version_number = 1L,
    injection_offset = 0L,
    column_id = "C001",
    method_family = "RP",
    instrument = "XXX",
    start_date_code = "260604",
    sample_inj_vol = 5,
    standard_status = "Standard",
    acquisition_mode = "PAN",
    instrument_method = ""
  )

  sequence <- build_injection_sequence(
    samples, batch_info,
    list(FALSE, 6L, 5) |> stats::setNames(c("enabled", "frequency", "inj_volume")),
    list(FALSE, 12L, 5) |> stats::setNames(c("enabled", "frequency", "inj_volume")),
    list(0L, 5) |> stats::setNames(c("start_count", "inj_volume")),
    list(0L, 0L, 0L, 0L) |> stats::setNames(c("wash_count", "blank_count", "qc_count", "sst_count")),
    list(0L, 0L, 0L, 0L) |> stats::setNames(c("wash_count", "blank_count", "qc_count", "sst_count")),
    list(plate = "G"),
    FALSE,
    1
  )

  testthat::expect_equal(sequence$source_sample_id, c("SA000003", "SA000001", "SA000002"))
})

testthat::test_that("qc and blank insertion frequencies work", {
  samples <- tibble::tibble(
    sample_id = sprintf("SA%06d", 1:6),
    sample_type = "sample",
    plate = "R",
    well = c("A1", "A2", "A3", "A4", "A5", "A6"),
    batch = "B1",
    comment = ""
  )

  batch_info <- list(
    project_number = "CMD00001",
    batch_number = 1L,
    version_number = 1L,
    injection_offset = 0L,
    column_id = "C001",
    method_family = "RP",
    instrument = "XXX",
    start_date_code = "260604",
    sample_inj_vol = 5,
    standard_status = "Standard",
    acquisition_mode = "PAN",
    instrument_method = ""
  )

  sequence <- build_injection_sequence(
    samples, batch_info,
    list(TRUE, 2L, 5) |> stats::setNames(c("enabled", "frequency", "inj_volume")),
    list(TRUE, 3L, 5) |> stats::setNames(c("enabled", "frequency", "inj_volume")),
    list(0L, 5) |> stats::setNames(c("start_count", "inj_volume")),
    list(0L, 0L, 0L, 0L) |> stats::setNames(c("wash_count", "blank_count", "qc_count", "sst_count")),
    list(0L, 0L, 0L, 0L) |> stats::setNames(c("wash_count", "blank_count", "qc_count", "sst_count")),
    list(plate = "G"),
    FALSE,
    1
  )

  testthat::expect_equal(sum(sequence$sample_name == "QC"), 2)
  testthat::expect_equal(sum(sequence$sample_name == "Blank"), 1)
})

testthat::test_that("support types can start in separate columns", {
  samples <- tibble::tibble(
    sample_id = "SA000001",
    sample_type = "sample",
    plate = "Y",
    well = "A1",
    batch = "1",
    comment = ""
  )

  batch_info <- list(
    project_number = "CMD00001",
    batch_number = 1L,
    version_number = 1L,
    injection_offset = 0L,
    column_id = "C001",
    method_family = "RP",
    instrument = "XXX",
    start_date_code = "260604",
    sample_inj_vol = 5,
    standard_status = "Standard",
    acquisition_mode = "PAN",
    instrument_method = ""
  )

  sequence <- build_injection_sequence(
    samples, batch_info,
    list(enabled = FALSE, frequency = 6L, inj_volume = 5, start_column = 1L),
    list(enabled = FALSE, frequency = 12L, inj_volume = 5, start_column = 3L),
    list(start_count = 2L, inj_volume = 5, start_column = 2L),
    list(wash_count = 0L, blank_count = 0L, qc_count = 2L, sst_count = 0L, wash_start_column = 5L, sst_start_column = 4L),
    list(wash_count = 0L, blank_count = 0L, qc_count = 0L, sst_count = 0L),
    list(plate = "G"),
    FALSE,
    1
  )

  testthat::expect_equal(sequence$position[[1]], "G:A1")
  testthat::expect_equal(sequence$position[[2]], "G:B1")
  testthat::expect_equal(sequence$position[[3]], "G:B2")
})
