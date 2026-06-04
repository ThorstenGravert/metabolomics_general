testthat::test_that("duplicate sample IDs are blocked", {
  samples <- tibble::tibble(
    sample_id = c("SA000001", "SA000001"),
    sample_type = c("sample", "sample"),
    plate = c("R", "R"),
    well = c("A1", "A2"),
    batch = c("B1", "B1"),
    comment = c("", "")
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
    autosampler = "Plate",
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

  messages <- validate_injection_workflow(
    samples = standardize_sample_table(samples),
    sequence = sequence,
    batch_info = batch_info,
    settings = list(
      qc_settings = list(enabled = FALSE, frequency = 6L),
      blank_settings = list(enabled = FALSE, frequency = 12L)
    )
  )

  testthat::expect_true(any(messages$severity == "Error"))
  testthat::expect_true(any(messages$field == "sample_id"))
})

testthat::test_that("invalid wells are blocked", {
  samples <- tibble::tibble(
    sample_id = "SA000001",
    sample_type = "sample",
    plate = "R",
    well = "Z99",
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
    autosampler = "Plate",
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

  messages <- validate_injection_workflow(
    samples = standardize_sample_table(samples),
    sequence = sequence,
    batch_info = batch_info,
    settings = list(
      qc_settings = list(enabled = FALSE, frequency = 6L),
      blank_settings = list(enabled = FALSE, frequency = 12L)
    )
  )

  testthat::expect_true(any(messages$field == "well"))
})

testthat::test_that("vial rack wells validate correctly", {
  testthat::expect_true(is_valid_well("A1", "Vial"))
  testthat::expect_true(is_valid_well("F9", "Vial"))
  testthat::expect_false(is_valid_well("G1", "Vial"))
  testthat::expect_false(is_valid_well("A10", "Vial"))
})
