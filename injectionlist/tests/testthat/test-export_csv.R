testthat::test_that("export column order matches LC template", {
  sequence <- tibble::tibble(
    file_name = "260604-XXX-PAN-1-001-SA000001",
    source_sample_id = "SA000001",
    final_sample_name = "SA000001",
    export_path = "D:\\Projects\\CMD00001\\instrument_files\\B1V1",
    instrument_method = "",
    position = "R:A1",
    inj_vol = 5,
    comment = ""
  )

  batch_info <- list(
    project_number = "CMD00001",
    client_name = "",
    laboratory_name = "Metabolomics",
    company_name = "",
    phone = ""
  )

  export <- build_export_table(sequence, batch_info)
  testthat::expect_identical(names(export), export_column_names)
})

testthat::test_that("timestamped filename includes project number", {
  timestamp <- as.POSIXct("2026-06-04 14:30:00", tz = "Europe/Copenhagen")
  name <- make_timestamped_filename("CMD01234", timestamp)
  testthat::expect_identical(name, "CMD01234_LC_injection_sequence_2026-06-04_1430.csv")
})
