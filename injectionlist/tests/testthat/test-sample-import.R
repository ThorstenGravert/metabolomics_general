testthat::test_that("sample submission manifest can be parsed", {
  manifest_path <- "Sample Submission Example.xlsx"

  testthat::skip_if_not(file.exists(manifest_path))

  imported <- parse_sample_manifest(manifest_path, sheet = "SampleSubmissionForm")

  testthat::expect_true(nrow(imported) >= 40)
  testthat::expect_true(all(c("sample_id", "sample_type", "plate", "well", "batch", "comment") %in% names(imported)))
  testthat::expect_identical(imported$sample_id[[1]], "SA000001")
  testthat::expect_true(all(grepl("^(SA|PB)", imported$sample_id)))
})
