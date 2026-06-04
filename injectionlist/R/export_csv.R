write_injection_csv <- function(sequence, batch_info, file) {
  export_table <- build_export_table(sequence, batch_info)

  con <- file(file, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  writeLines("Bracket Type=4", con = con)
  utils::write.table(
    export_table,
    file = con,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE,
    qmethod = "double"
  )

  invisible(file)
}
