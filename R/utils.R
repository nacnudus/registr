globalVariables(c("data",
                  "download.file",
                  "unzip",
                  "item_hash",
                  "item-hash",
                  "hash",
                  "hash-list",
                  "as_tibble",
                  "json",
                  "entry-number",
                  "desc",
                  "map_chr",
                  "items",
                  "key",
                  "n",
                  "timestamp",
                  "type",
                  "unnest",
                  "."))

#' Return column names missing from a data frame
#'
#' @param .data Data frame.
#' @param ... names of columns.
#' @return A character vector of column names that aren't in `.data`.
missing_col_names <- function(.data, ...) {
  UseMethod("missing_col_names")
}
missing_col_names.data.frame <- function(.data, ...) {
  col_names <- purrr::map_chr(rlang::enquos(...), rlang::quo_text)
  col_names[!(col_names %in% colnames(.data))]
}

#' Construct a zero-row tibble with given column names
#'
#' @details All columns will be `logical` so that they don't disturb exiting
#'   columns when appended with [dplyr::bind_rows()]`
#'
#' @param col_names Character vector of column names
blank_tibble <- function(col_names) {
  columns <- purrr::map(col_names, ~ rlang::expr(logical()))
  names(columns) <- col_names
  tibble::tibble(!!! columns)
}

#' Download or open a register file
#'
#' @details Either download a register, constructing the URL from the name of
#' the register and the given phase, or load it from a file containing the RSF
#' (register serialisation format).
#'
#' @param file Character, either the name of the register, or a file path.
#' @param phase Character, one of `"beta"` or `"alpha"`
#' @param path_type Character, one of `"url"` or `"file"` to decide what to do
#'   with `register`.
register_lines <- function(file, phase = c("beta", "alpha"),
                           path_type = c("url", "file")) {
  path_type <- match.arg(path_type)
  if (path_type == "url") {
    phase <- match.arg(phase)
    register_url <-
      switch(phase,
             beta = "https://{file}.register.gov.uk/download-rsf",
             alpha = "https://{file}.{phase}.openregister.org/download-rsf")
    register_url <- glue::glue(register_url)
    message("Downloading register '", file,
            "' from the '", phase, "' phase ...\n")
    register_path <- tempfile()
    on.exit(unlink(register_path))
    download.file(register_url, register_path)
    return(readr::read_lines(register_path))
  } else if (path_type == "file") {
    return(readr::read_lines(file))
  }
}
