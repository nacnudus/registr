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
