#' Split field values of cardinality=n into vectors
#'
#' @description
#' Fields whose cardinality is `"n"` can hold multiple values, separated by
#' semicolons (`;`).  This function splits those into vectors, so the column
#' becomes a list-column where each cell is a vector.
#'
#' @param field Character vector where each element contains zero or more values
#'   separated by semicolons, e.g. `NA`, `""`, `"foo"`, `"foo;bar"`,
#'   `"foo;bar;baz"`, etc.
#'
#' @export
#' @examples
#' x <- c(NA, "", "foo", "foo;bar", "foo;bar;baz")
#' cardinalise(x)
#' tibble::tibble(x = cardinalise(x))
cardinalise <- function(field) {
  stringr::str_split(field, pattern = ";")
}

