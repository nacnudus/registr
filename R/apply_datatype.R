#' Apply registers-style datatypes to fields
#'
#' @details Registers use their own datatypes. This function converts strings to
#' the equivalent R datatypes.
#'
#' @param x Character vector (a field of a register).
#' @param datatype Name of the datatype to apply: currently one of `"curie"`, `"url"`,
#'   `"datetime"`, `"string"`, `"integer"` and `"text"`.
#' @param apply_iso_8601 Logical, whether to parse ISO8601 strings as datetimes
#'   with [parsedate::parse_iso_8601()], otherwise leave as a string.  Partial
#'   datetimes are parsed as the earliest possible datetime, e.g. `"2018"`
#'   becomes `"2018-01-01 UTC"`.
#' @export
#' @examples
#' apply_datatype("2014-04", "datetime")
apply_datatype <- function(x, datatype, apply_iso_8601 = TRUE) {
  switch(datatype,
         curie = x,
         url = x,
         datetime = maybe_parse_iso_8601(x, apply_iso_8601),
         string = x,
         integer = as.integer(x),
         text = x)
}

#' Parse ISO8601 strings as datetimes or characters
#'
#' @param x Character vector
#' @param parse Logicial, whether to parse as a datetime or leave as (or convert
#' to) character [parsedate::parse_iso_8601()]
maybe_parse_iso_8601 <- function(x, parse = FALSE) {
  if (parse) {
    out <- purrr::map(x,
                      ~ if (actually) if (is.na(.x)) {
                        as.POSIXct(NA) }
                      else {
                        parsedate::parse_iso_8601(.x)
                      }) %>%
    out <- do.call(c, out) %>%
    return(out)
  }
  as.character(x)
}
