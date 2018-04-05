#' Apply registers-style datatypes to fields
#'
#' @details Registers use their own datatypes. This function converts strings to
#' the equivalent R datatypes.
#'
#' @param x Character vector (a field of a register).
#' @param x Name of the datatype to apply: currently one of `"curie"`, `"url"`,
#'   `"datetime"`, `"string"` and `"text"`.
#' @export
#' @examples
#' apply_datatype("2014-04", "datetime")
apply_datatype <- function(x, datatype) {
  switch(datatype,
         curie = x,
         url = x,
         datetime = maybe_parse_iso_8601(x),
         string = x,
         text = x)
}

maybe_parse_iso_8601 <- function(x) {
  purrr::map(x,
             ~ if (is.na(.x)) {
               as.POSIXct(NA) }
             else {
               parsedate::parse_iso_8601(.x)
             }) %>%
  do.call(c, .)
}
