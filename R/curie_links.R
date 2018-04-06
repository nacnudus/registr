#' List registers linked to by CURIEs
#'
#' @description
#' Values in fields of datatype `"curie"` link to other registers.  These
#' functions list which registers are linked to by a field or all the fields in
#' a register.
#'
#' [curie_fields()] names which fields in a register are of datatype `"curie"`.
#'
#' [curie_links()] names which registers are linked to by a field of datatype
#' `"curie"`, or by a whole register that has fields of datatype `"curie"`.
#'
#' @param x Object of class `"register"`, or a character vector that is a field
#' of a register object (`register$data$foo`) of datatype `"curie"`.
#'
#' @return A character vector of names of registers that are linked to.
#' @name curie
#' @examples
#' register <- rr_register("statistical-geography")
#' curie_fields(register)
#' curie_links(register$data$area)
#' curie_links(register$data$organisation)
#' curie_links(register)
NULL

#' @rdname curie
#' @export
curie_fields <- function(x) {
  UseMethod("curie_fields")
}

#' @rdname curie
#' @export
curie_fields.register <- function(x) {
  x$schema$fields %>%
  dplyr::filter(datatype == "curie") %>%
  dplyr::pull(field)
}

#' @rdname curie
#' @export
curie_links <- function(x) {
  UseMethod("curie_links")
}

#' @rdname curie
#' @export
curie_links.default <- function(x) {
  purrr::map_chr(x, ~ parse_curie(.x)$prefix) %>%
    purrr::discard(is.na) %>%
    unique()
}

#' @rdname curie
#' @export
curie_links.register <- function(x) {
  x$data %>%
  dplyr::select(curie_fields(x)) %>%
  purrr::map(curie_links) %>%
  purrr::flatten_chr() %>%
  unique()
}
