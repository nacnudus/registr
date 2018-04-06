#' List registers linked to by CURIEs
#'
#' @description
#' Values in fields of datatype `"curie"` link to other registers.  These
#' functions list which registers are linked to by a field or all the fields in
#' a register.
#'
#' [rr_curie_fields()] names which fields in a register are of datatype
#' `"curie"`.
#'
#' [rr_curie_links()] names which registers are linked to by a field of datatype
#' `"curie"`, or by a whole register that has fields of datatype `"curie"`.
#'
#' @param x Object of class `"register"`, or a character vector that is a field
#' of a register object (`register$data$foo`) of datatype `"curie"`.
#'
#' @return A character vector of names of registers that are linked to.
#' @name rr_curie
#' @examples
#' register <- rr_register("statistical-geography")
#' rr_curie_fields(register)
#' rr_curie_links(register$data$area)
#' rr_curie_links(register$data$organisation)
#' rr_curie_links(register)
NULL

#' @rdname rr_curie
#' @export
rr_curie_fields <- function(x) {
  UseMethod("rr_curie_fields")
}

#' @rdname rr_curie
#' @export
rr_curie_fields.register <- function(x) {
  x$schema$fields %>%
  dplyr::filter(datatype == "curie") %>%
  dplyr::pull(field)
}

#' @rdname rr_curie
#' @export
rr_curie_links <- function(x) {
  UseMethod("rr_curie_links")
}

#' @rdname rr_curie
#' @export
rr_curie_links.default <- function(x) {
  purrr::map(x,
             ~ if (length(.x) > 1) {
               purrr::map_chr(.x, ~ rr_parse_curie(.x)$prefix)
             } else {
               rr_parse_curie(.x)$prefix
             }) %>%
    purrr::flatten() %>%
    purrr::discard(is.na) %>%
    purrr::flatten_chr() %>%
    unique() %>%
    sort()
}

#' @rdname rr_curie
#' @export
rr_curie_links.register <- function(x) {
  x$data %>%
  dplyr::select(rr_curie_fields(x)) %>%
  purrr::map(rr_curie_links) %>%
  purrr::flatten_chr() %>%
  unique() %>%
  sort()
}
