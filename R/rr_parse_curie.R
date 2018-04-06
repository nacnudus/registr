#' Parse a CURIE into prefix and reference
#'
#' @param curie Character of the form `"prefix:reference"` or `"prefix:"`.
#' @export
#' @examples
#' rr_parse_curie("country:GB")
#' rr_parse_curie("country:")
#' rr_parse_curie("country")
rr_parse_curie <- function(curie) {
  prefix <- stringr::str_extract(curie, "^[^:]+(?=:)")
  reference <- stringr::str_extract(curie, "(?<=:)[^:]+$")
  list(prefix = prefix, reference = reference)
}

rr_curie_prefixes <- function(curies) {
  if (is.vector(curies)) {
    purrr::map_chr(curies, ~ rr_curie_prefixes(.x)$prefix)
  } else {
    rr_curie_prefixes(.x)$prefix
  }
}
