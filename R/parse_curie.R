#' Parse a CURIE into prefix and reference
#'
#' @param curie Character of the form `"prefix:reference"` or `"prefix:"`.
#' @export
#' @examples
#' parse_curie("country:GB")
#' parse_curie("country:")
#' parse_curie("country")
parse_curie <- function(curie) {
  prefix <- stringr::str_extract(curie, "^[^:]+(?=:)")
  reference <- stringr::str_extract(curie, "(?<=:)[^:]+$")
  list(prefix = prefix, reference = reference)
}
