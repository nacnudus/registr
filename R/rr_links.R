#' Links between registers
#'
#' @description
#' Registers can link to each other either by a field with the `"register"`
#' property, or via CURIEs.  See [rr_key_links()] and [rr_curie_links()] for
#' more about that.  [rr_links()] returns a data frame of edges.
#'
#' @param x Object of class `"register"`, or a list of objects of class
#' `"register"`.  If a list of registers, the graph of all links between all
#' registers will be returned.
#'
#' @export
#' @examples
#' sg <- rr_register("statistical-geography")
#' rr_links(sg)
#'
#' allergen <- rr_register("allergen")
#' rr_links(allergen)
#'
#' registers <- rr_registers()
#' rr_links(registers)
#'
#' if (require(tidygraph)) {
#'   as_tbl_graph(rr_links(rr_register("statistical-geography"))) %>%
#'     ggraph(layout = "nicely") +
#'       geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
#'       geom_edge_loop() +
#'       geom_node_label(aes(label = name))
#'
#'   as_tbl_graph(rr_links(rr_registers())) %>%
#'     ggraph(layout = "nicely") +
#'       geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
#'       geom_edge_loop() +
#'       geom_node_label(aes(label = name))
#' }
rr_links <- function(x) {
  UseMethod("rr_links")
}

#' @export
rr_links.register <- function(x) {
  name <- rr_snapshot(x)$schema$names$name
  key_links <- rr_key_links(x)
  curie_links <- rr_curie_links(x)
  dplyr::bind_rows(
    tibble::tibble(from = name, to = key_links, type = "key"),
    tibble::tibble(from = name, to = curie_links, type = "curie")
    )
}

#' @export
rr_links.list <- function(x) {
  purrr::map_dfr(x, rr_links)
}
