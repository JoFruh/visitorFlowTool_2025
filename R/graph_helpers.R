#### Converting a graph's nodes/edges to a table, on any tibble version ####

# The ABM failed on the server with
#
#   Agent-Based Model failed: Error in as_tibble(edge_attr(x)):
#   All columns in a tibble must be vectors.
#
# That message comes from tidygraph's edge_tibble(), which is literally
# `as_tibble(edge_attr(x))`, reached from `as_tibble(network %>% activate(edges))`
# in launchSim_v2.R. The offending column is SHAPE, the edges' sf geometry: an
# `sfc` is a CLASSED LIST, and tibble < 3.3 / vctrs < 0.7 refuse to accept one as
# a column. Newer versions accept it, which is why the dev box could never
# reproduce it - across 473 real save files SHAPE is the ONLY non-vector edge
# attribute, and all 473 convert cleanly on tibble 3.3.1 / vctrs 0.7.3.
#
# DESCRIPTION carries `tibble (>= 3.3.0)` and `vctrs (>= 0.7.0)` floors so a
# correct install cannot have the old pair, and upgrading the server remains the
# real fix. This exists so that a stale or partially-upgraded install degrades
# into a working app rather than a job that dies inside a worker, where the
# traceback points at tidygraph rather than at the package versions.

#' Nodes or edges of a graph as a tibble, without tibble's column validation.
#'
#' Mirrors tidygraph's node_tibble()/edge_tibble() exactly - same columns, same
#' order (from, to, then the edge attributes), same row.names handling - but
#' assembles the result by setting the class on a plain list instead of going
#' through as_tibble(). A data frame IS a list of equal-length columns, so the
#' object is identical; what is skipped is the validation that rejects `sfc`.
#'
#' Falls back to tidygraph for a focused graph: focusing is not used anywhere in
#' this app, and reimplementing focus_ind() to cover it would be more code than
#' the case is worth. Verified equivalent to the tidygraph path on every step-5
#' network in the saved files.
#'
#' @param graph an igraph/tbl_graph.
#' @param what "edges" or "nodes".
vftGraphTibble <- function(graph, what = c("edges", "nodes")){
  what <- match.arg(what)

  #A focused graph needs tidygraph's row subsetting; nothing here focuses, so
  #hand those back rather than reimplement it. Tested by class rather than with
  #tidygraph::is.focused_tbl_graph(), which is NOT exported - reaching for it
  #with :: would error on every call and silently take the fallback branch.
  #tidygraph's own predicate is exactly this inherits() check.
  if(inherits(graph, "focused_tbl_graph")){
    return(dplyr::as_tibble(
      if(what == "edges") tidygraph::activate(graph, "edges")
      else                tidygraph::activate(graph, "nodes")))
  }

  if(what == "edges"){
    n     <- igraph::gsize(graph)
    eList <- igraph::as_edgelist(graph, names = FALSE)
    mode(eList) <- "integer"
    #from/to first, exactly as edge_tibble() binds them
    cols  <- c(list(from = eList[, 1], to = eList[, 2]), igraph::edge_attr(graph))
  }else{
    n    <- igraph::gorder(graph)
    cols <- igraph::vertex_attr(graph)
  }

  cols <- as.list(cols)
  #an attribute of the wrong length would make a corrupt data frame rather than
  #an error, so refuse instead - a silent truncation deep inside the ABM would be
  #far harder to find than a message here.
  lens <- vapply(cols, length, integer(1))
  if(length(lens) && any(lens != n)){
    stop("visitorFlowTool: graph ", what, " attribute(s) ",
         paste(names(cols)[lens != n], collapse = ", "),
         " have length ", paste(unique(lens[lens != n]), collapse = "/"),
         " but the graph has ", n, " ", what, ".")
  }

  attr(cols, "row.names") <- .set_row_names(n)
  class(cols) <- c("tbl_df", "tbl", "data.frame")
  cols
}
