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

#' The seven per-activity attractiveness columns the edit handlers adjust.
VFT_DULN_COLS <- c("DULN_WALK_", "DULN_WALK1", "DULN_BIKER", "DULN_EBIKE",
                   "DULN_JOGGE", "DULN_DOG_N", "DULN_DOG_P")

#' Add `deltas` to one edge's attractiveness, and to both of its end nodes.
#'
#' What the newVersions edit handlers (submitting a changed or a new path) do
#' after the user picks signage, surface and width - written as one pass over a
#' local graph instead of the ~21 separate statements of the form
#'
#'   igraph::E(r$networkList[[pos]]$network)[[.data$edgeID_2 == id]]$X <-
#'     igraph::E(r$networkList[[pos]]$network)[.data$edgeID_2 == id]$X + d["X"]
#'
#' each of which re-scanned every edge for the id and copied the whole graph out
#' of and back into the reactive list: ~2.7 s of shared main thread per click on
#' a 47k-edge network. The arithmetic and its order are unchanged - the edge's
#' seven columns, then the `to_2` node's seven, then the `from_2` node's seven,
#' each read after the one before was written - so a path whose two ends are the
#' same node still gets the delta twice, as it did.
#'
#' @param graph the scenario's graph.
#' @param edgeID the edge's `edgeID_2`.
#' @param deltas a numeric vector named by VFT_DULN_COLS (a missing name adds NA,
#'   as indexing the named vector did).
#' @return the updated graph. Write it back ONCE.
vftShiftAttractivity <- function(graph, edgeID, deltas){
  #The attribute lists are edited as plain R vectors and handed back in one
  #assignment each: every igraph::set_*_attr() call copies the whole graph -
  #geometry included - so doing it per column cost ~0.1 s each on a large
  #network, most of what this function exists to save.
  ea <- igraph::edge_attr(graph)
  ei <- which(ea[["edgeID_2"]] == edgeID)
  for(a in VFT_DULN_COLS) ea[[a]][ei] <- ea[[a]][ei] + deltas[a]

  #V(g)[k] with a number is the k-th vertex, and to_2/from_2 hold vertex
  #positions - the same indexing the statements used
  va <- igraph::vertex_attr(graph)
  for(vi in list(ea[["to_2"]][ei], ea[["from_2"]][ei])){
    for(a in VFT_DULN_COLS) va[[a]][vi] <- va[[a]][vi] + deltas[a]
  }

  igraph::edge_attr(graph)   <- ea
  igraph::vertex_attr(graph) <- va
  graph
}
