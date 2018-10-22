# Functions for set operations

#' List all subsets of a set.
#'
#' @param x A set-like object (coercable to a vector or list).
#' @param minimum The smallest subset to list.
#' @param maximum The largest subset to list.
#' @return A list of subsets, each a set.
subsets <- function(x, minimum = 1, maximum = length(x)) {
  minimum:maximum %>%
    map(~combn(x, m = .x, simplify = FALSE)) %>%
    unlist(recursive = FALSE)
}
