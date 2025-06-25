#' Pipe operator
#'
#' See \code{magrittr::\%>\%} for details.
#'
#' @name %>%
#' @rdname pipe
#' @importFrom magrittr %>%
NULL

#' Return True/False depending on if value is zero or NA
#'
#' @param x A vector
#' @return A boolean vector of whether each index is a zero or NA
#' @export
#'
is_zero_or_na <- function(x) {
ifelse(x == 0 | is.na(x), T, F)
}
#'
#'
#'
