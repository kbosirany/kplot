#' Plot data
#'
#' @description
#' This function is the main function to plot data using different plotting
#' packages.
#'
#' @param x An object to be plotted.
#' @param package The plotting package to use (default is "ggplot").
#' @param ... Additional arguments passed to the specific plotting function.
#'
#' @return A plot of the object `x`.
#'
#' @export
#'
kplot <- function(x, package = "ggplot", ...) {
  match.fun(sprintf("kplot.%s", package))(x, ...)
}
