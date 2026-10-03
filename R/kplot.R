#' Plot data
#'
#' @description
#' Entry point to plot data with different plotting back ends. `package`
#' selects the back end: `kplot(x, "ggplot", ...)` calls [kplot.ggplot()]
#' (implemented by the \pkg{kggplot} package), `kplot(x, "tsstudio", ...)`
#' calls [kplot.tsstudio()]. Any function named `kplot.<package>` is a valid
#' back end.
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
