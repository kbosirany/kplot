#' Plot data with ggplot
#'
#' @description
#' The ggplot back end of [kplot()]. The implementation lives in the
#' \pkg{kggplot} package: this function is a thin wrapper around
#' [kggplot::kggplot()], and accepts all its arguments (`vars`, `type`,
#' `labels`, `theme`, ... or the shorter `x`, `y`, `color` forms).
#'
#' @param x Data to plot (data frame, `ts`, matrix, ...).
#' @param ... Arguments passed to [kggplot::kggplot()].
#'
#' @return A `kggplot` object (see [kggplot::as_ggplot()] to get a ggplot).
#'
#' @examples
#' kplot.ggplot(
#'   iris,
#'   vars = list(x = "Sepal.Length", y = "Sepal.Width", color = "Species"),
#'   type = "point",
#'   labels = list(
#'     title = "Iris Sepal Dimensions", x = "Sepal Length", y = "Sepal Width"
#'   )
#' )
#'
#' @export
#'
kplot.ggplot <- function(x, ...) {
  kggplot::kggplot(x, ...)
}
