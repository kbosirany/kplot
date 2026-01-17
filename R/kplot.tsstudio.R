#' @export
#'
kplot.tsstudio <- function(x, ...) {
  UseMethod("kplot.tsstudio", x)
}

#' @export
#' @rdname kplot.tsstudio
#'
kplot.tsstudio.data.frame <- function(
  x,
  vars,
  type,
  slider = TRUE,
  output_format = ifelse(
    knitr::is_html_output() || interactive(), "TSstudio", "ggplot"
  ),
  ...
) {
  if (output_format != "TSstudio") {
    return(geauplot.ggplot(x, vars = vars, type = type, ...))
  }
  if (!inherits(x[, vars$x, drop = TRUE], c("Date", "POSIXct", "POSIXlt"))) {
    stop("First column must be of class Date, POSIXct, or POSIXlt")
  }
  if ("all" %in% vars$y) vars$y <- colnames(x)[!colnames(x) == vars$x]
  TSstudio::ts_plot(select(x, any_of(c(vars$x, vars$y))), ..., slider = slider)
}
