#' Create Time Series Plots Using TSstudio
#'
#' @description
#' Generate interactive time series visualizations using the TSstudio
#' package, with automatic fallback to ggplot for non-HTML output
#' formats.
#'
#' @param x A data.frame containing time series data with a date/time
#'   column and one or more numeric columns to plot.
#' @param vars A list specifying the variables to plot. Should contain:
#'   \itemize{
#'     \item \code{x}: Name of the date/time column (must be Date,
#'           POSIXct, or POSIXlt)
#'     \item \code{y}: Character vector of column names to plot, or
#'           "all" to plot all columns except the date/time column
#'   }
#' @param type Character string specifying the plot type (passed to
#'   underlying plotting functions).
#' @param slider Logical indicating whether to include an interactive
#'   time slider in the TSstudio plot. Default is \code{TRUE}.
#' @param output_format Character string specifying the output format.
#'   Options are:
#'   \itemize{
#'     \item \code{"TSstudio"}: Generate interactive TSstudio plot
#'           (default for HTML output)
#'     \item \code{"ggplot"}: Generate static ggplot (default for
#'           non-HTML output)
#'   }
#'   By default, automatically selects TSstudio for HTML output or
#'   interactive sessions, and ggplot otherwise.
#' @param ... Additional arguments passed to
#'   \code{\link[TSstudio]{ts_plot}} or \code{\link{kplot.ggplot}},
#'   depending on the output format.
#'
#' @return
#' When \code{output_format = "TSstudio"}, returns a TSstudio plot
#' object (plotly-based interactive plot). When
#' \code{output_format = "ggplot"}, returns the result of
#' \code{\link{kplot.ggplot}}.
#'
#' @details
#' This function provides a flexible interface for creating time series
#' plots that automatically adapts to the output context. In R Markdown
#' documents with HTML output or in interactive R sessions, it produces
#' interactive TSstudio plots. For PDF or Word output, it falls back to
#' static ggplot visualizations.
#'
#' The date/time column specified in \code{vars$x} must be of class
#' Date, POSIXct, or POSIXlt. An error is raised if this requirement
#' is not met.
#'
#' @seealso
#' \code{\link{kplot.ggplot}} for the ggplot fallback method,
#' \code{\link[TSstudio]{ts_plot}} for the underlying TSstudio plotting
#' function
#'
#' @examples
#' \dontrun{
#' # Create sample time series data
#' df <- data.frame(
#'   date = seq.Date(from = as.Date("2020-01-01"),
#'                   to = as.Date("2020-12-31"),
#'                   by = "day"),
#'   value1 = cumsum(rnorm(366)),
#'   value2 = cumsum(rnorm(366))
#' )
#'
#' # Plot a single variable
#' kplot.tsstudio(df, vars = list(x = "date", y = "value1"))
#'
#' # Plot multiple variables
#' kplot.tsstudio(df, vars = list(x = "date",
#'                                 y = c("value1", "value2")))
#'
#' # Plot all variables (except date)
#' kplot.tsstudio(df, vars = list(x = "date", y = "all"))
#'
#' # Force ggplot output
#' kplot.tsstudio(df, vars = list(x = "date", y = "all"),
#'                output_format = "ggplot")
#'
#' # Disable the time slider
#' kplot.tsstudio(df, vars = list(x = "date", y = "value1"),
#'                slider = FALSE)
#' }
#'
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
    return(kplot.ggplot(x, vars = vars, type = type, ...))
  }
  if (!inherits(x[, vars$x, drop = TRUE], c("Date", "POSIXct", "POSIXlt"))) {
    stop("First column must be of class Date, POSIXct, or POSIXlt")
  }
  if ("all" %in% vars$y) vars$y <- colnames(x)[!colnames(x) == vars$x]
  TSstudio::ts_plot(select(x, any_of(c(vars$x, vars$y))), ..., slider = slider)
}
