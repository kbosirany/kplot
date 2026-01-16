#' Generate a color palette for a vector of values
#'
#' This function returns a named vector of colors corresponding to the unique
#' values in \code{x}. If the number of unique values is less than or equal to
#' the length of \code{main_colors}, it selects the first \code{n} colors from
#' \code{main_colors}. Otherwise, it generates a palette of \code{n} colors
#' interpolated from \code{main_colors}.
#'
#' @param x A vector of values for which a color palette is needed.
#' @inheritParams kplot.ggplot
#' @param main_colors A character vector of color names or hex codes to use as
#' the base palette.
#' @template cfg
#'
#' @return A named character vector of colors, with names corresponding to the
#' unique values in \code{x}.
#'
#' @examples
#' get_color_palette(c("A", "B", "C"), main_colors = c("red", "green", "blue"))
#'
#' @export
#'
get_color_palette <- function(
  x, theme, main_colors = cfg$themes[[theme]]$colors, cfg = loadConfig()
) {
  if (missing(main_colors) && missing(theme)) {
    stop("Argument 'theme' is required.")
  }
  if (!missing(theme) && !theme %in% names(cfg$themes)) {
    stop(paste0("Theme '", theme, "' not found in configuration."))
  }
  n <- length(unique(x))
  if (n <= length(main_colors)) {
    palette <- main_colors[1:n]
  } else {
    if (n > 100) message("Can't generate palette for more than 100 colors")
    pal_fun <- grDevices::colorRampPalette(main_colors)
    palette <- pal_fun(n)
  }
  return(stats::setNames(palette, unique(x)))
}
