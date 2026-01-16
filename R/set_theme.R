#' Set Plot Theme for Various Plot Objects
#'
#' Generic function to set a plot theme for different types of plot objects.
#'
#' @param x A plot object (e.g., a ggplot object).
#' @inheritParams kplot.ggplot
#' @param scale [character] A vector indicating which aesthetics to scale (e.g.,
#'   "color", "fill").
#'
#' @return A plot object with the specified theme applied.
#'
#' @export
#'
set_theme <- function(x, ...) {
  UseMethod("set_theme", x)
}

#' @export
#' @rdname set_theme
#'
set_theme.ggplot <- function(x, theme = NULL, scale, vars, ...) {
  if (is.null(theme)) return(x)

  # colors and fills
  for (i in scale) {
    if (is.null(vars[[i]])) next
    palette <- scales::manual_pal(get_color_palette(x$data[[i]], theme = theme))
    x <- x +
      discrete_scale(i, palette = palette)
  }

  # theme
  theme.fun <- match.fun(sprintf("theme.%s.ggplot", theme))
  x <- x +
    theme.fun(...)

  return(x)
}
