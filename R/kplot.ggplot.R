#' Plot data with ggplot
#'
#' @description
#' Generic and method specific functions to plot data using ggplot2.
#'
#' @param x Data to plot.
#' @param vars [list] Variables to plot
#' @param type [character] Type of plot to create (e.g., "point", "line",
#' "density", "cumFreq", "bar", "bar_dodge", "convexhull").
#' @param labels [list] Labels for the plot (title, x, y, color, fill).
#' @param theme [character] Theme to apply to the plot.
#' @param ... Additional arguments passed to the plotting method.
#'
#' @return A ggplot object.
#'
#' @examples
#' data(iris)
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
  UseMethod("kplot.ggplot", x)
}

#' @export
#' @rdname kplot.ggplot
#'
kplot.ggplot.data.frame <- function(
  x,
  vars,
  type,
  labels = list(
    title = NULL,
    x = NULL,
    y = NULL,
    color = vars$color,
    fill = vars$fill
  ),
  theme = NULL,
  ...
) {
  # Format the data
  data <- rename(x, x = .data[[vars$x]])

  vars$x <- "x"

  if ("all" %in% vars$y) vars$y <- colnames(data)[!colnames(data) == vars$x]

  if (!is.null(vars$group)) {
    stopifnot(length(vars$group) == 1, vars$group %in% colnames(data))
  }

  scale <- unique(
    c(
      switch(
        type,
        point = "color",
        line = "color",
        bar = "fill"
      ),
      names(vars)[names(vars) %in% c("color", "fill", "group")]
    )
  )

  for (i in scale) {
    if (is.null(vars[[i]])) {
      data <- mutate(data, !!sym(i) := NA)
    } else {
      # stopifnot(vars[[i]] %in% colnames(data))
      # data <- mutate(data, !!sym(i) := .data[[vars[[i]]]])
      if (vars[[i]] %in% colnames(data)) {
        data <- mutate(data, !!sym(i) := .data[[vars[[i]]]])
      } else {
        data <- mutate(data, !!sym(i) := vars[[i]])
      }
    }

    vars[[i]] <- i
  }

  if (length(vars$y) == 1) {
    data <- data %>%
      select(any_of(c(vars$x, vars$y, vars$color, vars$fill, vars$group))) %>%
      rename(y_values = .data[[vars$y]]) %>%
      mutate(y_vars = vars$y)
  } else {
    data <-  data %>%
      select(any_of(c(vars$x, vars$y, vars$color, vars$fill, vars$group))) %>%
      pivot_longer(any_of(vars$y), names_to = "y_vars", values_to = "y_values")
  }

  if (all(is.na(data$color))) data$color <- data$y_vars

  # Create the ggplot object
  plot <- ggplot(
    data,
    aes(
      x = .data$x,
      y = .data$y_values,
      color = get_aes_values(.data, vars$color),
      fill = get_aes_values(.data, vars$fill),
      group = get_aes_values(.data, vars$group)
    )
  )

  # Add the appropriate geom based on the type
  switch(
    type,
    "point" = {
      plot <- plot +
        geom_point(...)
    },
    "line" = {
      plot <- plot +
        geom_line(...)
    },
    "density" = {
      plot <- plot +
        geom_density() +
        expand_limits(y = 0)
    },
    "cumFreq" = {
      plot <- plot +
        geom_line(stat = "ecdf")
    },
    "bar" = {
      plot <- plot +
        geom_bar(stat = "identity", ...)
    },
    "bar_dodge" = {
      plot <- plot +
        geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.8))
    },
    "convexhull" = {
      plot <- plot +
        ggConvexHull::geom_convexhull(...)
    }
  )

  # colors and theme
  plot <- set_theme(
    plot,
    theme = theme,
    scale = scale,
    vars = vars,
    ...
  )

  # Labels
  for (i in c("color", "fill")) {
    if (!is.null(vars[[i]])) labels[[i]] <- vars[[i]]
  }

  plot <- plot +
    do.call(labs, labels[!sapply(labels, is.null)])

  return(plot)
}

#' Get Aesthetic Value from Data
#'
#' This function retrieves a specific aesthetic value from a data frame or list.
#' If the specified aesthetic is not found, it returns NULL.
#'
#' @param data A data frame or list containing the aesthetic values.
#' @param which A character string specifying the name of the aesthetic to
#' retrieve.
#'
#' @return The value of the specified aesthetic from the data, or NULL if not
#' found.
#'
#' @noRd
#'
get_aes_values <- function(data, which) {
  if (is.null(which)) return(NULL)
  return(data[[which]])
}
