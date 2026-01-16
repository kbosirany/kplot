devtools::load_all()
cfg <- loadConfig()

library(ggplot2)
# library(InraeThemes)
# devtools::load_all("../InraeThemes")

# test <- readRDS(system.file("extdata/OM.RDS", package = "kplot")) %>%
#   as.data.frame()

data(iris)

test <- iris

plot <- kggplot(
  test, plot_type = "point",
  vars = list(
    x = "Sepal.Length", y = c("Sepal.Width", "Petal.Width"), color = "Species"
  ),
  labels = list(title = "Test", x = "Sepal.Length", y = "Sepal.Width"),
  show.legend = FALSE, base_size = 8
)

print(plot)

palette <- scales::manual_pal(getPalette(plot$data$color))

print(
  plot +
    theme_inrae(base_size = 8) +
    scale_color_inrae(palette = palette) +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
)

colors <- c(
  "#00a3a6", "#9dc544", "#9ed6e3", "#423089", "#ed6e6c", "#797870",
  "#c4c0b3"
)

plot2 <- plot

for (i in seq.int(length(vec))) {
  plot2 <- plot2 +
    geom_point(
      data = test %>% mutate(LAI = LAI + i * 0.5),
      aes(x = dates, y = LAI), color = palette[i]
    )
}

print(plot2)

print(
  ggplot(
    test %>% mutate(col = "Test"), aes(x = dates, y = LAI, color = col)
  ) +
    geom_line() +
    scale_color_inrae() +
    theme_inrae() +
    labs(col = NULL)
)


library(InraeThemes)
library(ggplot2)
library(palmerpenguins)

# If needed
# sysfonts::font_add_google("Raleway")

# Load the fonts
extrafont::loadfonts(quiet = TRUE)

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Relation entre la longueur des nageoires et la masse corporelle",
    subtitle = "Données des pingouins par espèce",
    x = "Longueur des nageoires (mm)",
    y = "Masse corporelle (g)"
  ) +
  theme_inrae() +
  scale_color_inrae()