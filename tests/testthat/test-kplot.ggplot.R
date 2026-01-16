data(iris)

test_that(
  "kplot.ggplot works correctly",
  {
    plot <- kplot.ggplot(
      iris,
      vars = list(x = "Sepal.Length", y = "Sepal.Width", color = "Species"),
      type = "point",
      labels = list(
        title = "Iris Sepal Dimensions", x = "Sepal Length", y = "Sepal Width"
      )
    )

    expect_s3_class(plot, "ggplot")
  }
)
