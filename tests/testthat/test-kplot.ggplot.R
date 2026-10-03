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

    expect_s3_class(plot, "kggplot")
    expect_s3_class(kggplot::as_ggplot(plot), "ggplot")
  }
)

test_that("kplot() dispatches on the back end", {
  plot <- kplot(iris, "ggplot", vars = list(x = "Sepal.Length", y = "Sepal.Width"))
  expect_s3_class(plot, "kggplot")
  expect_error(kplot(iris, "unknown_backend"), "kplot.unknown_backend")
})

test_that("kplot.tsstudio falls back to ggplot", {
  df <- data.frame(
    date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 10),
    a = 1:10, b = 10:1
  )
  plot <- kplot.tsstudio(
    df, vars = list(x = "date", y = "all"), output_format = "ggplot"
  )
  expect_s3_class(plot, "kggplot")
  expect_s3_class(kggplot::as_ggplot(plot), "ggplot")
})
