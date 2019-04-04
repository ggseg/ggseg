context("test-ggseg3d")

test_that("Check that ggseg3d is working", {
  p = ggseg3d()
  expect_is(p, c("plotly", "htmlwidget"))
  expect_equal(length(p$x), 7)

  expect_error(ggseg3d(atlas=dkt), "not a 3d atlas")
  expect_error(ggseg3d(atlas=hhj), "object 'hhj")
  expect_error(ggseg3d(atlas=dkt_3d, hemisphere = "hi"), "hemisphere")


  expect_warning(
    ggseg3d(.data=data.frame(
      area = c("transverse tempral", "insula",
               "pre central","superior parietal"),
      p = sample(seq(0,.5,.001), 4), stringsAsFactors = F),
      colour = "p")
  )

  expect_error(
    ggseg3d(.data=data.frame(
      area = c("transverse temporal", "insula",
               "pre central","superior parietal"),
      p = sample(seq(0,.5,.001), 4), stringsAsFactors = F),
      colour = "p", palette="ponyomedium")
  )

  someData <- data.frame(
    area = c("transverse temporal", "insula",
             "pre central","superior parietal"),
    p = sample(seq(0,.5,.001), 4),
    stringsAsFactors = F)

  expect_is(
    ggseg3d(.data=someData,
      colour = "p", text="p", palette=c("black", "white")),
    c("plotly", "htmlwidget")
  )

  expect_is(
    ggseg3d(.data=someData,
      colour = "p", text="p", palette=c("black", "white"),
      show.legend = T),
    c("plotly", "htmlwidget")
  )

  expect_is(
    ggseg3d(.data=someData,
      colour = "p", text="p", palette=c("black", "white"),
      remove.axes = F),
    c("plotly", "htmlwidget")
  )

  expect_is(
    ggseg3d(.data=someData,
      colour = "p", text="p", palette=c("black", "white"),
      remove.axes = F, camera = list(eye = list(x = 2, y = 0, z = 1))),
    c("plotly", "htmlwidget")
  )

  expect_is(
    ggseg3d(.data=someData,
      colour = "p", text="p", palette=c("black", "white"),
      remove.axes = F, camera = "medial"),
    c("plotly", "htmlwidget")
  )

  expect_is(
    ggseg3d(.data=someData,
      colour = "p", text="p", palette=c("black", "white"),
      remove.axes = F, camera = "medial", na.alpha = .5),
    c("plotly", "htmlwidget")
  )

  expect_error(ggseg3d(atlas = aseg_3d, surface = "white"), "no surface")

})



test_that("Check glassbrain", {
  p = ggseg3d(atlas = aseg_3d,
              glassbrain = .5)
  expect_is(p, c("plotly", "htmlwidget"))
  expect_equal(length(p$x), 7)

  p = ggseg3d(atlas = aseg_3d,
              glassbrain = .5,
              glassbrain_hemisphere = "right")
  expect_is(p, c("plotly", "htmlwidget"))
  expect_equal(length(p$x), 7)

  p = ggseg3d(atlas = aseg_3d,
              glassbrain = .5,
              glassbrain_hemisphere = "left",
              glassbrain_colour = "red")
  expect_is(p, c("plotly", "htmlwidget"))
  expect_equal(length(p$x), 7)

})
