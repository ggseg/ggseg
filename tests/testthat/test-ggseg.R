context("test-ggseg")


test_that("Check that ggseg ggplot object is correct", {
  expect_is(ggseg(),c("gg","ggplot"))

  p <- ggseg()

  # should be a list of 9
  # a ggplot object should be a list of 9
  expect_equal(mode(p), "list")
  expect_equal(length(p), 9)

  ## p$data
  expect_equal(dim(p$data)[1], 10913)
  expect_equal(dim(p$data)[2], 12)
  expect_equal(colnames(p$data)[1], "long")

  ## p$layers
  # should have a defined number of layers
  expect_equal(length(p$layers), 1)
  # https://stackoverflow.com/questions/13457562/how-to-determine-the-geom-type-of-each-layer-of-a-ggplot2-object/43982598#43982598
  # layer should be a specific geom
  expect_equal(class(p$layers[[1]]$geom)[1], "GeomPolygon")
  # if there are multiple layers we can test more than one...

  ## p$labels
  # should have x but not y labels
  expect_equal(p$labels$x, "hemisphere")
  expect_equal(p$labels$y, NULL)
})

test_that("Check that themes are working", {
  expect_is(ggseg() + theme_brain(),c("gg","ggplot"))
  expect_is(ggseg() + theme_darkbrain(),c("gg","ggplot"))
  expect_is(ggseg() + theme_custombrain(),c("gg","ggplot"))
})


test_that("Check that ggseg is working", {
  expect_is(ggseg(),c("gg","ggplot"))
  expect_is(
    ggseg(data=data.frame(
      area = c("transverse temporal", "insula",
               "pre central","superior parietal"),
      p = sample(seq(0,.5,.001), 4),
      stringsAsFactors = FALSE),mapping=aes(fill=p)),
    c("gg","ggplot"))
  expect_is(ggseg(atlas = dkt, mapping=aes(fill=area), position="stacked"),c("gg","ggplot"))
  expect_warning(ggseg(atlas = aseg, position="stacked"))

  expect_warning(
    ggseg(.data=data.frame(
      area = c("transverse tempral", "insula",
               "pre central","superior parietal"),
      p = sample(seq(0,.5,.001), 4),
      stringsAsFactors = FALSE),mapping=aes(fill=p))
  )

  expect_is(ggseg(hemisphere = "left", adapt_scales = T),c("gg","ggplot"))
  expect_is(ggseg(hemisphere = "right", adapt_scales = T),c("gg","ggplot"))
  expect_is(ggseg(view = "lateral", adapt_scales = T),c("gg","ggplot"))
  expect_is(ggseg(view = "medial", adapt_scales = T),c("gg","ggplot"))
})


