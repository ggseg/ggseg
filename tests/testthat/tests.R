#covr::zero_coverage(package_coverage("testcovr"))


test_that("Test that palette display works", {
  expect_is(display.brain.pal(),c("gg","ggplot"))
  expect_is(display.brain.pal(name = "yeo17"),c("gg","ggplot"))
  expect_is(display.brain.pal(n=1:7),c("gg","ggplot"))
  expect_is(display.brain.pal(n=7),c("gg","ggplot"))
  expect_is(display.brain.pal(name="yeo7", n=7),c("gg","ggplot"))
  expect_warning(display.brain.pal(name = "dkt",n=2))
  expect_warning(display.brain.pal(name = "dkt",n=48))
  expect_error(display.brain.pal(name = "et",n=48))
})


test_that("Check that palette extraction happens ok", {
  expect_is(class(brain.pal.info),"character")
  expect_equal(length(brain_pal("yeo17",3)), 3)
  expect_equal(length(brain_pal("yeo7","all")), 7)
  expect_equal(length(brain_pal("yeo7",1:7)), 7)
  expect_equal(length(brain_pal("dkt",13, unname=TRUE)), 13)
  expect_equal(length(brain_pal("dkt",13, direction=-1)), 13)
  expect_warning(brain_pal("yeo17",18))
  expect_warning(brain_pal("yeo17",2))
  expect_error(brain_pal("yeo"))
})


test_that("Check that scales are working", {
  expect_equal(mode(adapt_scales(dkt)), "list")
  expect_equal(mode(adapt_scales(yeo7)), "list")
  expect_equal(mode(adapt_scales(yeo17)), "list")
  expect_equal(mode(adapt_scales(midsagittal)), "list")
  expect_equal(mode(adapt_scales(aseg)), "list")
  expect_equal(mode(adapt_scales(jhu)), "list")
  expect_equal(mode(adapt_scales(glasser)), "list")

  expect_warning(adapt_scales(glasser %>% select(-atlas)))

})

test_that("Check that adapt_scales is working", {
  expect_is(ggseg(atlas = yeo17, mapping=aes(fill=area),adapt_scales = F ) +
              scale_brain("yeo17"),c("gg","ggplot"))
  expect_is(ggseg(atlas = yeo17, mapping=aes(color=area),adapt_scales = F ) +
              scale_color_brain("yeo17"),c("gg","ggplot"))
  expect_is(ggseg(atlas = yeo17, mapping=aes(colour=area),adapt_scales = F ) +
              scale_colour_brain("yeo17"),c("gg","ggplot"))
  expect_error(ggseg(atlas = yeo17, mapping=aes(fill=area),adapt_scales = F ) +
                 scale_fill_brain("yeo"))
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
  expect_is(ggseg(atlas = yeo17),c("gg","ggplot"))
  expect_is(ggseg(atlas = yeo17, mapping=aes(fill=area), adapt_scales = F ),c("gg","ggplot"))
  expect_is(ggseg(atlas = dkt, mapping=aes(fill=area), position="stacked"),c("gg","ggplot"))
  expect_warning(ggseg(atlas = aseg, position="stacked"))
  expect_error(ggseg(plot.areas = "default"))
  expect_is(ggseg(atlas = yeo7, plot.areas = "default"),c("gg","ggplot"))

  expect_warning(
    ggseg(data=data.frame(
      area = c("transverse tempral", "insula",
               "pre central","superior parietal"),
      p = sample(seq(0,.5,.001), 4),
      stringsAsFactors = FALSE),mapping=aes(fill=p))
    )

  expect_is(ggseg(hemisphere = "left", adapt_scales = T),c("gg","ggplot"))
  expect_is(ggseg(view = "lateral", adapt_scales = T),c("gg","ggplot"))
})


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


