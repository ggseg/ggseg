

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
  expect_is(ggseg(atlas = yeo17, mapping=aes(fill=area),adapt.scales = F ) +
              scale_brain("yeo17"),c("gg","ggplot"))
  expect_is(ggseg(atlas = yeo17, mapping=aes(color=area),adapt.scales = F ) +
              scale_color_brain("yeo17"),c("gg","ggplot"))
  expect_error(ggseg(atlas = yeo17, mapping=aes(fill=area),adapt.scales = F ) +
                 scale_brain("yeo"))
})

test_that("Check that themes are working", {
  expect_is(ggseg() + theme_brain(),c("gg","ggplot"))
  expect_is(ggseg() + theme_darkbrain(),c("gg","ggplot"))
  expect_is(ggseg() + theme_custombrain(),c("gg","ggplot"))
})


test_that("Check that ggseg is working", {
  expect_is(ggseg(),c("gg","ggplot"))
  expect_is(ggseg(atlas = yeo17),c("gg","ggplot"))
  expect_is(ggseg(atlas = yeo17, mapping=aes(fill=area),adapt.scales = F ),c("gg","ggplot"))
  expect_is(ggseg(atlas = dkt, mapping=aes(fill=area), position="stacked"),c("gg","ggplot"))
  expect_error(ggseg(atlas = aseg, position="stacked"))
  expect_error(ggseg(plot.areas = "default"))

})


