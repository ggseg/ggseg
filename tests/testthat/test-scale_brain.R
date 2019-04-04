context("test-scale_brain")

test_that("Check scales", {
  p <- ggseg(atlas=dkt, mapping=aes(fill=area)) +
    scale_brain("dkt")
  expect_equal(length(p$scales$scales), 3)

  p <- ggseg(atlas=dkt, mapping=aes(fill=area)) +
    scale_fill_brain("dkt")
  expect_equal(length(p$scales$scales), 3)

  p <- ggseg(atlas=dkt, mapping=aes(colour=area)) +
    scale_colour_brain("dkt")
  expect_equal(length(p$scales$scales), 3)

  p <- ggseg(atlas=dkt, mapping=aes(color=area)) +
    scale_color_brain("dkt")
  expect_equal(length(p$scales$scales), 3)

})
