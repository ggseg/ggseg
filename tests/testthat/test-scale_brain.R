context("test-scale_brain")

test_that("Check scales", {
  p <- ggseg(atlas=dk, mapping=aes(fill=area)) +
    scale_brain("dk")
  expect_equal(length(p$scales$scales), 3)

  p <- ggseg(atlas=dk, mapping=aes(fill=area)) +
    scale_fill_brain("dk")
  expect_equal(length(p$scales$scales), 3)

  p <- ggseg(atlas=dk, mapping=aes(colour=area)) +
    scale_colour_brain("dk")
  expect_equal(length(p$scales$scales), 3)

  p <- ggseg(atlas=dk, mapping=aes(color=area)) +
    scale_color_brain("dk")
  expect_equal(length(p$scales$scales), 3)

})
