test_that("Check scales", {
  p <- ggseg(atlas=dk, mapping=aes(fill=region)) +
    scale_brain("dk")
  expect_equal(length(p$scales$scales), 3)

  p <- ggseg(atlas=dk, mapping=aes(fill=region)) +
    scale_fill_brain("dk")
  expect_equal(length(p$scales$scales), 3)

  p <- ggseg(atlas=dk, mapping=aes(colour=region)) +
    scale_colour_brain("dk")
  expect_equal(length(p$scales$scales), 3)

  p <- ggseg(atlas=dk, mapping=aes(color=region)) +
    scale_color_brain("dk")
  expect_equal(length(p$scales$scales), 3)

})
