context("ggseg-scales")
test_that("Check scales", {
  p <- ggseg(atlas=dk,
             mapping=aes(fill=region))

  expect_doppelganger("ggseg scale brain fill",
                      p + scale_brain("dk"))

  expect_doppelganger("ggseg scale fill brain",
                      p + scale_fill_brain("dk"))


  p <- ggseg(atlas=dk,
             mapping=aes(colour=region))

  expect_doppelganger("ggseg scale brain colour",
                      p + scale_brain("dk",
                                      aesthetics = "colour"))

  expect_doppelganger("ggseg scale colour brain",
                      p + scale_colour_brain("dk"))

  expect_doppelganger("ggseg scale color brain",
                      p + scale_color_brain("dk"))

  p <-   ggplot() +
    geom_brain(atlas = dk, aes(fill = region))

  expect_doppelganger("geom scale fill brain2",
                      p + scale_fill_brain2(dk$palette))

  expect_doppelganger("geom scale brain2",
                      p + scale_brain2(dk$palette))

  p <- ggplot() +
    geom_brain(atlas = dk, fill = "white",
               aes(colour = region))

  expect_doppelganger("geom scale colour brain2",
                      p + scale_colour_brain2(dk$palette))

  expect_doppelganger("geom scale color brain2",
                      p + scale_color_brain2(dk$palette))


})
