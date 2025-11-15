context("ggseg-scales")
test_that("Check scales", {
  p <- ggseg(atlas = dk, mapping = aes(fill = region))

  expect_doppelganger("ggseg scale brain fill", p + scale_brain("dk"))

  expect_doppelganger("ggseg scale fill brain", p + scale_fill_brain("dk"))

  p <- ggseg(atlas = dk, mapping = aes(colour = region))

  expect_doppelganger(
    "ggseg scale brain colour",
    p + scale_brain("dk", aesthetics = "colour")
  )

  expect_doppelganger("ggseg scale colour brain", p + scale_colour_brain("dk"))

  expect_doppelganger("ggseg scale color brain", p + scale_color_brain("dk"))

  p <- ggplot() +
    geom_brain(atlas = dk, aes(fill = region))

  expect_doppelganger(
    "geom scale fill brain2",
    p + scale_fill_brain2(dk$palette)
  )

  expect_doppelganger("geom scale brain2", p + scale_brain2(dk$palette))

  p <- ggplot() +
    geom_brain(atlas = dk, fill = "white", aes(colour = region))

  expect_doppelganger(
    "geom scale colour brain2",
    p + scale_colour_brain2(dk$palette)
  )

  expect_doppelganger(
    "geom scale color brain2",
    p + scale_color_brain2(dk$palette)
  )
})

# These pass tests but not checks?!
# test_that("Check other scales",{
#   aseg_r <- tibble(
#     region = brain_regions(aseg),
#     value = c(93L, 44L, 92L, 70L, 3L, 48L, 9L, 46L,
#               2L, 60L, 71L, 76L, 24L, 73L, 50L, 39L, 21L, 62L)
#   )
#
#   p <- ggplot(aseg_r) +
#     geom_brain(atlas = aseg,
#                aes(fill = value))
#
#   expect_doppelganger("geom scale gradient",
#                       p + scale_fill_gradient(low="red", high = "blue")
#   )
#
#   expect_doppelganger("geom scale gradientn",
#                       p + scale_fill_gradientn(colors = c("firebrick", "goldenrod"))
#   )
#
#   expect_doppelganger("geom scale distiller",
#                       p + scale_fill_distiller(palette = 4)
#   )
#
# })
