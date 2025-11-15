test_that("check that ggseg_atlas is correct", {
  tt <- data.frame(
    .long = double(),
    .lat = double(),
    .id = character(),
    hemi = character(),
    side = character()
  )

  expect_error(as_ggseg_atlas(tt))

  tt <- data.frame(
    .long = double(),
    .lat = double(),
    .id = character(),
    .subid = character(),
    region = character(),
    atlas = character(),
    type = character(),
    hemi = character(),
    side = character()
  )
  atlas <- as_ggseg_atlas(tt)
  expect_equal(
    names(atlas),
    c("atlas", "type", "hemi", "side", "region", "ggseg")
  )
  expect_equal(typeof(atlas$ggseg), "list")

  expect_equal(dim(as_ggseg_atlas(dk)), c(90, 8))
})

test_that("check that is_ggseg_atlas works", {
  dk2 <- as_ggseg_atlas(dk)
  expect_true(is_ggseg_atlas(dk2))

  dt <- data.frame(
    .long = double(),
    .lat = double(),
    .id = character(),
    area = as.character(),
    hemi = character(),
    side = character()
  )

  expect_false(is_ggseg_atlas(dt))
})


test_that("check that as_ggseg_atlas works", {
  dk2 <- as_ggseg_atlas(dk)
  expect_true(is_ggseg_atlas(dk2))

  expect_error(as_ggseg_atlas(list()), "Cannot make object of class")

  expect_error(as_ggseg_atlas(data.frame()), "missing necessary columns")

  dt <- data.frame(
    .long = double(),
    .lat = double(),
    .id = character(),
    atlas = character(),
    region = character(),
    .subid = character(),
    type = character(),
    area = character(),
    hemi = character(),
    side = character()
  )
  k <- as_ggseg_atlas(dt)
  expect_true(is_ggseg_atlas(k))

  k <- as_ggseg_atlas(dk)
  k <- as_ggseg_atlas(k)
  expect_true(inherits(k, "ggseg_atlas"))
})

test_that("brain-polygon", {
  ka <- as_ggseg_atlas(dk)

  expect_true(inherits(ka$ggseg, "brain_polygon"))
  expect_true(is_brain_polygon(ka$ggseg))

  ka <- as.list(ka$ggseg)
  expect_true(inherits(ka, "list"))

  expect_true(inherits(as_brain_polygon(ka), "brain_polygon"))

  ka <- brain_polygon(ka)
  expect_true(inherits(ka, "brain_polygon"))

  expect_equal(capture.output(ka[1]), "< p:  1 - v: 12>")
})


context("ggseg-atlas")
test_that("ggseg_atlas S3 methods work", {
  dk2 <- as_ggseg_atlas(dk)

  expect_doppelganger("ggseg_atlas plot dk", plot(dk2))

  k <- capture.output(dk2)
  expect_equal(k[1], "# ggseg atlas")
})
