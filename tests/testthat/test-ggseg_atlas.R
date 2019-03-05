context("test-ggseg_atlas")

test_that("check that ggseg_atlas is correct", {
  tt <- data.frame(long = double(),
                   lat = double(),
                   id = character(),
                   hemi = character(),
                   side = character())

  expect_error(as_ggseg_atlas(tt))
  expect_warning(as_ggseg_atlas())

})

test_that("check that ggseg3d_atlas is correct", {
  tt <- data.frame(atlas = character(),
                   surf = character(),
                   hemi = character())

  expect_error(as_ggseg3d_atlas(tt))
  expect_warning(as_ggseg_atlas())

})

