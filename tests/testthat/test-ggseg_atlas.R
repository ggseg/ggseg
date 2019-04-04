context("test-ggseg_atlas")

test_that("check that ggseg_atlas is correct", {
  tt <- data.frame(.long = double(),
                   .lat = double(),
                   .id = character(),
                   hemi = character(),
                   side = character())

  expect_error(as_ggseg_atlas(tt))

  tt <- data.frame(long = double(),
                   lat = double(),
                   id = character(),
                   hemi = character(),
                   side = character(),
                   area = character())
  expect_warning(as_ggseg_atlas(tt))

  expect_equal(dim(as_ggseg_atlas(dkt)), c(80,6))

})

test_that("check that ggseg3d_atlas is correct", {
  tt <- data.frame(atlas = character(),
                   surf = character(),
                   hemi = character())

  expect_error(as_ggseg3d_atlas(tt))
  expect_warning(as_ggseg_atlas())

})

test_that("check that is_ggseg_atlas works", {
  expect_true(is_ggseg_atlas(dkt))
  expect_true(is_ggseg_atlas(dkt_3d))

  dt <- data.frame(.long = double(),
                   .lat = double(),
                   .id = character(),
                   area = as.character(),
                   hemi = character(),
                   side = character()
  )

  expect_false(is_ggseg_atlas(dt))

})

