test_that("check that ggseg_atlas is correct", {
  tt <- data.frame(.long = double(),
                   .lat = double(),
                   .id = character(),
                   .subid = character(),
                   hemi = character(),
                   side = character())

  expect_error(as_ggseg_atlas(tt))

  tt <- data.frame(long = double(),
                   lat = double(),
                   id = character(),
                   hemi = character(),
                   side = character(),
                   region = character())
  expect_warning(as_ggseg_atlas(tt))

  expect_equal(dim(as_ggseg_atlas(dk)), c(90, 6))

})

test_that("check that is_ggseg_atlas works", {
  expect_true(is_ggseg_atlas(dk))

  dt <- data.frame(.long = double(),
                   .lat = double(),
                   .id = character(),
                   area = as.character(),
                   hemi = character(),
                   side = character()
  )

  expect_false(is_ggseg_atlas(dt))

})

