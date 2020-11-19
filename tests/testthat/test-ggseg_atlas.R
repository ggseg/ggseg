test_that("check that ggseg_atlas is correct", {
  tt <- data.frame(.long = double(),
                   .lat = double(),
                   .id = character(),
                   hemi = character(),
                   side = character())

  expect_error(as_ggseg_atlas(tt))

  tt <- data.frame(.long = double(),
                   .lat = double(),
                   .id = character(),
                   .subid = character(),
                   region = character(),
                   atlas = character(),
                   type = character(),
                   hemi = character(),
                   side = character())
  atlas <- as_ggseg_atlas(tt)
  expect_equal(names(atlas), c("atlas", "type", "hemi", "side", "region", "ggseg"))
  expect_equal(typeof(atlas$ggseg), "list")

  expect_equal(dim(as_ggseg_atlas(dk)), c(90, 8))
})

test_that("check that is_ggseg_atlas works", {
  dk2 <- as_ggseg_atlas(dk)
  expect_true(is_ggseg_atlas(dk2))

  dt <- data.frame(.long = double(),
                   .lat = double(),
                   .id = character(),
                   area = as.character(),
                   hemi = character(),
                   side = character()
  )

  expect_false(is_ggseg_atlas(dt))

})

