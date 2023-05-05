

test_that("as_brain-atlas", {

  ks <- as.data.frame(dk)
  ka <- as_brain_atlas(ks)
  expect_equal(class(ka),
               "brain_atlas")
  expect_true(is_brain_atlas(ka))

  ka <- as_brain_atlas(ks)
  expect_equal(class(ka),
               "brain_atlas")
  expect_true(is_brain_atlas(ka))

  expect_equal(class(as_brain_atlas(dk)),
               "brain_atlas")

  ka <- as_ggseg_atlas(dk)
  expect_s3_class(ka, "ggseg_atlas")
  ks <- as_brain_atlas(ka)
  expect_s3_class(ks, "brain_atlas")
})

test_that("brain-atlas changes", {

  expect_equal(class(as.data.frame(dk)),
               "data.frame")

  expect_true(inherits(as_ggseg_atlas(dk),
                       "ggseg_atlas"))

})

