context("test-adapt_scales")

test_that("Check that scales are working", {
  expect_equal(class(adapt_scales(unnest(dk, ggseg))), "list")
  expect_equal(class(adapt_scales(unnest(aseg, ggseg))), "list")
})
