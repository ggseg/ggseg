context("test-adapt_scales")

test_that("Check that scales are working", {
  expect_equal(class(adapt_scales(unnest(dkt, ggseg))), "list")
  expect_equal(class(adapt_scales(unnest(aseg, ggseg))), "list")
})
