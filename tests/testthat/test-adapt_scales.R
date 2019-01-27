context("test-adapt_scales")

test_that("Check that scales are working", {
  expect_equal(class(adapt_scales(dkt)), "list")
  expect_equal(class(adapt_scales(aseg)), "list")

  expect_warning(adapt_scales(dkt %>% select(-atlas)))
})
