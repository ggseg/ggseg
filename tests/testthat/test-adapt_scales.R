<<<<<<< HEAD
=======
context("test-adapt_scales")

>>>>>>> 9330d878f2fd8bca2b91eab6cb80021f5e3d370a
test_that("Check that scales are working", {
  expect_equal(class(adapt_scales(unnest(dk, ggseg))), "list")
  expect_equal(class(adapt_scales(unnest(aseg, ggseg))), "list")
})
