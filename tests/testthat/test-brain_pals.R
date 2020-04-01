
test_that("check new palettes work", {
  expect_equal(length(brain_pal("yeo7", package = "ggsegExtra")), 7)
  expect_equal(length(brain_pal("yeo17", package = "ggsegExtra")), 17)
  expect_equal(length(brain_pal("tracula", package = "ggsegExtra")), 11)
  expect_equal(length(brain_pal("jhu", package = "ggsegExtra")), 12)
  expect_equal(length(brain_pal("glasser", package = "ggsegExtra")), 180)
})
