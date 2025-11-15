test_that("Check that palette extraction happens ok", {
  expect_is(class(brain_pals_info()), "character")
  expect_equal(length(brain_pal("dk", 13, unname = TRUE)), 13)
  expect_equal(length(brain_pal("dk", 13, direction = -1)), 13)
  expect_equal(length(brain_pal("aseg")), 45)

  expect_equal(length(brain_pal("aseg", n = 1:8)), 8)
  expect_equal(length(brain_pal("aseg", "all")), 45)

  expect_warning(length(brain_pal("aseg", 2)), "3")
  expect_equal(length(brain_pal("aseg", 22, unname = TRUE)), 22)
  expect_warning(brain_pal("dk", 50))
  expect_error(brain_pal("yeo"))
})
