test_that("position_formula works", {
  expect_error(
    position_formula(hemi ~ hemi, as.data.frame(dk)),
    "Cannot position brain"
  )

  expect_error(
    position_formula(bla ~ ., as.data.frame(dk)),
    "position formula not correct"
  )

  k <- position_formula(hemi ~ side, as.data.frame(dk))
  expect_equal(names(k), c("position", "chosen"))
  expect_equal(k$position, c("hemi", "side"))
  expect_equal(k$chosen, c("hemi", "side"))

  k <- position_formula(side ~ hemi, as.data.frame(dk))
  expect_equal(names(k), c("position", "chosen"))
  expect_equal(k$position, c("side", "hemi"))
  expect_equal(k$chosen, c("side", "hemi"))

  k <- position_formula(. ~ hemi + side, as.data.frame(dk))
  expect_equal(names(k), c("position", "chosen"))
  expect_equal(k$position, c("columns"))
  expect_equal(k$chosen, c("hemi", "side"))
})
