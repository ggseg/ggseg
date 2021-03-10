test_that("position_formula works", {

  expect_error(position_formula(hemi ~ age),
               "argument \"type\" is missing")

  expect_error(position_formula(hemi ~ hemi),
               "Cannot position brain")

  expect_error(position_formula(hemi ~ side, "subcortical"),
               "how to position subcortical data")

  expect_error(position_formula(bla ~ ., "cortical"),
               "position formula not correct")

  k <- position_formula(hemi ~ side, "cortical")
  expect_equal(names(k), c("position", "chosen"))
  expect_equal(k$position, c("hemi", "side"))
  expect_equal(k$chosen, c("hemi", "side"))

  k <- position_formula(side ~ hemi, "cortical")
  expect_equal(names(k), c("position", "chosen"))
  expect_equal(k$position, c("side", "hemi"))
  expect_equal(k$chosen, c("side", "hemi"))

  k <- position_formula(. ~ hemi + side, "cortical")
  expect_equal(names(k), c("position", "chosen"))
  expect_equal(k$position, c("columns"))
  expect_equal(k$chosen, c("hemi", "side"))

})
