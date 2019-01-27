context("test-get_paletteer")

test_that("Check that get_paletteer is working", {
  expect_is(get_paletteer("oslo"),c("character"))

  expect_equal(length(get_paletteer("oslo")),5)
  expect_equal(length(get_paletteer("PonyoMedium")),7)
  expect_equal(length(get_paletteer("seadra")),10)
  expect_equal(length(get_paletteer("qualitative")),12)
  expect_error(get_paletteer("mononoke"))
})


