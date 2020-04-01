test_that("Check that palette extraction happens ok", {
  expect_is(class(brain_pals_info()),"character")
  expect_equal(length(brain_pal("dk", 13, unname=TRUE)), 13)
  expect_equal(length(brain_pal("dk", 13, direction=-1)), 13)
  expect_equal(length(brain_pal("aseg")), 21)

  expect_equal(length(brain_pal("aseg", n=1:8)), 8)
  expect_equal(length(brain_pal("aseg", "all")), 21)

  expect_warning(length(brain_pal("aseg", 2)), 3)
  expect_warning(length(brain_pal("aseg",22, unname=TRUE)))
  expect_warning(brain_pal("dk",50))
  expect_error(brain_pal("yeo"))
})

test_that("Test that palette display works", {

  expect_is(display_brain_pal(),c("gg","ggplot"))
  expect_is(display_brain_pal(n=1:7),c("gg","ggplot"))
  expect_is(display_brain_pal(n=7),c("gg","ggplot"))
  expect_is(display_brain_pal(name="aseg", n=7),c("gg","ggplot"))
  expect_is(display_brain_pal(name="aseg", n="all"),c("gg","ggplot"))

  expect_warning(display_brain_pal(name = "dk",n=2))
  expect_warning(display_brain_pal(name = "dk",n=48))
  expect_error(display_brain_pal(name = "et", n=48))
  expect_error(display_brain_pal(name = "et"))
})


