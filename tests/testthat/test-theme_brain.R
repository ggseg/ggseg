
test_that("Check that themes are working", {
  expect_is(ggseg() + theme_brain(),c("gg","ggplot"))
  expect_is(ggseg() + theme_darkbrain(),c("gg","ggplot"))
  expect_is(ggseg() + theme_custombrain(),c("gg","ggplot"))
})

