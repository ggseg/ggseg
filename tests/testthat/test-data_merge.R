context("test-data_merge")


someData <- data.frame(
  region = c("transverse temporal", "insula",
           "precentral","superior parietal",
           "transverse temporal", "insula",
           "precentral","superior parietal"),
  p = sample(seq(0,.5,.001), 8),
  Group = c(rep("G1",4), rep("G2",4)),
  stringsAsFactors = FALSE) %>%
  group_by(Group)

test_that("Check that merging with grouped data works", {

    testData <- data_merge(someData, unnest(dk, cols = ggseg))

    expect_equal(names(testData)[1], "Group")
    expect_equal(unique(testData$Group), c("G1", "G2"))
})

test_that("Check that plotting with grouped data works", {

  pp <- ggseg(.data=someData, mapping=aes(fill=p)) +
    facet_wrap(~Group)

  expect_is(pp, c("gg","ggplot"))
  expect_true("Group" %in% names(pp$data))
})

test_that("Check that simple data merge works", {
  someData <- someData %>%
    #tidyr::unnest() %>%
    dplyr::filter(Group == "G1")

  testData <- data_merge(someData, unnest(dk, ggseg))

  expect_equal(names(testData)[1], "Group")
  expect_equal(unique(testData$Group), "G1")
})
