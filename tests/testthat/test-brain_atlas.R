test_that("brain_atlas works", {
  k <- brain_atlas("test", "cortical", dk$data)
  expect_equal(class(k), "brain_atlas")

  expect_error(brain_atlas("test", "csortical", dk$data))
  expect_error(brain_atlas(c("test", "test"), "cortical", dk$data))

  pal <- dk$palette
  k <- brain_atlas("test", "cortical", dk$data, pal)
  expect_equal(class(k), "brain_atlas")
  expect_error(brain_atlas("test", "cortical", dk$data, pal[-1]))

  names(pal)[1] <- "bla"
  expect_error(brain_atlas("test", "cortical", dk$data, pal))
})


test_that("brain-atlas format", {
  k <- capture.output(dk)
  expect_equal(k[1], "# dk cortical brain atlas")
  expect_equal(k[2], "  regions: 35 ")
})

test_that("as_brain-atlas", {
  expect_error(as_brain_atlas(~age), "Cannot make object")

  expect_error(as_brain_atlas(data.frame()), "Cannot make object")

  ka <- data.frame(
    atlas = NA,
    type = "cortical",
    hemi = NA,
    region = NA,
    side = NA,
    label = NA
  )
  expect_error(as_brain_atlas(ka), "Object does not contain")

  ks <- as.data.frame(dk)
  ka <- as_brain_atlas(ks)
  expect_equal(class(ka), "brain_atlas")
  expect_true(is_brain_atlas(ka))

  ka <- as_brain_atlas(ks)
  expect_equal(class(ka), "brain_atlas")
  expect_true(is_brain_atlas(ka))

  ka <- as_ggseg_atlas(dk)
  ks <- as_brain_atlas(ka)
  expect_equal(class(ks), "brain_atlas")
  expect_true(is_brain_atlas(ks))

  expect_error(as_brain_atlas(list()), "Cannot make object")

  expect_equal(class(as_brain_atlas(dk)), "brain_atlas")

  ka <- as.list(dk)
  expect_equal(class(as_brain_atlas(ka)), "brain_atlas")

  expect_true(inherits(brain_data(dk$data), "brain_data"))

  expect_true(inherits(as_brain_data(dk$data), "brain_data"))
})

test_that("brain-atlas changes", {
  expect_equal(class(as.data.frame(dk)), "data.frame")
  expect_equal(class(as.list(dk)), "list")
  expect_true(inherits(as_ggseg_atlas(dk), "ggseg_atlas"))

  k <- dk
  k$type <- NA
  k <- as.list(k)
  expect_equal(class(k), "list")
  expect_true(inherits(as_brain_atlas(k), "brain_atlas"))
})
