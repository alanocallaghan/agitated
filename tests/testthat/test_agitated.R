context("agitated")
test_that("empty test", {
  data <- agitated:::example_data()
  expect_is(agitated(data), "gg")
  expect_is(agitated(data, nsets = 10), "gg")
  expect_is(agitated(data, exclusive = FALSE), "gg")
  expect_is(agitated(data, intersection_order = "degree"), "gg")
  expect_is(agitated(data, sort_sets = FALSE), "gg")
  expect_is(agitated(data, title = "title", subtitle = "subtitle"), "gg")

})
