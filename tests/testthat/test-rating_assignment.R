test_that("assignment works", {
  set.seed(010320)
  items <- c('a', 'b', 'c')
  raters <- c('r1', 'r2', 'r3')
  pair_size <- 2
  
  output <- rating_assignment(items, raters, pair_size)
  
  expect_equal(output$rater_record$r1, c('a', 'b'))
  expect_equal(output$rater_record$r2, c('b', 'c'))
  expect_equal(output$rater_record$r3, c('a', 'c'))
})

test_that("empty raters vector throws error", {
  items <- c('a', 'b', 'c')
  raters <- c()
  pair_size <- 2
  
  expect_error(rating_assignment(items, raters, pair_size), regexp = 'Error: \'raters\'.*vector')
})

test_that("single element raters vector throws error", {
  items <- c('a', 'b', 'c')
  raters <- c('r1')
  pair_size <- 2
  
  expect_error(rating_assignment(items, raters, pair_size), regexp = 'Error: \'raters\'.*elements')
})

test_that("empty items vector throws error", {
  items <- c()
  raters <- c('r1', 'r2', 'r3')
  pair_size <- 2
  
  expect_error(rating_assignment(items, raters, pair_size), regexp = 'Error: \'items\'.*vector')
})


test_that("pair size is valid", {
  items <- c('a', 'b', 'c')
  raters <- c('r1', 'r2', 'r3')
  pair_size <- 0
  
  expect_error(rating_assignment(items, raters, pair_size), regexp = 'Error: \'pair_size\'.*')

  pair_size <- 1
  
  expect_error(rating_assignment(items, raters, pair_size), regexp = 'Error: \'pair_size\'.*')

  pair_size <- 10
  
  expect_error(rating_assignment(items, raters, pair_size), regexp = 'Error: \'pair_size\'.*')
})