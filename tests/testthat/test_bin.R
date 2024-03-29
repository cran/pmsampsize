context("binary_ss_checks")

test_that("Test 4.1", {
  ss <- c(623, 662, 221, 662)
  SPP <- c(4.52, 4.8, 1.6, 4.8)
  test <- pmsampsize(type="b", csrsquared= 0.288, parameters=24, prevalence = 0.174)
  test_ss <- test$results_table[,1]
  test_SPP <- test$results_table[,7]
  names(test_ss) <- names(test_SPP) <- c()
  expect_equal(test_ss,ss)
  expect_equal(test_SPP,SPP)
})

test_that("Test 4.2", {
  ss <- c(255, 532, 221, 532)
  SPP <- c(1.85, 3.86, 1.6, 3.86)
  test <- pmsampsize(type="b", csrsquared= 0.55, parameters=24, prevalence = 0.174)
  test_ss <- test$results_table[,1]
  test_SPP <- test$results_table[,7]
  names(test_ss) <- names(test_SPP) <- c()
  expect_equal(test_ss,ss)
  expect_equal(test_SPP,SPP)
})

test_that("Test 4.3", {
  ss <- c(934, 992, 221, 992)
  SPP <- c(4.51, 4.79, 1.07, 4.79)
  test <- pmsampsize(type="b", csrsquared= 0.288, parameters=36, prevalence = 0.174)
  test_ss <- test$results_table[,1]
  test_SPP <- test$results_table[,7]
  names(test_ss) <- names(test_SPP) <- c()
  expect_equal(test_ss,ss)
  expect_equal(test_SPP,SPP)
})

test_that("Test 4.4", {
  ss <- c(623, 590, 289, 623)
  SPP <- c(6.49, 6.15, 3.01, 6.49)
  test <- pmsampsize(type="b", csrsquared= 0.288, parameters=24, prevalence = 0.25)
  test_ss <- test$results_table[,1]
  test_SPP <- test$results_table[,7]
  names(test_ss) <- names(test_SPP) <- c()
  expect_equal(test_ss,ss)
  expect_equal(test_SPP,SPP)
})


