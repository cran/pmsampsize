context("survival_ss_checks")

test_that("Test 3.1", {
  ss <- c(4286, 875, 4286, 4286)
  SPP <- c(23.07, 4.71, 23.07, 23.07)
  test <- pmsampsize(type="s",rsquared=0.051,parameters=25,rate=0.065,timepoint=2,meanfup=2.07)
  test_ss <- test$results_table[,1]
  test_SPP <- test$results_table[,6]
  names(test_ss) <- names(test_SPP) <- c()
  expect_equal(test_ss,ss)
  expect_equal(test_SPP,SPP)
})

test_that("Test 3.2", {
  ss <- c(3335, 868, 3335, 3335)
  SPP <- c(17.95, 4.67, 17.95, 17.95)
  test <- pmsampsize(type="s",rsquared=0.065,parameters=25,rate=0.065,timepoint=2,meanfup=2.07)
  test_ss <- test$results_table[,1]
  test_SPP <- test$results_table[,6]
  names(test_ss) <- names(test_SPP) <- c()
  expect_equal(test_ss,ss)
  expect_equal(test_SPP,SPP)
})

test_that("Test 3.3", {
  ss <- c(8572, 1749, 8572, 8572)
  SPP <- c(23.07, 4.71, 23.07, 23.07)
  test <- pmsampsize(type="s",rsquared=0.051,parameters=50,rate=0.065,timepoint=2,meanfup=2.07)
  test_ss <- test$results_table[,1]
  test_SPP <- test$results_table[,6]
  names(test_ss) <- names(test_SPP) <- c()
  expect_equal(test_ss,ss)
  expect_equal(test_SPP,SPP)
})

test_that("Test 3.4", {
  ss <- c(4286, 725, 4286, 4286)
  SPP <- c(35.49, 6, 35.49, 35.49)
  test <- pmsampsize(type="s",rsquared=0.051,parameters=25,rate=0.1,timepoint=2,meanfup=2.07)
  test_ss <- test$results_table[,1]
  test_SPP <- test$results_table[,6]
  names(test_ss) <- names(test_SPP) <- c()
  expect_equal(test_ss,ss)
  expect_equal(test_SPP,SPP)
})

test_that("Test 3.5", {
  ss <- c(4286, 875, 4286, 4286)
  SPP <- c(23.07, 4.71, 23.07, 23.07)
  test <- pmsampsize(type="s",rsquared=0.051,parameters=25,rate=0.065,timepoint=4,meanfup=2.07)
  test_ss <- test$results_table[,1]
  test_SPP <- test$results_table[,6]
  names(test_ss) <- names(test_SPP) <- c()
  expect_equal(test_ss,ss)
  expect_equal(test_SPP,SPP)
})

test_that("Test 3.6", {
  ss <- c(4286, 684, 4286, 4286)
  SPP <- c(44.57, 7.11, 44.57, 44.57)
  test <- pmsampsize(type="s",rsquared=0.051,parameters=25,rate=0.065,timepoint=2,meanfup=4)
  test_ss <- test$results_table[,1]
  test_SPP <- test$results_table[,6]
  names(test_ss) <- names(test_SPP) <- c()
  expect_equal(test_ss,ss)
  expect_equal(test_SPP,SPP)
})

test_that("Test 3.7", {
  ss <- c(5798, 974, 5798, 5798)
  SPP <- c(57.98, 9.74, 57.98, 57.98)
  test <- pmsampsize(type="s",rsquared=0.06,parameters=40,rate=0.08,timepoint=3,meanfup=5)
  test_ss <- test$results_table[,1]
  test_SPP <- test$results_table[,6]
  names(test_ss) <- names(test_SPP) <- c()
  expect_equal(test_ss,ss)
  expect_equal(test_SPP,SPP)
})

test_that("Test 3.8", {
  ss <- c(2950, 273, 2950, 2950)
  SPP <- c(79.65, 7.37, 79.65, 79.65)
  test <- pmsampsize(type="s",rsquared=0.03,parameters=10,rate=0.09,timepoint=3,meanfup=3)
  test_ss <- test$results_table[,1]
  test_SPP <- test$results_table[,6]
  names(test_ss) <- names(test_SPP) <- c()
  expect_equal(test_ss,ss)
  expect_equal(test_SPP,SPP)
})

test_that("Test 3.9", {
  ss <- c(1974, 2279, 2279, 2279)
  SPP <- c(3.7, 4.27, 4.27, 4.27)
  test <- pmsampsize(type="s",rsquared=0.3,parameters=80,rate=0.03,timepoint=1,meanfup=5)
  test_ss <- test$results_table[,1]
  test_SPP <- test$results_table[,6]
  names(test_ss) <- names(test_SPP) <- c()
  expect_equal(test_ss,ss)
  expect_equal(test_SPP,SPP)
})


