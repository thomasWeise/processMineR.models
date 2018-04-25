library("processMineR.models")
context("Models.joinNames")

library(regressoR)

test_that("Test Models.joinNames", {
  expect_identical(Models.joinNames("a"), "a");
  expect_identical(Models.joinNames("a/n"), "a/n");
  expect_identical(Models.joinNames(c("a", "a")), "a");
  expect_identical(Models.joinNames(c("a/n", "a/n")), "a/n");
  expect_identical(Models.joinNames(c("a/b", "a/n")), "a");
  expect_identical(Models.joinNames(c("1/a", "2/a")), "a");
  expect_identical(Models.joinNames(c("1/a/b", "2/a/b")), "a/b");
  expect_identical(Models.joinNames(c("1/a/6/c/h/h/7", "2/a/8/c/e/h/9")), "a/c/h");
})
