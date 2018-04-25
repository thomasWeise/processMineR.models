library("processMineR.models")
context("processMineR.models.Models")

library(regressoR)

test_that("Test Models", {
  res <- new("Models",
            name="ab",
            features=list(a="b", c=45),
            models=list(
              RegressionResult.new(12),
              RegressionResult.new(13)
            ));
  expect_true(!is.null(res));
  expect_is(res, "Models");
  validObject(res);
})



test_that("Test Models.new", {
  res <- Models.new(
             name="ab",
             features=list(a="b", c=45, b=23),
             models=list(
               RegressionResult.new(12),
               RegressionResult.new(13)
             ));
  expect_true(!is.null(res));
  expect_is(res, "Models");
  validObject(res);
  expect_identical(res@features, list(a="b", b=23, c=45));
})
