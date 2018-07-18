library("processMineR.models")
context("Models.batchLoad")

library(regressoR)

slow.tests <- is.na(Sys.getenv("TRAVIS", unset=NA))

.check <- function(result) {
  expect_true(!(is.null(result)));
  expect_is(result, "RegressionResult");
  expect_true(is.function(result@result@f));
  expect_true(is.numeric(result@result@quality));
  expect_true(is.integer(result@result@size));
  expect_true(is.numeric(result@time));
  expect_true(is.character(result@name));
  if(!(is.null(result@metric))) {
    expect_equal(result@metric@quality(result@result@f), result@result@quality);
  }
}

.file.make <- function(dir, name, x, y) {
  con <- file(file.path(dir, name), open="wt");
  text <- unname(unlist(lapply(X=1:length(x), FUN=function(i) paste(c(x[i], y[i]), sep="", collapse="\t"))));
  writeLines(text=text, con=con);
  close(con);
}

.file.make.2 <- function(dir, name, f) {
  x <- sort(unique(round(c(runif(n=as.integer(runif(n=1, min=1, max=4)), min=1, max=6),
                           runif(n=as.integer(runif(n=1, min=2, max=4)), min=1, max=50)), digits=2)));
  .file.make(dir, name, x, round(f(x),digits=4));
}


.make.data <- function() {
  dir <- tempfile();
  dir.create(dir, showWarnings=FALSE, recursive=TRUE);

  results <- file.path(dir, "results");
  dir.create(results, showWarnings=FALSE, recursive=TRUE);

  dir.a <- file.path(results, "a");
  dir.create(dir.a, showWarnings=FALSE, recursive=TRUE);
  f <- function(x) 1+exp(2-x/4)
  .file.make.2(dir.a, "1.txt", f);
  .file.make.2(dir.a, "2.txt", f);
  .file.make.2(dir.a, "3.txt", f);
  .file.make.2(dir.a, "4.txt", f);

  f <- function(x) 1+exp(1.8-x/9)+0.4*sin(x)
  dir.b <- file.path(results, "b");
  dir.create(dir.b, showWarnings=FALSE, recursive=TRUE);
  .file.make.2(dir.b, "1.txt", f);
  .file.make.2(dir.b, "2.txt", f);
  .file.make.2(dir.b, "3.txt", f);
  .file.make.2(dir.b, "4.txt", f);

  f <- function(x) 1+exp(2.18-x/3.3)+0.4*cos(x)
  dir.c <- file.path(results, "c");
  dir.create(dir.c, showWarnings=FALSE, recursive=TRUE);
  .file.make.2(dir.c, "1.txt", f);
  .file.make.2(dir.c, "2.txt", f);
  .file.make.2(dir.c, "3.txt", f);

  return(c(dir, results));
}


.test <- function(cores) {
  data <- .make.data();
  output <- regressoR.batchLearn(source=data[2],
                                 q=0, learn.single = TRUE, learn.all = FALSE,
                                 returnResults = TRUE,
                                 cores=cores,
                                 logging = FALSE);
  expect_length(output, 11L);
  for(res in output) {
    .check(res);
  }

  results <- Models.batchLoad(path=file.path(data[1], "models"),
                                             cores=cores,
                                             logging=FALSE,
                                             featuresLoader=NULL);

  expect_length(results, 3L);

  expect_identical(results[[1]]@name, "a");
  expect_length(results[[1]]@data, 4L);
  expect_equal(results[[1]]@data[[1]], output[[1]]);
  expect_equal(results[[1]]@data[[2]], output[[2]]);
  expect_equal(results[[1]]@data[[3]], output[[3]]);
  expect_equal(results[[1]]@data[[4]], output[[4]]);

  expect_identical(results[[2]]@name, "b");
  expect_length(results[[2]]@data, 4L);
  expect_equal(results[[2]]@data[[1]], output[[5]]);
  expect_equal(results[[2]]@data[[2]], output[[6]]);
  expect_equal(results[[2]]@data[[3]], output[[7]]);
  expect_equal(results[[2]]@data[[4]], output[[8]]);

  expect_identical(results[[3]]@name, "c");
  expect_length(results[[3]]@data, 3L);
  expect_equal(results[[3]]@data[[1]], output[[9]]);
  expect_equal(results[[3]]@data[[2]], output[[10]]);
  expect_equal(results[[3]]@data[[3]], output[[11]]);

  unlink(data[1], recursive = TRUE);
}


test_that("Test Models.batchLoad I", {
  .test(cores=1L);
})

test_that("Test Models.batchLoad II", {
  .test(cores=2L);
})

test_that("Test Models.batchLoad III", {
  if(slow.tests) { .test(cores=3L); }
})

test_that("Test Models.batchLoad IIII", {
  if(slow.tests) { .test(cores=4L); }
})


