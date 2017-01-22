test_that("testing the euclid metric", {

  v1 = c(1,2,3,4);
  v2 = c(4,3,2,1);
  d = metric.euclid(v1,v2);
  expect_true(abs(d - 4.472136) <0.1)
})

test_that("testing the cos metric", {

  x = c(1,2,3,4);
  y = c(1,2,3,5);
  d = metric.cos(x,y)
  d2 = x %*% y / sqrt(x%*%x * y%*%y)
  expect_true(abs(1-d - 0.006) < 0.1)
  expect_true(abs(1-d-0.006) < 0.1)
  expect_true(abs(d - d2) < 0.1)
})
