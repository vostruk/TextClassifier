#here is testing of knn
#
context("knn testing")
library(class)

data("iris")
df = data.frame(iris)
colnames(df)[1:4] = paste("w.", colnames(df)[1:4], sep='')
colnames(df)[5]="c.Species"
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
iris.training <- iris[ind==1, 1:4]
iris.test <- iris[ind==2, 1:4]
iris.trainLabels <- iris[ind==1, 5]
iris.testLabels <- iris[ind==2, 5]

test_that("knn from standard package class", {
  iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
  #print("standart knn-class:")
  acc <- compute.metrixes(table(iris_pred, iris.testLabels))[3]
  #print(acc)
  expect_true(acc>0.85)
})

test_that("knn simple function test", {

  res = knn.simple(iris.training, iris.trainLabels, iris.test, metric.euclid, k=5)
  restab = table(res, iris.testLabels)
  #print("knn-class like simple implementation:")
  acc <- compute.metrixes(restab)[3]
  #print(acc)
  expect_true(acc>0.85)
})

test_that("knn fit predict function test", {

  #this function last much longer than simple one
  res = fit.knn(iris.training, iris.trainLabels, k=5, metric.euclid)
  restab = table(predict(res, iris.test), iris.testLabels)
  #print("fit-predict knn implementation:")
  acc <- compute.metrixes(restab)[3]
  #print(acc)
  expect_true(acc>0.85)
})
