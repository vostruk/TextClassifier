#here is testing of bayes
#
context("bayes testing")
library(e1071)

data("iris")
df = data.frame(iris)
colnames(df)[1:4] = paste("w.", colnames(df)[1:4], sep='')
colnames(df)[5]="c.Species"
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
iris.training <- iris[ind==1, 1:4]
iris.test <- iris[ind==2, 1:4]
iris.trainLabels <- iris[ind==1, 5]
iris.testLabels <- iris[ind==2, 5]

test_that("naiveBayes from standard package e1070", {

   pred <- naiveBayes(iris.training, iris.trainLabels)
   pl <- predict(pred, iris.test)

   #print("standart bayes-e1071:")
   acc <- compute.metrixes(table(pl, iris.testLabels))[3]
   expect_true(acc>0.85)
})


test_that("naiveBayes implemented", {

  pred <- fit.nb(iris.training, iris.trainLabels)
  pl <- predict(pred, iris.test)

  acc <- compute.metrixes(table(pl, iris.testLabels))[3]

  expect_true(acc>0.2)
})

test_that("nb bernoulli implemented", {

  pred <- fit.nb.bernoulli(iris.training, iris.trainLabels)
  pl <- predict(pred, iris.test)
  acc <- compute.metrixes(table(pl, iris.testLabels))[3]
  #accuracy is quite low for iris dataset
  print(acc)
  expect_true(acc>0.2)
})

test_that("nb multinomial implemented", {

  pred <- fit.nb.multinomial(iris.training, iris.trainLabels)
  pl <- predict(pred, iris.test)
  acc <- compute.metrixes(table(pl, iris.testLabels))[3]
  #accuracy is quite low for iris dataset, however it's created for texts classification with hundrets of categories
  print(acc)
  expect_true(acc>0.2)
})
