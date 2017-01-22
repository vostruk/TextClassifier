#' Function fit.knn implements model building algorithm of k nearest neighbours
#'
#' This fit function just remembers the train data in the model
#'
#' @param data data.frame contains values of our training set (bag-of-words format of text)
#' @param fact  factor containing proper classification of the training samples in data set (one per observation)
#' @param k  number of the nearest neighbours taken into acount
#' @param metric function implementing the metric of the similarity
#'
#' @return object of the class ptmKnn to use in predict function
#'
#' @examples
#' fit.knn(train.data, label, 5, metric.cos)
#'
#' @export
fit.knn <- function(data, fact, k=3, metric=metric.cos) {

  vectors <- data.compact(data)
  model <- list(vectors=vectors, fact=fact, k=k, metric=metric)

  class(model) <- 'ptmKnn'
  return (model)
}

#' Function predict.ptmKnn extends generic function predict for Naive Bayes bernoulli model
#'
#'
#' @param model list should contain $vectors $fact, $k, $metric
#' @param data data.frame contains values of our testing set (identical structure that was use in training set for building model)
#'
#' @return result vector where for each sample from data set assigned category is returned
#'
#' @examples
#' predict(model, data)
#'
#' @export
predict.ptmKnn <- function(model, data) {

  result <- c()
  cdata <- data.compact(data)

  apply.metric <- function (v1,v2,metric){
    mock.v2 <- data.frame (id=v2$id, count=v2$count*0)
    m.v1 <- match(v1$id, v2$id)
    c.v1 <- rbind (v1[is.na(m.v1),], v1[!is.na(m.v1),], mock.v2[-m.v1[!is.na(m.v1)],])
    mock.v1 <- data.frame (id=v1$id, count=v1$count*0)
    m.v2 <- match (v2$id, v1$id)
    c.v2 <- rbind (mock.v1[-m.v2[!is.na(m.v2)],], v2[is.na(m.v2),], v2[!is.na(m.v2),])

    return (metric (c.v1, c.v2))
  }

  # one by one of each sample in test set
  for (i in 1:nrow(data)){
    distances <- sapply (model$vectors, apply.metric, cdata[[i]], model$metric)
    order <- order(distances)
    voters <- table(model$fact[order[1:model$k]])
    voteing <- as.data.frame(voters)
    voteing_order <- rev(order(voteing$Freq))
    voteing_res <- voteing[voteing_order[1],1]
    result <- append(result, levels(model$fact)[voteing_res])
  }
	return (result)
}


#' Function knn.simple implements model building and prediction algorithm of k nearest neighbours in one function
#'
#'
#' @param data data.frame contains values of our training set (bag-of-words format of text)
#' @param fact  factor containing proper classification of the training samples in data set (one per observation)
#' @param test.data  contains values of our testing set for classification
#' @param k  number of the nearest neighbours taken into acount
#' @param metric function implementing the metric of the similarity
#'
#' @return vector of predicted labels for test.set
#'
#' @examples
#' knn.simple(train.data, label, test.data, 5, metric.cos)
#'
#' @export
knn.simple <- function(data, fact, test.data, metric, k=5) {
  data <- as.matrix(data);
  test.data <- as.matrix(test.data);

  N  <- nrow(test.data);
  NR <- nrow(data);

  lvl    <- levels(fact);
  CN	  <- length(lvl);
  result <- character(N);

  for(r in 1:N) {
    test.row <- test.data[r, ];

    dists <- rep(0, nrow(data));
    for(i in 1:NR) {
      dists[i] <- metric(test.row, data[i, ]);
    }

    ord   <- order(dists,decreasing=FALSE);
    nbh   <- ord[1:k]; # neighbours numbers
    nbh.cls <- fact[nbh];

    class.votes <- sapply(1:CN, function(r) sum(nbh.cls == lvl[r]) );
    result[r] <- lvl[ which(class.votes == max(class.votes))[1] ];
  }

  tmp <- factor(result, lvl);
  return(tmp);
}

