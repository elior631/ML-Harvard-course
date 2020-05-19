library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
set.seed(1995)
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]
y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

#distance between two numbers
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

#compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

#compute distance between each row
d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]

#visualize these distances
image(as.matrix(d))

#order the distance by labels
image(as.matrix(d)[order(y), order(y)])

#compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d))
d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))

##
library(caret)
data("tissue_gene_expression")
#merge into matrix
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
xy <- cbind(y, x)

#create partition
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)
test_set <- xy[test_index, ]
train_set <-xy[-test_index, ]

#set variables 
ks <- seq(1, 11, 2)
x_train <- train_set[ , 2:501]
y_train <- as.factor(train_set[ , 1])
x_test <- test_set[ , 2:501]
y_test <- as.factor(test_set[ , 1])

func <- sapply(ks, function(v){
  fit <- knn3(x_train, y_train, k=v)
  y_hat <- predict(fit, x_test, type = 'class')
  confusionMatrix(data = y_hat, reference = y_test)$overall['Accuracy']
})

better solution
{### or
set.seed(1)
library(caret)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})}

###