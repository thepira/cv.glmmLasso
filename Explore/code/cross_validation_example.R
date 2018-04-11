library(modelr)
iris %>% head
iris %>% dim

hold <- crossv_kfold(data=iris, k=5)
hold
hold$train[[1]]
hold$train[[1]]$data
hold$train[[1]]$idx

hold$train[[1]]$data %>% dplyr::slice(hold$train[[1]]$idx)
hold$test[[1]]$data %>% dplyr::slice(hold$test[[1]]$idx)

hold$train[[1]]$data %>% dplyr::slice(hold$train[[1]]$idx)
hold$test[[2]]$data %>% dplyr::slice(hold$test[[2]]$idx)

library(caret)
hold2 <- createFolds(iris, k=10)
hold2
hold3 <- caret::createFolds(y=iris$Species, k=5)

iris %>% dplyr::slice(hold3$Fold1)
iris %>% dplyr::slice(-hold3$Fold1)