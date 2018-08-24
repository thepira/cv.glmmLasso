library(glmmLasso)
library(dplyr)

data("soccer", package = "glmmLasso")
soccer[,c(4,5,9:16)]<-scale(soccer[,c(4,5,9:16)],center=TRUE,scale=TRUE)
soccer<-data.frame(soccer)

tib <- tibble(row = seq(nrow(soccer)),
       group = sample(rep(seq(5), length.out = nrow(soccer))))


fix = points ~ transfer.spendings + ave.unfair.score 
+ ball.possession + tackles 
+ ave.attend + sold.out

testIndex <- tib %>% filter(.data$group == 1) %>% pull(row)
trainIndex <- tib$row[-testIndex]

lm1 <- glmmLasso(points ~ transfer.spendings + ave.unfair.score 
                 + ball.possession + tackles 
                 + ave.attend + sold.out, rnd = list(team=~1), 
                 lambda=10, data = soccer %>% slice(trainIndex))


lm2 <- glmmLasso(points ~ transfer.spendings + ave.unfair.score 
                 + ball.possession + tackles 
                 + ave.attend + sold.out, rnd = list(team=~1), 
                 lambda=10, data = soccer %>% slice(trainIndex))

modList_foldk <- list(lm1,lm2)

is(lm1)

predict(lm1, soccer %>% slice(testIndices)
        
predict_list <- map(lm_List, stats::predict )

predict_list %>% is()
predict_list[[1]] %>% is


numList <- list(numbers, numbers2)

numbers <- as.vector(numbers %>% pull(row))
numbers2 <- numbers

map(numList, mean, trim = 0.5)

list1 <- vector("list", 5)
list1 <- list()
list1[[1]] < c(1,2,3,4,5,)

response_var <- fix[[2]] %>% as.character()

lossvaluesList <- vector(mode = 'list', 2)

data <- soccer

# employing the loss function in form loss(actual,predicted)

# using loss function, calculating a list of loss values for each vector of prediction
# which comes from a glmmLasso model with a specific lambda 
actualData <- data %>% slice(testIndex) %>% pull(response_var)

# predicting values for each of the glmmLasso model (100 lambda) 
predictionList <- map(modList_foldk, stats::predict)

# storing loss values for each fold
lossvaluesList[[1]] <- actualData %>% map(Metrics::mse, predicted = predictionList)


map(actualData, Metrics::mse, predicted = predictionList)


    