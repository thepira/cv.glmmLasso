library(dplyr)
data("soccer", package = "glmmLasso")
soccer[,c(4,5,9:16)]<-scale(soccer[,c(4,5,9:16)],center=TRUE,scale=TRUE)
soccer<-data.frame(soccer)
data <- soccer

tib <- dplyr::tibble(row = seq(nrow(soccer)),
              group = sample(rep(seq(5), length.out = nrow(soccer))))

testIndices <- tib %>% filter(.data$group == 1) %>% pull(row)
trainIndices <- tib$row[-testIndices]

fix <- points ~ transfer.spendings + ave.unfair.score + ball.possession + tackles 
+ ave.attend + sold.out

rnd <- list(team= ~ 1)

response_var <- fix[[2]] %>% as.character()


actualDataVector <- data %>% dplyr::slice(testIndices) %>% dplyr::pull(response_var) 

actualDataVector %>% is

devtools::load_all()

modList1 <- glmmLasso_MultLambdas(fix = fix, 
                      rnd = rnd,
                      data = data %>% dplyr::slice(trainIndices))

modList1[95]
modList1[1]


mod1 <- glmmLasso::glmmLasso(fix = fix,
                    rnd = rnd,
                    data = data %>% slice(trainIndices),
                    family = gaussian(link = "identity"),
                    lambda = exp(8.1))

# comparing models from glmmLasso_MultLambdas and manual fitting 

theLambds <- buildLambdas(fix = fix, rnd = rnd, data = data %>% slice(trainIndices))

mod1 <- glmmLasso::glmmLasso(fix = fix,
                    rnd = rnd,
                    data = data %>% slice(trainIndices),
                    family = gaussian(link = "identity"),
                    lambda = theLambds[1])

mod100 <- glmmLasso::glmmLasso(fix = fix,
                    rnd = rnd,
                    data = data %>% slice(trainIndices),
                    family = gaussian(link = "identity"),
                    lambda = theLambds[100])



computeLambdaMax(fix = fix,
                 rnd = rnd,
                 data = data %>% slice(trainIndices))

x[,1] %*% y
sum(y)

(colSums(x*y) %>% abs() %>% max() ) / nrow(data)
crossprod(x,y) %>% abs() %>% max() 

length(y)
dim(x)

apply(x, 2, function(x) {}, x = )

max( abs(t(y - mean(y)*(1-mean(x))) %*% x ) )/ (1 * nrow(data %>% slice(trainIndices)))


predictionMatrix <- predict.glmmLasso_MultLambdas(
    modList_foldk[[k]],
    newdata = data %>% dplyr::slice(testIndices))
    
    
mod1$Deltamatrix[mod1$conv.step,]
mod1$conv.step


y <- useful:build.y(fix, data %>% slice(trainIndices))
x <- useful::build.x(fix, data %>% slice(trainIndices))
z <- useful::build.x(fix, data %>% slice(trainIndices),contrasts = FALSE, 
                     sparse = TRUE)

glm1 <- glmnet::glmnet(x,y, family = "gaussian")
glm1$lambda

computeLambdaMax(fix = fix,
                 rnd = rnd,
                 data = data %>% slice(trainIndices))
buildLambdas(fix = fix, rnd = rnd, data = data %>% slice(trainIndices))

predictionMatrix <- predict.glmmLasso_MultLambdas(modList1,
    newdata = data %>% dplyr::slice(testIndices))

calc_mse(actual = data %>% dplyr::slice(testIndices) %>% pull(points), predicted = predictionMatrix)
modList1[92]

devtools::load_all()

cv.glmmLasso(fix = fix, 
             rnd = rnd,
             data = data)

for(l in seq_along(list(1,2,3,4,5)))
{
    l
}