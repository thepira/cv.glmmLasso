actual <- rep(c(0, 1), length.out=3)

temp <- runif(n = 6, min = 0, max = 1)

predicted <- matrix(data = temp, nrow =3, ncol = 2)

devtools::load_all()

Metrics::mse(trueVal, predicted)

colMeans((actual - predicted)^2) 

temp <- (actual - predicted)^2

temp[,2] %>% mean()

cv.glmmLasso::calc_logloss(actual, predicted)
Metrics::logLoss(actual, predicted[,2])

cv.glmmLasso::calc_multilogloss(actual, predicted)
MLmetrics::MultiLogLoss(actual, predicted[,1])


svm.model <- e1071::svm(Species~., data = iris, probability = TRUE)
pred <- predict(svm.model, iris, probability = TRUE)
MultiLogLoss(y_true = iris$Species, y_pred = attr(pred, "probabilities"))


y_true <- model.matrix(~0 + ., as.character(iris$Species) %>% as.data.frame())

svm.model <- e1071::svm(Species~., data = iris, probability = TRUE)
y_pred <- predict(svm.model, iris, probability = TRUE)

y_pred <- attr(y_pred, "probabilities")

(log(y_pred) * y_true) %>%  sum()

apply(predicted, 2, mean)


dismo::calc.deviance()

temp <- rpois(6,5)
predicted <- matrix(temp, nrow = 3, ncol = 2)
actual <- rpois(3,5)

devtools::load_all()
calc_deviance(actual, predicted, family = 'poisson')


dismo::calc.deviance(obs = actual, pred = predicted[,2], family = 'poisson')
