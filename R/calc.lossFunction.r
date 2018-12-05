
# modified from calc.deviance in dismo package - credit to Robert Hijmans

calc_mse <- function(actual, predicted)
    
{
    return(colMeans((actual - predicted)^2)) 
}


calc_logloss <- function(actual, predicted)
{
    
    score <- -(actual * log(predicted) + (1 - actual) * log(1 -predicted))
    score[actual == predicted] <- 0
    score[is.nan(score)] <- Inf
    return(colMeans(score))
    
}


# modified from MultiLogLoss in MLMetrics package - credit to Yachen Yan

calc_multilogloss <- function(actual, predicted) 
{
    return(apply(predicted, 2, MLmetrics::MultiLogLoss, y_true = actual)) 
}

calc_deviance <- function(actual, predicted, family = 'poisson',...)
{
    # return(apply(predicted, 2, function(x, ... = ...) {
    #     dismo::calc.deviance(obs = actual, pred = x, ...)
    # }) 
    return(apply(predicted, 2, dismo::calc.deviance, obs = actual, family = family,
          ...))
}




