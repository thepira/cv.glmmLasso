

# modified from calc.deviance in dismo package - credit to Robert Hijmans

#' 
#' @title calc_mse
#' @description Function for calculating mse
#' @details Loss functions written for use in cv.glmmLasso 
#' @author Pirapong Jitngamplang, Jared Lander
#' @param actual actual data values 
#' @param predicted predicted data values
#' @return error between actual versus prediction
#'

calc_mse <- function(actual, predicted)
{
    return(colMeans((actual - predicted)^2)) 
}

#' 
#' @title calc_logloss
#' @description Functions for calculating logloss
#' @details Loss functions written for use in cv.glmmLasso 
#' @author Pirapong Jitngamplang, Jared Lander
#' @param actual actual data values 
#' @param predicted predicted data values
#' @return error between actual versus prediction
#'
calc_logloss <- function(actual, predicted)
{
    
    score <- -(actual * log(predicted) + (1 - actual) * log(1 -predicted))
    score[actual == predicted] <- 0
    score[is.nan(score)] <- Inf
    return(colMeans(score))
    
}

#' 
#' @title calc_multilogloss
#' @description Function for calculating multilogloss
#' @details loss functions written for use in cv.glmmLasso 
#' @author Pirapong Jitngamplang, Jared Lander
#' @param actual actual data values 
#' @param predicted predicted data values
#' @return error between actual versus prediction
#'

# modified from MultiLogLoss in MLMetrics package - credit to Yachen Yan

calc_multilogloss <- function(actual, predicted) 
{
    return(apply(predicted, 2, MLmetrics::MultiLogLoss, y_true = actual)) 
}


#' 
#' @title calc_deviance
#' @description Functions for calculating deviance
#' @details loss functions written for use in cv.glmmLasso 
#' @author Pirapong Jitngamplang, Jared Lander
#' @param actual actual data values 
#' @param predicted predicted data values
#' @param family default value is poisson
#' @param \dots can receive parameters accepted by dismo::calc.deviance
#' @return error between actual versus prediction
#'
calc_deviance <- function(actual, predicted, family = 'poisson',...)
{
    
    return(apply(predicted, 2, dismo::calc.deviance, obs = actual, family = family,
                 ...))
}




