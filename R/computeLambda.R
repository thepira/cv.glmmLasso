# this function compute the max lambda based on formula given

#' @title computeLambdaMax
#' @description compute the maximum lambda value based on given dataset 
#' @details lambdaMax is computed based on  the coordinate descent algorithm from this paper: Friedman, Jerome, Trevor Hastie, and Rob Tibshirani. "Regularization paths for generalized linear models via coordinate descent."
#' @author Pirapong Jitngamplang, Jared Lander
#' @param fix A two-sided linear formula object describing the fixed-effects part of the model, with the response on the left of a ~ operator and the terms, separated by + operators, on the right. For categorical covariables use as.factor(.) in the formula. Note, that the corresponding dummies are treated as a group and are updated blockwise
#' @param rnd A two-sided linear formula object describing the random-effects part of the model, with the grouping factor on the left of a ~ operator and the random terms, separated by + operators, on the right; aternatively, the random effects design matrix can be given directly (with suitable column names). If set to NULL, no random effects are included.
#' @param data The data frame containing the variables named in formula.
#' @param scale default value is true
#' @return returns the lambdaMax value based on given dataset


computeLambdaMax <- function(fix, rnd, data, scale=TRUE)
{
    # converting formula into matrices to do lambdaMax calculation
    y <- useful::build.y(fix, data)
    x <- useful::build.x(fix, data)
    
    if(scale)
    {
        x <- scale(x)
    }
    
    # exp because of log scale
    # N*alpha*lambdaMax = max_l(<x_l, y>)
    lambdaMax <- exp(max(abs(colSums(x*y)), na.rm=TRUE) / nrow(data))
    
    # colSums(x*y) is same as crossprod(x,y)
    
    return(lambdaMax)
}

#' @title buildLambdas
#' @description generate lambda vector based on dataset given
#' @author Pirapong Jitngamplang, Jared Lander
#' @param fix A two-sided linear formula object describing the fixed-effects part of the model, with the response on the left of a ~ operator and the terms, separated by + operators, on the right. For categorical covariables use as.factor(.) in the formula. Note, that the corresponding dummies are treated as a group and are updated blockwise
#' @param rnd A two-sided linear formula object describing the random-effects part of the model, with the grouping factor on the left of a ~ operator and the random terms, separated by + operators, on the right; alternatively, the random effects design matrix can be given directly (with suitable column names). If set to NULL, no random effects are included.
#' @param data The data frame containing the variables named in formula.
#' @param nlambdas the number of lambdas values, default value is 100 if lambdas is not user-supplied
#' @param lambda.min.ratio Smallest value for lambda, as a fraction of lambda.max, the (data derived) entry value (i.e. the smallest value for which all coefficients are zero). The default depends on the sample size nobs relative to the number of variables nvars. If nobs > nvars, the default is 0.0001, close to zero. If nobs < nvars, the default is 0.01.
#' @return returns a vector of lambda
#'


buildLambdas <- function(fix, rnd, data, 
                         nlambdas = 100, 
                         lambda.min.ratio = ifelse(nobs < nvars, 0.01, 0.0001))
{
    # converting formula into matrices to do lambdaMax calculation
    x <- useful::build.x(fix, data)
    nobs <- nrow(x)
    nvars <- ncol(x)
    
    lambdaMax = computeLambdaMax(fix = fix, 
                                 rnd = rnd,
                                 data = data)
    
    lambda_vec <- seq(from = lambdaMax, 
                      to = lambdaMax * lambda.min.ratio, 
                      length.out = nlambdas) 
    # sorting such that first lambda is the largest
    lambda_vec <- sort(lambda_vec, decreasing = TRUE)
    
    return(lambda_vec)
}

