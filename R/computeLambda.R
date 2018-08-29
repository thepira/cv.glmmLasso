# this function compute the max lambda based on formula given


computeLambdaMax <- function(fix, rnd, data, scale=TRUE)
{
    # converting formula into matrices to do lambdaMax calculation
    y <- useful::build.y(fix, data)
    x <- useful::build.x(fix, data)
    
    if(scale)
    {
        x <- scale(x)
    }
    
    # N*alpha*lambdaMax = max_l(<x_l, y>)
    lambdaMax <- exp(max(abs(colSums(x*y)), na.rm=TRUE) / nrow(data))
    
    # colSums(x*y) is same as crossprod(x,y)
    
    return(lambdaMax)
}

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

