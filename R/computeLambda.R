# this function compute the max lambda based on formula given




computeLambdaMax <- function(fix, rnd, data)
{
    # converting formula into matrices to do lambdaMax calculation
    y <- useful::build.y(fix, data)
    x <- useful::build.x(fix, data)
    
    # N*alpha*lambdaMax = max_l(<x_l, y>)
    abs_dotprod <- abs(crossprod(x, y))
    lambdaMax <- max(abs_dotprod) / nrow(data)
    return(lambdaMax)
}

buildLambdas <- function(lambdaMax, 
                         nlambda = 100, 
                         lambda.min.ratio=ifelse(nobs < nvars, 0.01, 0.0001))
{
    lambda_vec <- seq(from = lambdaMax, 
                      to = lambdaMax * lambda.min.ratio, 
                      length.out = nlambda) 
    # sorting such that first lambda is the largest
    lambda_vec <- sort(lambda_vec, decreasing = TRUE)
    
    return(lambda_vec)
}
