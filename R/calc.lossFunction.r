# modified from calc.deviance in dismo package - credit to Robert Hijmans

calc.deviance <- function (actual, predicted, weights = rep(1, length(obs)), calc.mean = TRUE) 
{
    obs <- actual
    pred <- predicted
    
    if (length(obs) != length(pred)) {
        stop("observations and predictions must be of equal length")
    }
    y_i <- obs
    u_i <- pred 
        deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - (y_i - u_i)
    deviance <- 2 * sum(deviance.contribs * weights)
    
    if (calc.mean) 
        deviance <- deviance/length(obs)
    return(deviance)
}

# modified from MultiLogLoss in MLMetrics package - credit to Yachen Yan
calc.multilogloss <- function (actual, preditced) 
{
    y_true <- actual
    y_pred <- predicted
    
    if (is.matrix(y_true) == FALSE) {
        y_true <- model.matrix(~0 + ., data.frame(as.character(y_true)))
    }
    eps <- 1e-15
    N <- nrow(y_pred)
    y_pred <- pmax(pmin(y_pred, 1 - eps), eps)
    MultiLogLoss <- (-1/N) * sum(y_true * log(y_pred))
    return(MultiLogLoss)
}
