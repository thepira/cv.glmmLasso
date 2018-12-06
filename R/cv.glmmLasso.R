if(getRversion() >= "2.15.1")  utils::globalVariables(c(".data"))

#' @title cv.glmmLasso
#' @description Does k-fold cross validation for glmmLasso  
#' @details Build multiple models given a sequence of lambda values
#' @author Pirapong Jitngamplang, Jared Lander
#' @export
#' @importFrom dplyr "%>%"
#' @param fix A two-sided linear formula object describing the fixed-effects part of the model, with the response on the left of a ~ operator and the terms, separated by + operators, on the right. For categorical covariables use as.factor(.) in the formula. Note, that the corresponding dummies are treated as a group and are updated blockwise
#' @param rnd A two-sided linear formula object describing the random-effects part of the model, with the grouping factor on the left of a ~ operator and the random terms, separated by + operators, on the right; aternatively, the random effects design matrix can be given directly (with suitable column names). If set to NULL, no random effects are included.
#' @param data The data frame containing the variables named in formula.
#' @param family A GLM family, see [glm()] and [family()]. Also ordinal response models can be fitted: use family=acat() and family=cumulative() for the fitting of an adjacent category or cumulative model, respectively. If family is missing then a linear mixed model is fit; otherwise a generalized linear mixed model is fit.
#' @param kfold Number of folds - default is 10. Although k-folds can be as large as the sample size (leave-one-out CV), it is not recommended for large datasets. Smallest value allowable is nfolds = 3
#' @param lambdas Optional user-supplied lambda sequence; default is NULL, and glmmLasso_MultLambdas chooses its own sequence
#' @param nlambdas The number of lambdas values, default value is 100 if lambdas is not user-supplied
#' @param lambda.min.ratio Smallest value for lambda, as a fraction of lambda.max, the (data derived) entry value (i.e. the smallest value for which all coefficients are zero). The default depends on the sample size nobs relative to the number of variables nvars. If nobs > nvars, the default is 0.0001, close to zero. If nobs < nvars, the default is 0.01.
#' @param loss Loss function used to calculate error, default values is based on family: \cr
#' 
#' \itemize{
#' \item gaussian = [cv.glmmLasso::calc_mse()] 
#' \item binomial = [cv.glmmLasso::calc_logloss()] 
#' \item multinomial = [cv.glmmLasso::calc_multilogloss()] 
#' \item poisson = [cv.glmmLasso::calc_deviance()]
#'   }
#' 
#' @param lambda.final Choice for final model to use lambda.1se or lambda.min, default is lambda.1se
#' @param \dots can receive parameters accepted by glmmLasso
#' @md
#' @return A list of cross-validation values including: \cr 
#' 
#' 
#' \describe{
#' \item{lambdas}{The values of lambda used in the fits} 
#' \item{cvm}{The mean cross-validated error - a vector of length length(lambda)} 
#' \item{cvsd}{Estimate of standard error of cvm.}
#' \item{cvup}{Upper curve = cvm+cvsd.}
#' \item{cvlo}{Lower curve = cvm-cvsd.} 
#' \item{glmmLasso.final}{A fitted glmmLasso object for the full data} 
#' \item{lambda.min}{Value of lambda that gives minimum cvm} 
#' \item{lambda.1se}{Largest value of lambda such that error is within 1 standard error of the minimum}
#' }
#' 
#' 
#' @examples 
#' data("soccer", package = "glmmLasso")
#' soccer[,c(4,5,9:16)]<-scale(soccer[,c(4,5,9:16)],center=TRUE,scale=TRUE)
#' soccer <- data.frame(soccer)
#' 
#' mod1 <- cv.glmmLasso(fix = points ~ transfer.spendings + ave.unfair.score + 
#' ball.possession + tackles, rnd = list(team=~1), data = soccer, 
#' family = gaussian(link = "identity"), kfold = 5, lambda.final = 'lambda.1se')


cv.glmmLasso <- function(fix, rnd, data, 
                         family = stats::gaussian(link = "identity"), 
                         kfold = 5, lambdas = NULL, nlambdas = 100, 
                         lambda.min.ratio = ifelse(nobs < nvars, 0.01, 0.0001), 
                         loss,
                         lambda.final=c('lambda.1se', 'lambda.min'),
                         ...)
{
    lambda.final <- match.arg(lambda.final)
    
    if(missing(loss))
    {
        # switch allows us to do take the family arg as assign the appropriate 
        # loss function 
        loss <- switch(family$family, 
                       'gaussian' = calc_mse,
                       'binomial' = calc_logloss,
                       'multinomial' = calc_multilogloss,
                       'poisson' = calc_deviance)
    }
    
    x <- useful::build.x(fix, data)
    nobs <- nrow(x)
    nvars <- ncol(x)
    
    # if lambda isn't specified by user, build the lambdas vector, this is 
    # static for all k folds
    if (is.null(lambdas))
    {
        # building the lambda vector
        lambdas <- buildLambdas(fix = fix,
                                rnd = rnd,
                                data = data, 
                                nlambdas = nlambdas, 
                                lambda.min.ratio= lambda.min.ratio)   
    }
    
    
    
    
    # building data frame to map a specific row to kth group
    # column 1 is the row, column 2 is a randomly assigned group
    # number of groups is determined by kfold value  
    rowDF <- tibble::tibble(
        row = seq(nobs),
        group = sample(rep(seq(kfold), length.out=nobs), replace = FALSE)
    )
    
    # sorting by group 
    rowDF <-  dplyr::arrange(rowDF, .data$group)
    
    
    #instantiating list to hold loss and models for each fold
    lossVecList <- vector(mode = 'list', length = kfold)
    modList_foldk <- vector(mode = 'list', length = kfold)
    
    for(k in 1:kfold)
    {
        testIndices <- dplyr::filter(rowDF, .data$group == k) %>% dplyr::pull(row)
        trainIndices <- rowDF$row[-testIndices]
        
        # fitting model
        # modList_foldk is a glmmLasso_MultLambdas object, which is a list of 
        # glmmLasso objects
        
        # for showing lambda at each iterations
        # message(sprintf('Round: %s\n ', k))
        modList_foldk[[k]] <- glmmLasso_MultLambdas(fix = fix,
                                                    rnd = rnd,
                                                    data = data %>% dplyr::slice(trainIndices),
                                                    family = family,
                                                    lambdas = lambdas,
                                                    nlambdas = nlambdas,
                                                    lambda.min.ratio = lambda.min.ratio,
                                                    ...)
        
        
        
        # hacky way of getting the response variable out of the         
        response_var <- fix[[2]] %>% as.character()
        
        # pulling out actual data
        actualDataVector <- data %>% dplyr::slice(testIndices) %>% 
            dplyr::pull(response_var)
        
        # predicting values for each of the glmmLasso model (100 lambda) 
        # using matrix form for easier error calculation in loss()
        
        predictionMatrix <- predict.glmmLasso_MultLambdas(
            object = modList_foldk[[k]],
            newdata = data %>% dplyr::slice(testIndices)
        )
        
        # employing the loss function in form loss(actual,predicted)
        # using loss function, calculating a list of loss values for each vector 
        # of prediction
        # which comes from a glmmLasso model with a specific lambda 
        # storing loss values for each fold
        
        # TODO: think an error is thrown here 
        lossVecList[[k]] <- loss(actual = actualDataVector, predicted = predictionMatrix)
        # each element of this list should be 1 x nlambdas
    }
    
    #building matrix (k by nlambdas) to help calculate cross-validated mean error
    cvLossMatrix <- do.call(what = rbind, args = lossVecList)
    
    cvm = colMeans(cvLossMatrix)
    
    # calculating sd, cv, up, down
    cvsd <- apply(cvLossMatrix, 2, stats::sd, na.rm = TRUE)
    cvup <- cvm + cvsd
    cvlo <- cvm - cvsd
    
    
    # finding the minimum cvm value in order pull out the lambda.min out of 
    # list of lambda
    minIndex <- which.min(cvm)    
    lambda.min <- lambdas[minIndex]
    
    # finding 1se index by doing vectorized comparision such that cvm <= cvup 
    # of minIndex
    my1seIndex <- min(which(cvm <= cvup[minIndex]))
    lambda.1se <- lambdas[my1seIndex]
    
    # chosing lambda.final to use by checking lambda.final option
    # note that first element lambda.final default value will return true for
    # lambda.1se 
    chosenLambda <- if(lambda.final == 'lambda.1se')
    {
        lambda.1se
    }else if(lambda.final == 'lambda.min')
    {
        lambda.min
    }
    
    
    
    glmmLasso.final <- glmmLasso::glmmLasso(fix = fix,
                                            rnd = rnd,
                                            data = data,
                                            family = family,
                                            lambda = chosenLambda)
    
    # add control list argument to this to make converge faster form one that 
    # create lambda.1se
    # TODO: (maybe) For final model fit, supply control list from the model that led to     either lambda.1se or lambda.min
    
    # mimicking cv.glmnet return objects
    return_List <- list(lambdas=lambdas,
                        cvm=cvm,
                        cvsd=cvsd,
                        cvup=cvup,
                        cvlo=cvlo,
                        glmmLasso.final=glmmLasso.final,
                        lambda.min=lambda.min,
                        lambda.1se=lambda.1se)
    
    
    class(return_List) <- 'cv.glmmLasso'
    
    
    return(return_List)
    
}