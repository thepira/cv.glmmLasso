

# switch allows us to do take the family arg as assign the appropriate loss function 
cv.glmmLasso <- function(fix, rnd, data, family=gaussian, 
                         kfold, lambdas, nlambdas, lambda.min.ratio, 
                         loss=switch(family()$family, 'gaussian' = Metrics::mse,
                                     'binomial' = Metrics::logLoss,
                                     'multinomial' = cv.glmmLasso::calc.multilogloss,
                                     'poisson' = cv.glmmLasso::calc.deviance),
                         ...)
{
    # TODO: need to rewrite this with rsample
    # TODO: write documentation for all the functions
    # TODO: look at what cv.glmmnet returns and try to mimic as much as possible
    # TODO: think about the n x number of lambdas fits, and taking averages
    # TODO: check that response var line works. 
    # TODO: think about fitting lambda to the entire dataset + and then -> check glmnet documentations 
    # 
    # building randomIndices to cut up data for cross-validation
    
    # if lambda isn't specified by user, build the lambdas vector, this is static for all k folds
    if (is.null(lambdas))
    {
        
        # calculating lambda max
        lambda.max <- computeLambdaMax(fix = fix,
                                       rnd = rnd,
                                       data = data)
        # building the lambda vector
        lambdas <- buildLambdas(lambdaMax = lambda.max, 
                                nlambdas = nlambdas, 
                                lambda.min.ratio= lambda.min.ratio)   
    }
    
    nrow <- nrow(data)
    randomIndices <- dplyr::sample(nrow)
    
    # building data frame to map a specific row to kth group
    # column 1 is the row, column 2 is a randomly assigned group
    # number of groups is determined by kfold value  
    rowDF <- tibble::tibble(
        row = seq(nrow),
        group = dplyr::sample(rep(seq(kfold), length.out=nrow), replace = FALSE)
    )
    
    # sorting by group 
    rowDF <- rowDF %>% dplyr::arrange(group)
    
    # storing number of groups = kfold -> can't we just use k fold?
    numGroups <- unique(rowDF$group)
    
    #instantiating list of 
    lossvaluesList <- vector(mode = 'list', length = numGroups)
    
    for(k in numGroups)
    {
        testIndices <- dplyr::filter(rowDF, .data$group == k) %>% dplyr::pull(row)
        trainIndices <- rowDF$row[-testIndices]
        
        # fitting model
        # modList_foldk is a glmmLasso_MultLambdas object, which is a list of glmmLasso objects
        modList_foldk <- glmmLasso_MultLambdas(fix = fix,
                                      rnd = rnd,
                                      data = data %>% dplyr::slice(trainIndices),
                                      family = family,
                                      lambdas = lambdas,
                                      nlambdas = nlambdas,
                                      lambda.min.ratio = lambda.min.ratio,
                                      ...)
        
       
        
        # hacky way of getting the response variable out of the         response_var <- fix[[2]] %>% as.character()
        
        # employing the loss function in form loss(actual,predicted)
        
        # using loss function, calculating a list of loss values for each vector of prediction
        # which comes from a glmmLasso model with a specific lambda 
        actualData <- data %>% slice(testIndices) %>% pull(response_var)
       
        # predicting values for each of the glmmLasso model (100 lambda) 
        predictionList <- map(modList_foldk, stats::predict)
        
        #TODO: Continue here thinking about how to store and calculate average errors. 
        
        # storing loss values for each fold
        
        for(n in nlambdas)
        {
            lossvalueList[[k]] <- loss(actual = actualData, predicted = predictionList)    
        }
        
        
        # old code
        #lossValue <- loss(trueValues=data %>% slice(testIndices) %>% pull(response_var),
        #                            predictedValues=predictionList)
        # need to pass response of the formula into pull()
        #foldLoss[k] <- lossValue
        
        # continue here once have written the compute error function and predict function
        # as we need to generate the error matrix (5 folds row by 100 col of lambda) to calculate
        # the column mean of each lambda 1 - lambda 100.
    }
    
    
    lossvaluesList[[1]][1]
    
    # mimicking cv.glmnet return objects
    return(list(lambdas,
                cvm,
                cvsd,
                cvup,
                cvdown,
                nzero,
                glmmLasso.fit,
                lambda.min,
                lambda.1se,
                LossMean=mean(foldLoss),
                LossSD=sd(foldLoss)))
    
}