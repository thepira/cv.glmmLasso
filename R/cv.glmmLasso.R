

# switch allows us to do take the family arg as assign the appropriate loss function 
cv.glmmLasso <- function(fix, rnd, data, family=gaussian, 
                         kfold, lambdas = NULL, nlambdas, lambda.min.ratio, 
                         loss=switch(family()$family, 'gaussian' = calc_mse,
                                     'binomial' = calc_logloss,
                                     'multinomial' = calc_multilogloss,
                                     'poisson' = calc_deviance),
                         ...)
{
    # TODO: need to rewrite this with rsample
    # TODO: write documentation for all the functions
    # TODO: think about fitting lambda to the entire dataset + and then -> check glmnet documentations 
    # 
    # building randomIndices to cut up data for cross-validation
    
    # if lambda isn't specified by user, build the lambdas vector, this is static for all k folds
    if (is.null(lambdas))
    {
        # building the lambda vector
        lambdas <- buildLambdas(fix = fix,
                                rnd = rnd,
                                data = data, 
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
    lossVecList <- vector(mode = 'list', length = numGroups)
    modList_foldk <- vector(mode = 'list', length = numGroups)
    
    for(k in numGroups)
    {
        testIndices <- dplyr::filter(rowDF, .data$group == k) %>% dplyr::pull(row)
        trainIndices <- rowDF$row[-testIndices]
        
        # fitting model
        # modList_foldk is a glmmLasso_MultLambdas object, which is a list of glmmLasso objects
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
        actualDataVector <- data %>% dplyr::slice(testIndices) %>% dplyr::pull(response_var)
       
        # predicting values for each of the glmmLasso model (100 lambda) 
        # using matrix form for easier error calculation in loss()
        # predictionMatrix <- purrr::map(.x = modList_foldk[k], .f = predict.glmmLasso_MultLambdas, 
        #                             newdata = data %>% dplyr::slice(testIndices))
        #TODO: Start here!
        predictionMatrix <- predict.glmmLasso_MultLambdas(
            modList_foldk[[k]],
            newdata = data %>% dplyr::slice(testIndices)
        )
            
        # employing the loss function in form loss(actual,predicted)
        # using loss function, calculating a list of loss values for each vector of prediction
        # which comes from a glmmLasso model with a specific lambda 
        # storing loss values for each fold
        lossVecList[k] <- loss(actual = actualDataVector, predicted = predictionMatrix)
        
    }
    
    #building matrix (k by nlambda) to help calculate cross-validated mean error
    cvLossMatrix <- do.call(what = rbind, args = lossVecList)
    cvm = colMeans(cvLossMatrix)
    
    # calculating sd, cv, up, down
    cvsd <- apply(cvLossMatrix, 1, sd, na.rm = TRUE)
    cvup <- cvm + cvsd
    cvlo <- cvm - cvsd
    nzero <- #?? which fold is this?? take a look at each glmmLasso objects for each lambda, count non-zero coef
    
    glmmLasso.fit <- glmmLasso::glmmLasso(fix = fix,
                                          rnd = rnd,
                                          data = data,
                                          family = family,
                                          lambda = lambda.1se,
                                          ...)
    
    minIndex <- which.min(cvsd)    
    lambda.min <- lambdas[minIndex]
    
    # figuring out the OneSEIndex with do-while style loop
    OneSEIndex <- minIndex
    repeat
    {
        if(cvup < cvsd[OneSEIndex])
        {break}
        OneSEIndex <- OneSEIndex + 1
    }
    
    lambda.1se <- lambdas[OneSEIndex]
        
    
    
    
    # mimicking cv.glmnet return objects
    return(list(lambdas,
                cvm,
                cvsd,
                cvup,
                cvlo,
                nzero,
                glmmLasso.fit,
                lambda.min,
                lambda.1se,
                LossMean=mean(foldLoss),
                LossSD=sd(foldLoss)))
    
}