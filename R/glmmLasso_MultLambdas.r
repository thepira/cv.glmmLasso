
#' @title glmmLasso_MultLambdas
#' @description Variable selection using glmmLasso for multiple lambdas values  
#' @details Build multiple models given a sequence of lambda values
#' @author Pirapong Jitngamplang, Jared Lander
#' @export
#' @param fix A two-sided linear formula object describing the fixed-effects part of the model, with the response on the left of a ~ operator and the terms, separated by + operators, on the right. For categorical covariables use as.factor(.) in the formula. Note, that the corresponding dummies are treated as a group and are updated blockwise
#' @param rnd A two-sided linear formula object describing the random-effects part of the model, with the grouping factor on the left of a ~ operator and the random terms, separated by + operators, on the right; aternatively, the random effects design matrix can be given directly (with suitable column names). If set to NULL, no random effects are included.
#' @param data The data frame containing the variables named in formula.
#' @param family a GLM family, see glm and family. Also ordinal response models can be fitted: use family=acat() and family=cumulative() for the fitting of an adjacent category or cumulative model, respectively. If family is missing then a linear mixed model is fit; otherwise a generalized linear mixed model is fit.
#' @param lambda The penalty parameter that controls the shrinkage of fixed terms and controls the variable selection. The optimal penalty parameter is a tuning parameter of the procedure that has to be determined, e.g. by use of information criteria or cross validation. Should inputted as a numeric vector from high to low. (See details for an example.)
#' @return Returns a list glmmLasso models for each lambda value.  
#' @examples
#' 
#' linear mixed model with slope on ave.attend
#' 
#' library(glmmLasso)
#' data("soccer")
#' glmmLasso_MultLambdas(fix = points ~ transfer.spendings ball.possession + ave.attend, rnd = list(team =~ 1 + ave.attend), data = soccer, family = poisson(link = log), lambda = seq(from = 500, to = 1, by = -5))
#' 
#'  

glmmLasso_MultLambdas <- function(fix, rnd, data, 
                                  family = gaussian(link = "identity"), 
                                  lambdas = NULL,
                                  nlambdas = 100,
                                  lambda.min.ratio=ifelse(nobs < nvars, 0.01, 0.0001), 
                                  ...)
{

    # fitting first model to generate initial inputs for control parameter
    # here we use the first lambda (highest penalty) to start
    # based glmmLasso's author, glmmLasso is faster when final coefficient
    # estimates corresponding to a lambda is used as the starting value for
    # the next smaller lambda  
    
    # defining the number of observation
    nobs <- nrow(data)
    
    # defining the number of preditors based on the number of terms in fix formula
    nvars <- length(attr(terms(fix), 'term.labels'))
    
    if (is.null(lambdas))
    {

        # building the lambda vector
        lambdas <- buildLambdas(fix = fix,
                                rnd = rnd,
                                data = data, 
                                nlambdas = nlambdas, 
                                lambda.min.ratio = lambda.min.ratio)    
    }
    
    # TODO: take out the first model fit. 
    # 
    # mod1 <- glmmLasso::glmmLasso(fix = fix,
    #                   rnd = rnd,
    #                   data = data,
    #                   family = family,
    #                   lambda = lambdas[1],
    #                   ...)
    
    # modList[[1]] <- mod1
    
    # passing Q.start and Delta.start is modeled from glmmLasso demo file
    # from the "More Elegant section" 
    
    # Delta is matrix containing the estimates of fixed and random effects 
    # (columns) for each iteration (rows) of the main algorithm (i.e. before 
    # the final re-estimation step is performed, see details).
    # Passing the set of estimates from the last iteration as the 
    # 'start' parameter of the controlList
    
    # Delta.start <- mod1$Deltamatrix[mod1$conv.step, ]
    # 
    # # Q_long is a list containing the estimates of the random effects 
    # # variance-covariance parameters for each iteration of the main algorithm.
    # # Passing the variance-covaiance matrix as the q_start parameter of
    # # the controlList 
    # 
    # Q.start <- mod1$Q_long[[mod1$conv.step + 1]]
    # 
    # # building the controlList as the first starting control parameter for loop
    # controlList <- list(start = Delta.start, 
    #                     q_start = as.data.frame(Q.start))
   
    # initializing list of object to hold the model outputs 
    modList <- vector(mode = 'list', length = length(lambdas))
    
    
    # Delta.start <- fit$Deltamatrix[fit$conv.step, ]
    # Q.start <- fit$Q_long[[fit$conv.step + 1]]
    # controlList <- list(start = Delta.start, 
    #                     q_start = as.data.frame(Q.start))
    
    # controlList <- vector(mode = 'list', length = 2)
    # calling glmmLasso for each lambda value and after each fit, controlList
    # gets updated with the lastest model's coefficient to increase speed
    
    # fit first lambda
    first_fit <- glmmLasso::glmmLasso(fix = fix,
                                rnd = rnd,
                                data = data,
                                family = family,
                                lambda = lambdas[1],
                                ...)
    # builing the first Delta.start, transpose required to make dimension
    # correct for rbind later on.
    # 
    Delta.start <- first_fit$Deltamatrix[first_fit$conv.step, ] %>% t()
    Q.start <- first_fit$Q_long[[first_fit$conv.step + 1]]
    # controlList <- list(start = Delta.start, 
                        # q_start = as.data.frame(Q.start))
    
    for (l in seq_along(lambdas))
    {
        message(sprintf('Lambda: %s\n ', lambdas[l]))
        set.seed(1)
        fit <- glmmLasso::glmmLasso(fix = fix,
                                    rnd = rnd,
                                    data = data,
                                    family = family,
                                    lambda = lambdas[l],
                                    control = list(start=Delta.start[l,],
                                                   q_start=Q.start[l]),...)
        
        # storing model objects before storing to modList
        fit$lambda <- lambdas[l]
        fit$Delta.start <- Delta.start[l,]
        fit$Q.start <- Q.start[l]
        fit$data <- data
        fit$rnd <- rnd
        fit$fix <- fix
        fit$family <- family
        
        modList[[l]] <- fit
        Delta.start <- rbind(Delta.start, fit$Deltamatrix[fit$conv.step, ])
        Q.start <- c(Q.start, fit$Q_long[[fit$conv.step + 1]])
        
     
        
    }
    
    # the function returns a list of glmmLasso models 
    
    attr(modList, 'lambdas') <- lambdas
    
    class(modList) <- 'glmmLasso_MultLambdas'
    
    return(modList)
}



predict.glmmLasso_MultLambdas <- function(object, newdata, ...)
{
    # instantiating list to hold nlambdas number of n x 1 vectors 
    # pred_vec_list <- vector(mode = 'list', length = length(object))
    # storing returned vectors in a list 
    
    pred_vec_list <- purrr::map(.x = object, .f = glmmLasso:::predict.glmmLasso, 
                                # newdata = as.data.frame(newdata))
                                newdata = newdata)
    
    pred_matrix <- do.call(what = cbind, args = pred_vec_list)
    
    return(pred_matrix)
}


