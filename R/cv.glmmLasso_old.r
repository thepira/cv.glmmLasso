
# need to build type checks for parameter passing 

cv.glmmLasso <- function(fix, rnd, data, family, switch.NR, final.re, control, kfold)
{
    #storing original paramters
   
    
    cv.fixed_formula <- fix
    cv.random_formula <- rnd
    cv.data <- data
    cv.family <- family
    cv.switch.NR <- switch.NR
    cv.final.re <- final.re
    cv.control <-control
    
    # storing new parameters
    
    cv.kfold <- kfold
    
    # call glmmLasso for many lambda to do cross validation
    
    set.seed(123)
    
    # storing the number of rows in the data set
    N<-dim(cv.data)[1]
    
    # creating random selection matrix
    ind<-sample(N,N)
    
    lambda <- seq(500,0,by=-5)
    
    # storing the number of folds we will cut the data into
    kk<- cv.kfold
    
    # nk is the index that will be used to split the data
    nk <- floor(N/kk)
    
    # deviance matrix, will be used for finding optimal lambda
    Devianz_ma<-matrix(Inf,ncol=kk,nrow=length(lambda))
    
    ## first fit good starting model
    #library(MASS);library(nlme)
    #PQL<-glmmPQL(points~1,random = ~1|team,family=family,data=soccer)
    #Delta.start<-c(as.numeric(PQL$coef$fixed),rep(0,6),as.numeric(t(PQL$coef$random$team)))
    #Q.start<-as.numeric(VarCorr(PQL)[1,1])
    
    lhs_var_index <- which(colnames(cv.data)== all.vars(cv.fixed_formula)[1])
    
    for(j in 1:length(lambda))
    {
        print(paste("Iteration ", j,sep=""))
        
        for (i in 1:kk)
        {
            if (i < kk)
            {
                indi <- ind[(i-1)*nk+(1:nk)]
            }else{
                indi <- ind[((i-1)*nk+1):N]
            }
            
            cv.data.train <- cv.data[-indi,]
            cv.data.test <- cv.data[indi,]
            
            
            
            glm <- try(glmmLasso( fix = cv.fixed_formula,
                                  rnd = cv.random_formula,  
                                  family = cv.family, 
                                  data = cv.data.train, 
                                  lambda=lambda[j],
                                  switch.NR= cv.switch.NR,
                                  final.re= cv.final.re,
                                  control= cv.control)
                        ,silent=TRUE) 
            
            if(class(glm)!="try-error")
            {  
                y.hat <- predict(glm,cv.data.test)    
                
                Devianz_ma[j,i] <- sum(family$dev.resids(cv.data.test[,lhs_var_index],
                                                       y.hat,
                                                       wt=rep(1,length(y.hat))))
            }
        }
        browser()
        print(sum(Devianz_ma[j,]))
       
    }
    
    Devianz_vec<-apply(Devianz_ma,1,sum)
    
    opt<-which.min(Devianz_vec)
    
    
    glm_final <- glmmLasso(fix = cv.fixed_formula,
                           rnd = cv.random_formula,  
                           family = cv.family, 
                           data = cv.data, 
                           lambda=lambda[opt],
                           switch.NR= cv.switch.NR,
                           final.re= cv.final.re,
                           control= cv.control)
    
    
    summary(glm_final)
    
}

library(glmmLasso)
data("soccer")
## generalized additive mixed model
## grid for the smoothing parameter

## center all metric variables so that also the starting values with glmmPQL are in the correct scaling

soccer[,c(4,5,9:16)]<-scale(soccer[,c(4,5,9:16)],center=T,scale=T)
soccer<-data.frame(soccer)


library(MASS);library(nlme)
PQL<-glmmPQL(points~1,random = ~1|team,family=poisson(link = log),data=soccer)
Delta.start<-c(as.numeric(PQL$coef$fixed),rep(0,6),as.numeric(t(PQL$coef$random$team)))
Q.start<-as.numeric(VarCorr(PQL)[1,1])



cv.glmmLasso(fix = points ~ transfer.spendings + ave.unfair.score + ball.possession + tackles + ave.attend + sold.out,
             rnd = list(team=~1),
             family = poisson(link = log),
             data = soccer,
             switch.NR = FALSE,
             final.re = TRUE,
             control = list(start=Delta.start,q_start=Q.start),
             kfold = 5)

