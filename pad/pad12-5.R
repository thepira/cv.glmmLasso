library(glmmLasso)
library(dplyr)

data("soccer", package = "glmmLasso")
soccer[,c(4,5,9:16)]<-scale(soccer[,c(4,5,9:16)],center=TRUE,scale=TRUE)
soccer<-data.frame(soccer)

fix = points ~ transfer.spendings + ave.unfair.score 
+ ball.possession + tackles 
+ ave.attend + sold.out


cv.glmmLasso::cv.glmmLasso(fix = points ~ transfer.spendings + ave.unfair.score 
             + ball.possession + tackles 
             + ave.attend + sold.out, 
             rnd = list(team=~1), 
             data = soccer, 
             family= gaussian(link = "identity"), 
             kfold = 5, 
             lambda.final= 'lambda.1se')
