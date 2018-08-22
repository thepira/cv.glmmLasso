devtools::load_all()

# make defaults arg for nlambda = length(lambdas)


library(glmmLasso)

data("soccer")
## generalized additive mixed model
## grid for the smoothing parameter

## center all metric variables so that also the starting values with glmmPQL are in the correct scaling

soccer[,c(4,5,9:16)]<-scale(soccer[,c(4,5,9:16)],center=T,scale=T)
soccer<-data.frame(soccer)


bob <- glmmLasso_MultLambdas(fix = points ~ transfer.spendings + ave.unfair.score + ball.possession + tackles + ave.attend + sold.out,
                             rnd = list(team =~ 1),
                             data = soccer,
                             family = poisson(link = log),
                             lambdas = 1,
                             nlambdas = 1)

dude <- glmmLasso::glmmLasso(fix = points ~ transfer.spendings + ave.unfair.score + ball.possession + tackles + ave.attend + sold.out,
                             rnd = list(team =~ 1),
                             data = soccer,
                             family = poisson(link = log),
                             lambda = 1)



length(bob)
length(dude)
length(bob[[1]])

identical(bob[[1]], dude)

dude
bob[[1]]

