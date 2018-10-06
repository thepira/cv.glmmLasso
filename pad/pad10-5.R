m1 <- 10:1
cvup1 <- 6
m1[m1 <= cvup1]

mycvm <- cv1$cvm
mycvm[47] <- 44.3
mycvup <- 47
plot(mycvm)

library(glmnet)
?cv.glmnet

set.seed(1010)
n=1000;p=100
nzc=trunc(p/10)
x=matrix(rnorm(n*p),n,p)
beta=rnorm(nzc)
fx= x[,seq(nzc)] %*% beta
eps=rnorm(n)*5
y=drop(fx+eps)
px=exp(fx)
px=px/(1+px)
ly=rbinom(n=length(px),prob=px,size=1)
set.seed(1011)
cvob1=cv.glmnet(x,y)

cvob1$cvm %>% plot
plot(cvob1)

myMinIndex <- which.min(cvob1$cvm)
my1seIndex <- min(which(cvob1$cvm <= cvob1$cvup[myMinIndex]))
modList1 %>% purrr::map_dbl(~ max(sum(.x$coefficients != 0) - 1, 0))

cvob1$lambda[myMinIndex]
cvob1$lambda.min


cvob1$lambda[my1seIndex]
cvob1$lambda.1se

colSums(cvob1$glmnet.fit$beta != 0)



1:100 %>% map_dbl(nchar, type='character')
1:100 %>% map_dbl(~ nchar(.x, type='character'))
map_dbl(.x=1:100, .f= ~ nchar(.x, type='character'))
map_dbl(.x=1:100, .f= function(.x){ nchar(.x, type='character') })