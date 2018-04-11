data(soccer)

soccer %>% View()

lambda <- seq(500,0, by=-5)

soccer_lasso <- map(.x = lambda,
                     .f = ~ glmmLasso(points ~ transfer.spendings + ave.unfair.score 
                                     + ball.possession + tackles 
                                     + ave.attend + sold.out, rnd = list(team=~1), 
                                     lambda= .x, data = soccer))

soccer_lasso %>% multiplot(sort = 'mag', intercept = FALSE)

coef(soccer_lasso[[1]])


library(glmnet)
library(useful)
library(coefplot)


fix_formula_soccer <- points ~ transfer.spendings + ave.unfair.score + 
    ball.possession + tackles + ave.attend + sold.out

random_formula_soccer <- list(team = ~1) <- 

soccerX <- build.x(fix_formula_soccer, data = soccer, contrasts = FALSE, 
                   sparse = TRUE)

soccerY <- build.y(fix_formula_soccer, data = soccer)

value2 <- glmnet(x = soccerX, y = soccerY,
                 family = 'gaussian')

value2 %>% plot(xvar = 'lambda')
value2 %>% coefpath(xvar = 'lambda')
 
## linear mixed model
lm1 <- glmmLasso(points ~ transfer.spendings + ave.unfair.score 
                 + ball.possession + tackles 
                 + ave.attend + sold.out, rnd = list(team=~1), 
                 lambda=10, data = soccer)

seq(500,0, by=-5)
