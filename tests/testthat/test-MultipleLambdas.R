context("MultipleLambdas")


mod_intercept_slope <- glmmLasso_MultLambdas(fix = points ~ transfer.spendings + ave.unfair.score + ball.possession + tackles + ave.attend + sold.out, 
                             rnd = list(team =~ 1),
                             data = soccer, 
                             family = poisson(link = log))

mod_intercept <- glmmLasso_MultLambdas(fix = points ~ transfer.spendings + ave.unfair.score + ball.possession + tackles + ave.attend + sold.out, 
                                             rnd = list(team =~ 1),
                                             data = soccer, 
                                             family = poisson(link = log))

test_that("The proper class is returned", {
    expect_is(mod_intercept_slope, 'glmmLasso_MultLambdas')
    
    expect_is(mod_intercept, 'glmmLasso_MultLambdas')
})

test_that('It does not have names', {
    expect_named(mod_intercept, NULL)
    expect_named(mod_intercept_slope, NULL)
})

test_that('Each element of our model is a glmmLasso', {
    expect_is(mod_intercept[[1]], 'glmmLasso')
    expect_is(mod_intercept_slope[[1]], 'glmmLasso')
    
    expect_named(mod_intercept[[1]])
    expect_named(mod_intercept_slope[[1]])
})
