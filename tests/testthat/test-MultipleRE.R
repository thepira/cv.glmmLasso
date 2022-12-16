context("MultipleRandomEffects")
data("soccer", package = "glmmLasso")
soccer <- transform(soccer, teamf=factor(team), posf=factor(pos))

lambdas=seq(50, 0, by=-5)

mod2RE <- cv.glmmLasso(fix = points ~ transfer.spendings + ave.unfair.score + 
                         ball.possession + tackles, rnd = list(teamf=~1, posf=~1), data = soccer, 
                     family = gaussian(link = "identity"), kfold = 5, lambda.final = 'lambda.1se')

test_that("Final model contains specified number of random effects", {
    expect_equal(length(mod2RE$glmmLasso.final$Q), 2)
})
