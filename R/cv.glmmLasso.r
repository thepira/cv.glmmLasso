
# need to build type checks for parameter passing 

cv.glmmLasso <- function(fix, rnd, data, family, switch.NR, final.re, control)
{
    #storing paramters
    
    cv.fixed_formula <- fix
    cv.random_formula <- rnd
    cv.data <- data
    cv.family <- family
    cv.switch.NR <- switch.NR
    cv.final.re <- final.re
    cv.control <-control
    
    #call glmmLasso for many lambda to do cross validation
    
    
}