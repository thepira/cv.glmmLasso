library(lme4)

data("sleepstudy")

library(dplyr)

sleepstudy %>% head

mod1 <- lmer(Reaction ~ Days + (1 + Days | Subject), data=sleepstudy)
mod1

fixef(mod1)
ranef(mod1)

coef(mod1)