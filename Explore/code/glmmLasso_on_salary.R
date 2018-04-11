# Generate random salary data

# Set up
library(dplyr) # data wrangling
library(lme4) # modeling
library(ggplot2) # visualization

# Parameters for generating faculty salary data
departments <- c('sociology', 'biology', 'english', 'informatics', 'statistics')
base.salaries <- c(40000, 50000, 60000, 70000, 80000)
annual.raises <- c(2000, 500, 500, 1700, 500)
faculty.per.dept <- 20
total.faculty <- faculty.per.dept * length(departments)

# Generate dataframe of faculty and (random) years of experience
ids <- 1:total.faculty
department <- rep(departments, faculty.per.dept)
experience <- floor(runif(total.faculty, 0, 10))
bases <- rep(base.salaries, faculty.per.dept) * runif(total.faculty, .9, 1.1) # noise
raises <- rep(annual.raises, faculty.per.dept) * runif(total.faculty, .9, 1.1) # noise
salary_data <- data.frame(ids, department, bases, experience, raises)


# Generate salaries (base + experience * raise)
salary_data <- salary_data %>% mutate(
    salary = bases + experience * raises
)

# Model with varying slope and intercept using lme4
m3 <- lmer(salary ~ experience + (1 + experience|department), data=salary_data)
salary_data$random.slope.int.preds <- predict(m3)

# Model with varying slope and intercept using glmmLasso

library(glmmLasso)

fix_formula <- salary ~ experience + bases
random_formula <- list(department = ~ 1 + experience)


lm1_salary <- glmmLasso(fix = fix_formula, rnd = random_formula,  
                 lambda=10000, data = salary_data)

lm1_salary %>% summary()

lm2_salary <- glmmLasso(fix = fix_formula, rnd = random_formula,  
                 lambda=100, data = salary_data)

lm2_salary %>% summary()
lm2_salary$fixerror
lm2_salary$ranerror















