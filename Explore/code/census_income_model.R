
library(readr)
library(purrr)
library(dplyr)

census_data <- read_tsv("data/census.txt")


head(census_data)
is(census_data)
View(census_data)

census_data %>% colnames()

census_data <- census_data %>% 
    select('State/U.S.-Abbreviation (USPS)',
          'Name of Area',
          'Area (Land)',
          'Area (Water)',
          'Total Population',
          'Population Density (Per Sq. Mile)',
          'Average Household Size',
          'In Labor Force 16 Years and Over:',
          'Per Capita Income (In 2016 Inflation Adjusted Dollars)') 

census_data <- census_data %>% slice(-1)

names(census_data) <- c('State',
                        'County',
                        'Land.Area',
                        'Water.Area',
                        'Total.Population',
                        'Density.Population',
                        'Household.Average.Size',
                        'Labor.Participation',
                        'Income.Per.Capita')


census_data <- census_data %>% 
    transform(Land.Area = as.numeric(Land.Area),
             Water.Area = as.numeric(Water.Area),
             Total.Population = as.numeric(Total.Population),
             Density.Population = as.numeric(Density.Population),
             Household.Average.Size = as.numeric(Household.Average.Size),
             Labor.Participation = as.numeric(Labor.Participation),
             Income.Per.Capita = as.numeric(Income.Per.Capita),
             State = as.factor(State))
             

census_data <- census_data %>% 
    mutate(Labor.Participation.Percent = Labor.Participation / Total.Population)

census_data <- census_data %>% 
    mutate(Total.Area = Land.Area + Water.Area)

library(glmmLasso)

fix_formula <- Income.Per.Capita ~ Total.Area + Total.Population + 
                Density.Population + Household.Average.Size + 
                Labor.Participation


random_formula <- list(State = ~ 1)



mm1_income <- glmmLasso(fix = fix_formula, rnd = random_formula,  
                        lambda=1, data = census_data)

mm1_income %>% summary()



mm2_income <- glmmLasso(fix = fix_formula, rnd = random_formula,  
                        lambda=10000, data = census_data)

mm2_income %>% summary()

library(purrr)

income_mm_lasso <- map(.x = c(0.01, 0,1, 1, 10, 100, 1000, 10000, 999999999), 
    .f = ~glmmLasso(fix = fix_formula, rnd = random_formula,  
                   lambda= .x, data = census_data))

#note here that glmmLasso takes factors for cluster variable

coefplot::multiplot(income_mm_lasso, sort = 'mag')


library(glmnet)
library(useful)
library(coefplot)

lm1_income <- lm(formula = fix_formula, data = census_data)

lm1_income %>% coefplot::coefplot()

incomeX <- build.x(fix_formula, data = census_data, contrasts = FALSE, 
                      sparse = TRUE)
incomeY <- build.y(fix_formula, data = census_data)

value2 <- glmnet(x = incomeX, y = incomeY,
                 family = 'gaussian')

value2 %>% plot(xvar = 'lambda')
value2 %>% coefpath(xvar = 'lambda')
