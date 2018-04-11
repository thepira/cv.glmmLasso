

library(readr)
library(dplyr)
library(purrr)


electric_data <- read_tsv("data/electric.dat")

electric_data <- read.delim("data/electric.dat", header = TRUE, sep = "")

head(electric_data)

electric_data   %>% View()

# what is the correct parameterization? what should be in the random effect formula
# fixed_formula <- treated.Pretest + Supplement + City + Grade
# random_formula <- list(Grade =~ treated.Pretest + Supplement + City)
