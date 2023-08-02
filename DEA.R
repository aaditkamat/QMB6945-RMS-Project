library(rDEA)
library(readr)
library(tidyverse)

## Load data
df <- read_csv("data/BenchmarkingSample20230607_with_features.csv")

# Group data
grouped_df <- df %>% 
  group_by(Unit) %>% 
  summarise(across(everything(), mean), .groups = 'drop') %>% 
  as.data.frame()

## Specify Inputs and Outputs for DEA
Y <- grouped_df['Gross Profit'] # Can have more than one column
X <- grouped_df['Total Cost of Sales'] # Can have more than one column

## Calculate Naive input-oriented DEA score for first 5 units under variable returns-to-scale
units <- 1 : nrow(X)
di_naive <- dea(XREF = X, YREF = Y, X = X[units, ], Y = Y[units, ], model = 'input')

## Tabulate the dea score for each unit
efficiency_data <- data.frame(
  Efficiency = di_naive$thetaOpt,
  Unit = grouped_df$Unit
)

## Plot the efficiency for the unit in descending order
ordered_efficiency_data <- arrange(efficiency_data, Efficiency)
ordered_efficiency_data$Unit <- 
  factor(ordered_efficiency_data$Unit, 
         levels = ordered_efficiency_data$Unit) 

BLUE <- "#076fa2"
plt <- ggplot(ordered_efficiency_data) + 
  geom_col(aes(Efficiency, Unit, fill = Unit), width = 0.6) + 
  ggtitle('Efficiency for each DMU')
  
