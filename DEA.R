library(rDEA)
library(readr)
library(tidyverse)

# Load data
df <- read_csv("data/BenchmarkingSample20230607_with_features.csv")

## Specify Inputs and Outputs for DEA
Y <- grouped_df['Gross Profit']
X <- grouped_df['Total Cost of Sales']

## Calculate Naive input-oriented DEA score for first 5 units under variable returns-to-scale
num_units <- 1 : nrow(X)
di_naive <- dea(XREF = X, YREF=Y, X = X[num_units, ], Y = Y[num_units, ], model = 'input')

## Tabulate the dea score for each unit
efficiency_data <- data.frame(
  Unit = grouped_df$Unit,
  Efficiency = di_naive$thetaOpt
)

## Plot the efficiency for the unit in descending order
ordered_efficiency_data <- arrange(efficiency_data, Efficiency)
ordered_efficiency_data$Unit <- 
  factor(ordered_efficiency_data$Unit, 
         levels = ordered_efficiency_data$Unit) 

BLUE <- "#076fa2"
benchmark <- median(efficiency_data$Efficiency)
plt <- ggplot(ordered_efficiency_data) + 
  geom_col(aes(Efficiency, Unit), fill = BLUE, width = 0.6) + 
  geom_vline(xintercept = benchmark, linetype=1, colour="red") + 
  ggtitle('Efficiency for each DMU') + 
  scale_x_continuous(breaks=c(0.0, 0.25, 0.5, 0.75, round(benchmark, 2), 1.0))
plt