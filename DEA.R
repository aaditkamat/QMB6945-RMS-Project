library(rDEA)
library(readr)
library(dplyr)

## Load data
df <- read_csv("Desktop/Projects/QMB6945-RMS-Project/data/BenchmarkingSample20230607_with_features.csv")
View(df)

# Group data
grouped_df <- df[c('Unit', 'Gross Profit', 'Total Cost of Sales')] %>% 
  group_by(Unit) %>% 
  summarise(across(everything(), mean), .groups='drop') %>% 
  as.data.frame()

## Specify Inputs and Outputs for analysis
Y = grouped_df['Gross Profit'] # Can have more than one column
X = grouped_df['Total Cost of Sales'] # Can have more than one column

## Calculate Naive input-oriented DEA score for first 5 units under variable returns-to-scale
units=1:5
di_naive = dea(XREF=X, YREF=Y, X=X[units, ], Y=Y[units, ], model = 'input')
print(di_naive$thetaOpt)
