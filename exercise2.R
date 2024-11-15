# File name:    exercise2.R
# Author:       Felix Thoemmes             
# Date:         11/1/2022



# Data            --------------------------------------------------------------
df2 <- read_csv("https://raw.githubusercontent.com/felixthoemmes/gesis_causal_mediation_2023/main/exercise2.csv")
df2$X <- factor(df2$X)
