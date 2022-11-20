# File name:    exercise1.R
# Author:       Felix Thoemmes             
# Date:         11/1/2022


# Packages and data  -----------------------------------------------------------
library(tidyverse)


df1 <- read_csv("https://raw.githubusercontent.com/felixthoemmes/gesis_causal_mediation_2022/main/exercise1.csv")


# your code goes here ----------------------------------------------------------
# consider starting with writing out the regression models first ---------------

#if you want to ignore the X-M interaction you could use psych::mediate() or
#form the effects of interest yourself using lm() statements