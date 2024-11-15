# File name:    example2.R
# Author:       Felix Thoemmes             
# Date:         11/1/2022


# Packages and data  -----------------------------------------------------------
library(tidyverse)
library(magrittr)


# Data            --------------------------------------------------------------
df1 <- read_csv("https://raw.githubusercontent.com/felixthoemmes/gesis_causal_mediation_2022/main/example2.csv")


# Effect estimation   ----------------------------------------------------------
#true individual level causal effect
df1$Y1 - df1$Y0

#true average causal effect - aggregating individual causal effects
mean(df1$Y1 - df1$Y0)

#prima facie effect on observed outcome
df1 %>% group_by(X) %>% summarize(mean(Y)) -> groupmeans
groupmeans[2,2] - groupmeans[1,2]

#model-based effect based on imputing potential outcomes
lm1 <- lm(Y ~ X, data=df1)
Y0pred <- predict(lm1,newdata=tibble(.rows = 16,X=0))
Y1pred <- predict(lm1,newdata=tibble(.rows = 16,X=1))
mean(Y1pred - Y0pred)


# Non-randomized example   -----------------------------------------------------


#Unadjusted prima facie effect
df1$Y_nr <- ifelse(df1$X_nr==0,df1$Y0,df1$Y1)
df1 %>% group_by(X_nr) %>% summarize(mean(Y_nr)) -> groupmeans2
groupmeans2[2,2] - groupmeans2[1,2]
lm2 <- lm(Y_nr ~ X_nr, data=df1)


#Adjusted effect - adjusted for Z
df1 %>% group_by(X_nr,Z) %>% summarize(mean(Y_nr)) -> groupmeans3

lm3 <- lm(Y_nr ~ X_nr*Z, data=df1)
Y0pred2 <- predict(lm3,newdata=tibble(.rows = 16,X_nr=0,Z=df1$Z))
Y1pred2 <- predict(lm3,newdata=tibble(.rows = 16,X_nr=1,Z=df1$Z))
mean(Y1pred2 - Y0pred2)
