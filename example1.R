# File name:    example1.R
# Author:       Felix Thoemmes             
# Date:         11/1/2022


# Packages and data  -----------------------------------------------------------
library(tidyverse)
library(psych)
library(lavaan)
library(magrittr)


#download data from github
df1 <- read_csv("https://raw.githubusercontent.com/felixthoemmes/gesis_causal_mediation_2022/main/example1.csv")

  
  # A set of regression ----------------------------------------------------------
  coef(lm(Y ~ X, data = df1))[2] -> c # the total effect
  coef(lm(M ~ X, data = df1))[2] -> a # the a path
  coef(lm(Y ~ M + X, data = df1))[2] -> b # the b path
  coef(lm(Y ~ M + X, data = df1))[3] -> cprime # the direct effect (c')
  #note that last two models are identical and I am just repeating code
  #because I wanted to grab two different coefficients from it in separate objects

cat("c path",round(c,2),"\n",
    "a path",round(a,2),"\n",
    "b path",round(b,2),"\n",
    "c' path",round(cprime,2),"\n",
    "a x b path",round(a*b,2),"\n",
    "c - c'",round(c-cprime,2))


# The same set of regressions (and standard errors via bootstrap) --------------
summary(psych::mediate(Y ~ X + (M), data = df1))


# The same set of regressions (and standard errors using normal theory) --------
medmodel <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
medfit <- sem(medmodel, data = df1)
summary(medfit)


# now with X-M interaction -----------------------------------------------------
# overwriting the simple objects that contain the path coefficients
df2 <- read_csv("https://raw.githubusercontent.com/felixthoemmes/gesis_causal_mediation_2022/main/example1b.csv")


coef(lm(Y ~ X, data = df2))[2] -> c # the total effect
coef(lm(M ~ X, data = df2))[2] -> a # the a path
coef(lm(Y ~ M * X, data = df2))[2] -> b1 # the b path when X = 0
coef(lm(Y ~ M * X, data = df2))[4] -> h # the interaction effect
coef(lm(Y ~ M * X, data = df2))[3] -> cprime1 # the c' path when M = 0


#two b paths when M is at the mean level of X0 and the mean level of X1
lmmodel <- lm(Y ~ M * X, data = df2)
emmeans(lmmodel,c("X","M"),contr="revpairwise", weights="proportinal",
        at=list(M=c(mean(df2$M[df2$X==0]),mean(df2$M[df2$X==1]))))$contrasts[c(1,6)]

#stringing effect together - note the alternative expression in the last line---
cat("c path",round(c,2),"\n",
    "a path",round(a,2),"\n",
    "b path for contrl group (X=0)",round(b1,2),"\n",
    "b path for treated group (X=1)",round(b1 + h,2),"\n",
    "c' path for control group (X=0)",round(cprime1,2),"\n",
    "c' path for treated group (X=1)",round(cprime1 + h,2),"\n",
    "a x b path (indirect effect for control group X=0)",round(a*b1,2),"\n",
    "a x b path (indirect effect for treated group X=1)",round(a*(b1+h),2),"\n",
    "c' (direct effect for control group X=0)",round((cprime1 + h*mean(df2$M[df2$X==0])),2),"\n",
    "c' (direct effect for control group X=1)",round((cprime1 + h*mean(df2$M[df2$X==1])),2))




