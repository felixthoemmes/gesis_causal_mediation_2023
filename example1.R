# File name:    simplemed.R
# Author:       Felix Thoemmes             
# Date:         11/1/2022


# Packages and data  -----------------------------------------------------------
library(tidyverse)
library(psych)
library(lavaan)
library(magrittr)



df1 <- read_csv("https://raw.githubusercontent.com/felixthoemmes/IPN_workshop/master/dfex2a.csv")


# A set of regression ----------------------------------------------------------
lm(Y ~ X, data = df1) %>% coef() %>% pluck(2) -> c # the total effect
lm(M ~ X, data = df1) %>% coef() %>% pluck(2) -> a # the a path
lm(Y ~ M + X, data = df1) %>% coef() %>% pluck(2) -> b # the b path
lm(Y ~ X + M, data = df1) %>%  coef() %>% pluck(2) -> cprime # the direct effect (c')

cat("c path",round(c,2),"\n",
    "a path",round(a,2),"\n",
    "b path",round(b,2),"\n",
    "c' path",round(cprime,2),"\n",
    "a x b path",round(a*b,2),"\n",
    "c - c'",round(c-cprime,2))


# The same set of regressions (and standard errors via bootstrap) --------------
summary(mediate(Y ~ X + (M), data = df1))


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
