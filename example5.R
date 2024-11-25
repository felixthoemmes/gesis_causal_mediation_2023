# File name:    example5.R
# Author:       Felix Thoemmes             
# Date:         11/1/2022


# Load packages ----------------------------------------------------------------
library(dagitty)
library(ggdag)
library(tidyverse)
library(mediation)
library(regmedint)

e#generate DAG ------------------------------------------------------------------



d1 <- dagitty("dag{
  c1
  m 
  x [exposure]
  y [outcome]
  c1 -> x
  c1 -> y
  c1 -> m
  m -> y
  x -> m
  x -> y
}")


#descendants
descendants(d1,"x")

#for x to y (total effect)
adjustmentSets(d1, type = "all")

#for x to m (a path)
adjustmentSets(d1,exposure = "x", outcome = "m", type = "all")

#for m to y (b path)
adjustmentSets(d1,exposure = "m", outcome = "y", type = "all")

#paths that do not traverse X
paths(d1, from = "m", to = "y", Z = "c1") -> p
p <- as_tibble(p)

p %>% filter(!grepl('x', paths))

# the adjustment set is C1 ------------------------------------------------------

# Packages and data  -----------------------------------------------------------


#download data from github
df5 <- read_csv("https://raw.githubusercontent.com/felixthoemmes/gesis_causal_mediation_2022/main/example5.csv")

# the causal mediation package

# first define the two regression models
# results are on absolute percent change in outcome
lm_y <- glm(y ~ m * x + c1, data = df5, family=binomial(link="probit"))
lm_m <- lm(m ~ x + c1, data = df5)

m1 <- mediate(model.m = lm_m,model.y = lm_y, data=df5, 
        sims = 5000, boot = FALSE, 
        treat="x", mediator = "m",
        control.value = 0, treat.value = 1)

summary(m1)
plot(m1)

# hypothesis test for interaction
test.TMint(m1, conf.level = .95)

lm_m2 <- lm(m ~ x + c1, data = df5)
lm_y2 <- glm(y ~ m + x + c1, data = df5, family=binomial(link="probit"))

m2 <- mediate(model.m = lm_m2,model.y = lm_y2, data=df5, 
              sims = 5000, boot = FALSE, 
              treat="x", mediator = "m",
              control.value = 0, treat.value = 1)

summary(m2)
plot(m2)
# alternative using regmedint 
# by default it appears that the covariates are also interacted, set to mean----
# results are on logistic scale

r1 <- regmedint(df5,
          yvar="y",avar="x",mvar = "m",
          cvar = "c1",a0=0,a1=1,m_cde=0,
          c_cond=-.11,mreg = "linear",yreg="logistic", interaction = TRUE)
summary(r1)

# sensitivity analysis

m2sens <- medsens(m2, rho.by=.1, eps=.01, effect.type="both")
summary(m2sens)
plot(m2sens)


# add effect modifier g to model m2

lm_m3 <- lm(m ~ x*g + c1, data = df5)
lm_y3 <- glm(y ~ m*g + x*g + c1, data = df5, family=binomial(link="probit"))

m3_g1 <- mediate(model.m = lm_m3,model.y = lm_y3, data=df5, 
              sims = 5000, boot = FALSE, 
              treat="x", mediator = "m",
              control.value = 0, treat.value = 1,
              covariates = list(g = 1))

m3_g5 <- mediate(model.m = lm_m3,model.y = lm_y3, data=df5, 
                 sims = 5000, boot = FALSE, 
                 treat="x", mediator = "m",
                 control.value = 0, treat.value = 1,
                 covariates = list(g = 5))

summary(m3_g1)
summary(m3_g5)

plot(m3_g1)
plot(m3_g5)
