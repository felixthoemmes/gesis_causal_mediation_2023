# File name:    exercise5.R
# Author:       Felix Thoemmes             
# Date:         11/1/2022


# Load packages ----------------------------------------------------------------
library(dagitty)
library(ggdag)
library(tidyverse)
library(mediation)
library(regmedint)

df5 <- read_csv("https://raw.githubusercontent.com/felixthoemmes/gesis_causal_mediation_2022/main/exercise5.csv")

#generate DAG ------------------------------------------------------------------



d1 <- dagitty("dag{
  g
  c1
  c2
  c3
  g
  m 
  x [exposure]
  y [outcome]
  c1 -> x 
  c1 -> y 
  c1 -> m 
  c2 -> y 
  c2 -> m 
  c3 -> x 
  c3 -> y 
  g -> x
  g -> m
  m -> y
  x -> m
  x -> y
}")

ggdag(d1)

#descendants
descendants(d1,"x")

#for x to y (total effect)
adjustmentSets(d1, type = "all")

#for x to m (a path)
adjustmentSets(d1,exposure = "x", outcome = "m", type = "all")

#for m to y (b path)
adjustmentSets(d1,exposure = "m", outcome = "y", type = "all")

#paths that do not traverse X
paths(d1, from = "m", to = "y", Z = c("c1","c2","c3")) -> p
p <- as_tibble(p)

p %>% filter(!grepl('x', paths))

# the adjustment set is C1,C2,C3, G------------------------------------------------
isAdjustmentSet(x = d1,exposure = "x",outcome = "m",Z = c("c1","c2","c3","g"))
isAdjustmentSet(x = d1,exposure = "x",outcome = "y", Z = c("c1","c2","c3","g"))
isAdjustmentSet(x = d1,exposure = "m",outcome = "y", Z = c("c1","c2","c3","g","x"))

# the causal mediation package

# first define the two regression models with interactions
lm_y <- lm(y ~ m * x + c1 + c2 + c3 + g, data = df5)
lm_m <- lm(m ~ x + c1 + c2 + c3 + g, data = df5)

m1 <- mediate(model.m = lm_m,model.y = lm_y, data=df5, 
              boot = FALSE, sims = 5000,
              treat="x", mediator = "m",
              control.value = 74107, treat.value = 86201) 

# note the control and treated value - mean and mean + 1 SD

summary(m1)
plot(m1)

# hypothesis test for interaction
test.TMint(m1, conf.level = .95)


lm_m2 <- lm(m ~ x + c1 + c2 + c3 + g, data = df5)
lm_y2 <- lm(y ~ m + x + c1 + c2 + c3 + g, data = df5)

m2 <- mediate(model.m = lm_m2,model.y = lm_y2, data=df5, 
              sims = 5000, boot = FALSE, 
              treat="x", mediator = "m",
              control.value = 74107, treat.value = 86201)

summary(m2)
plot(m2)


# sensitivity analysis

m2sens <- medsens(m2, rho.by=.01, eps=.01, effect.type="both")
summary(m2sens)
plot(m2sens)


# add effect modifier g to model m2

lm_m3 <- lm(m ~ x*g + c1 + c2 + c3, data = df5)
lm_y3 <- lm(y ~ m*g + x*g + c1 + c2 + c3, data = df5)

m3_g0 <- mediate(model.m = lm_m3,model.y = lm_y3, data=df5, 
                 sims = 5000, boot = FALSE, 
                 treat="x", mediator = "m",
                 control.value = 74107, treat.value = 86201,
                 covariates = list(g = 0))

m3_g1 <- mediate(model.m = lm_m3,model.y = lm_y3, data=df5, 
                 sims = 5000, boot = FALSE, 
                 treat="x", mediator = "m",
                 control.value = 74107, treat.value = 86201,
                 covariates = list(g = 1))

summary(m3_g0)
summary(m3_g1)

plot(m3_g0)
plot(m3_g1)
