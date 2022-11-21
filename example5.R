# File name:    example5.R
# Author:       Felix Thoemmes             
# Date:         11/1/2022


# Load packages ----------------------------------------------------------------
library(dagitty)
library(ggdag)

#generate DAG ------------------------------------------------------------------



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

#the adjustment set is C1


