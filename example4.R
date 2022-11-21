# File name:    example4.R
# Author:       Felix Thoemmes             
# Date:         11/1/2022


# Load packages ----------------------------------------------------------------
library(dagitty)
library(ggdag)

#generate DAG ------------------------------------------------------------------


d1 <- dagitty("dag{
  w2
  w3
  m 
  x [exposure]
  y [outcome]
  w2 -> m
  w2 -> x
  w2 <-> w3
  w3 -> x
  w3 -> y
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
paths(d1, from = "m", to = "y", Z = "w2") -> p
p <- as_tibble(p)

p %>% filter(!grepl('x', paths))
