# File name:    example3.R
# Author:       Felix Thoemmes             
# Date:         11/1/2022


# Load packages ----------------------------------------------------------------
library(dagitty)
library(ggdag)

#generate DAG ------------------------------------------------------------------


d1 <- dagitty("dag{
  c1
  c2
  c3
  c4
  c5
  c6
  m 
  x [exposure]
  y [outcome]
  c1 -> x
  c1 -> y
  c2 -> x
  c3 -> y
  c4 -> c6
  c4 -> x
  c5 -> c6
  c5 -> y
  m -> y
  x -> m
  x -> y
}")


# adjustment sets and plots
adjustmentSets(d1)

ggdag(d1)
ggdag_adjustment_set(d1)


# individual paths
paths(d1,from = "x",to = "y")
