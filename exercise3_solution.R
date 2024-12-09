# File name:    exercise3_solution.R
# Author:       Felix Thoemmes             
# Date:         11/1/2022


# Load packages ----------------------------------------------------------------
library(dagitty)
library(ggdag)

#generate DAG ------------------------------------------------------------------

d_ex <- dagitty("dag{
  z
  w
  v
  x [exposure]
  y [outcome]
  x -> y
  w -> y
  z -> w
  v -> y
  v -> z
  z -> v
  v -> x
  x -> v
  w -> x
  x -> w
}")


# adjustment sets and plots
adjustmentSets(d_ex)
adjustmentSets(d_ex, exposure = "x", outcome = "y")

ggdag(d_ex)
ggdag_adjustment_set(d_ex)


# individual paths
paths(d_ex,from = "x",to = "y")
paths(d_ex,from = "v",to = "w")
