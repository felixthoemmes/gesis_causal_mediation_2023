library(tidyverse)
library(emmeans)
library(lme4)
library(boot)

############################
#single experiment datagen #
############################
set.seed(123)
x <- rbinom(n = 60, size = 1, .5)
u <- rnorm(60,0,1)
m <- as.numeric(cut(.3*x + .3*u + rnorm(60,0,.5),2)) - 1
y <- .3*u + .3*x + .2*m + rnorm(60,0,.1)

df_sed <- tibble(x,m,y)
write_csv(df_sed,"df_sed.csv")

mediate.sed("y","m","x", data = df_sed, SI = TRUE, sims = 1000) -> med_sed_1
summary(med_sed_1)


#############################
#cross-over design datagen  #
#############################
set.seed(123)
exp <- rep(0:1,each=120)
id <- rep(1:120,times=2)
x <- rbinom(n = 120, size = 1, .5)
xcross <- ifelse(x==0,1,0)
u <- rnorm(120,0,1)
m <- as.numeric(cut(.3*x + .3*u + rnorm(120,0,.5),2)) - 1
mcross <- m
y <- .3*u + .3*x + .2*m + rnorm(120,0,.1)
ycross <- .3*xcross + .2*mcross + rnorm(120,0,.1)
x <- c(x,xcross)
m <- c(m,mcross)
y <- c(y,ycross)

df_cd <- tibble(exp,id,x,m,y)
write_csv(df_cd,"df_cd.csv")



#total and natural indirect effects
lmer(y ~ exp * x + (1 | id)) -> lmer1
emmeans(lmer1, ~exp + x)

#total effect from experiment 1
emmeans(lmer1, ~exp + x, contr = list(total = c(-1,0,1,0)))

#natural direct effects
emmeans(lmer1, ~exp + x, contr = list(nde0 = c(0,-1,1,0), nde1 = c(-1,0,0,1)))



###########################
# parallel design datagen #
###########################
set.seed(123)
exp <- as.numeric(rep(0:1,each=500))
#experiment 1
x1 <- rbinom(n = 500, size = 1, .5)
u <- rnorm(500,0,1)
m1 <- as.numeric(cut(.3*x1 + .3*u + rnorm(500,0,.5),2)) - 1
y1 <- .3*u + .3*x1 + .2*m1 + rnorm(500,0,.1)

#experiment 2
x2 <- rbinom(n = 500, size = 1, .5)
m2 <- rbinom(n = 500, size = 1, .5)
y2 <- .3*x2 + .2*m2 + rnorm(500,0,.1)

#pool experiments
x <- as.numeric(c(x1,x2))
m <- c(m1,m2)
y <- c(y1,y2)

df_pd <- tibble(x,m,y,exp)
write_csv(df_pd,"df_pd.csv")

#only works with binary outcomes, but should in theory work with continuous Y
# mediate.pd("y", "m", "x", "exp", df_pd, NINT = TRUE, sims = 1000) -> med_pd
# summary(med_pd)


#total effect from exp1
lm(y ~ (x)*exp, df_pd) -> lm1
emmeans(lm1,~exp+x, contr = list(total = c(-1,0,1,0)))$contrast@bhat[2] -> cont1

#direct effect from experiment 2
lm(y ~ (x + m)*exp, df_pd) -> lm2
emmeans(lm2,~exp+x, contr = list(total = c(0,-1,0,1)))$contrast@bhat[2] -> cont2

#indirect effect
cont1 - cont2

#perform manual bootstrap
# Create a function to compute the difference between means
diff_means <- function(data, indices) {
  data <-data[indices,]
  
  lm(y ~ (x)*exp, data) -> lm1
  emmeans(lm1, ~exp + x, contr = list(total = c(-1, 0, 1, 0)))$contrast@bhat[2] -> cont1
  
  lm(y ~ (x + m)*exp, data) -> lm2
  emmeans(lm2, ~exp + x, contr = list(total = c(0, -1, 0, 1)))$contrast@bhat[2] -> cont2
  
  # Indirect effect
  diff <- cont1 - cont2
  return(diff)
}

diff_means(df_pd)


  # Perform bootstrap resampling
boot_result <- boot(data = df_pd, statistic = diff_means, R = 100)

# Display the results
boot_result
