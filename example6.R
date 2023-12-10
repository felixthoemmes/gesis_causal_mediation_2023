library(tidyverse)
library(boot)
library(emmeans)
library(lme4)
library(readr)
library(mediation)

###################
#single-experiment
###################

read_csv("https://raw.githubusercontent.com/felixthoemmes/gesis_causal_mediation_2023/main/df_sed.csv") -> df_sed
mediate.sed("y","m","x", data = df_sed, SI = TRUE, sims = 1000) -> med_sed_1
summary(med_sed_1)


##################
#cross-over design
##################
read_csv("https://raw.githubusercontent.com/felixthoemmes/gesis_causal_mediation_2023/main/df_cd.csv") -> df_cd

#total and natural indirect effects
lmer(y ~ exp * x + (1 | id), data = df_cd) -> lmer1
emmeans(lmer1, ~exp + x)

#total effect from experiment 1
emmeans(lmer1, ~exp + x, contr = list(total = c(-1,0,1,0)))

#natural direct effects
emmeans(lmer1, ~exp + x, contr = list(nde0 = c(0,-1,1,0), nde1 = c(-1,0,0,1)))


#################
#parallel design
#################
read_csv("https://raw.githubusercontent.com/felixthoemmes/gesis_causal_mediation_2023/main/df_pd.csv") -> df_pd


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


