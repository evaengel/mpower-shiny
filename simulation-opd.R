library(mpower)
library(bws)
library(tidyverse)
library(bkmr)

#reads in OPD data
opd_df <- read.csv('data/opd.csv')
opd_df <- opd_df %>%
  mutate(DEP=URXOP2 + URXOP4 + URXOP6,
         DMP=URXOP1 + URXOP3 + URXOP5) %>%
  mutate(DAP=DEP+DMP)

#names variables we want to study
chems <- c("DEP", "DMP", "DAP")

#creates MixtureModel object
xmod <- mpower::MixtureModel(data = opd_df[, chems], method = "resampling")

#creates OutcomeModel object based on given relationship between variables (select between small and large effect)
# small effect estimates are from this paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4892910/
obs_mod_small <- mpower::OutcomeModel(f = "-4.17*log10(DAP) - 3.64*log10(DMP)", family = "gaussian")
obs_mod_large <- mpower::OutcomeModel(f = "–7.00*log10(DAP) + –5.97*log10(DMP)", family = "gaussian")


##inference models

########## GLM ##########
glm_mod <- InferenceModel(model = "glm", family = "gaussian")

#for loop for GLM model of SMALL effect
df <- data.frame()
for(i in 2:200){
  #creates power curve for glm model
  glm_out <- mpower::sim_curve(xmod=xmod, ymod=obs_mod_small, imod=glm_mod, s=s, n=(i*10), cores=2)
  #stores tabular summary of power curve as dataframe
  ab <- as.data.frame(summary(glm_out, crit = "pval", thres = 0.05, how = "lesser"))
  #adds new summary to dataframe of previous summaries
  df <- rbind(df, ab)
}
#saves dataframe to csv
write.csv(df,"data/output_opd_glm_small.csv", row.names = TRUE)

#for loop for GLM model of LARGE effect
df <- data.frame()
for(i in 2:200){
  #creates power curve for glm model
  glm_out <- mpower::sim_curve(xmod=xmod, ymod=obs_mod_large, imod=glm_mod, s=s, n=(i*10), cores=2)
  #stores tabular summary of power curve as dataframe
  ab <- as.data.frame(summary(glm_out, crit = "pval", thres = 0.05, how = "lesser"))
  #adds new summary to dataframe of previous summaries
  df <- rbind(df, ab)
}
write.csv(df,"data/output_opd_glm_large.csv", row.names = TRUE)



########### BWS ##########
bws_mod <- InferenceModel(model = "bws", iter = 5000, chains = 2, refresh = 0, family = "gaussian")

# SMALL effect
dfa <- data.frame()
for(i in seq(from = 20, to= 2220, by = 100)){
  #creates power curve for bws model
  bws_out <- mpower::sim_curve(xmod=xmod, ymod=obs_mod_small, imod=bws_mod, s=1000, n=i, cores=2)
  #saves tabular summary of power curve as dataframe
  ab <- as.data.frame(summary(bws_out, crit = "beta", thres = 0.05, how = "lesser"))
  #creates new column and puts sample size in it
  ab[,4] <- (i*10)
  colnames(ab)[colnames(ab) == "n"] <- "Sample.Size"
  #adds summary of power curve to dataframe of previous summaries
  dfa <- rbind(dfa, ab)
  #saves dataframe to csv
  write.csv(dfa,"data/output_opd_bws_small.csv",row.names = TRUE)
}
write.csv(df,"data/output_opd_bws_small.csv", row.names = TRUE)

# LARGE effect
dfa <- data.frame()
for(i in seq(from = 20, to= 2220, by = 100)){
  #creates power curve for bws model
  bws_out <- mpower::sim_curve(xmod=xmod, ymod=obs_mod_large, imod=bws_mod, s=1000, n=i, cores=2)
  #saves tabular summary of power curve as dataframe
  ab <- as.data.frame(summary(bws_out, crit = "beta", thres = 0.05, how = "lesser"))
  #creates new column and puts sample size in it
  ab[,4] <- (i*10)
  colnames(ab)[colnames(ab) == "n"] <- "Sample.Size"
  #adds summary of power curve to dataframe of previous summaries
  dfa <- rbind(dfa, ab)
  #saves dataframe to csv
  write.csv(dfa,"data/output_opd_bws_large.csv",row.names = TRUE)
}
write.csv(df,"data/output_opd_bws_large.csv", row.names = TRUE)



########### BKMR ##########
# define the BKMR inference model
bkmr_mod <- mpower::InferenceModel(model = "bkmr", iter = 2000, varsel = TRUE, family = "gaussian")

#for loop for bkmr model of SMALL effect
df <- data.frame()
for(i in 1:50){
  # run simulation for 100 iterations, for a sample size of 10, using 2 cores
  bkmr_out <- mpower::sim_curve(xmod=xmod, ymod=obs_mod_small, imod=bkmr_mod, s=100, n=(10*i), cores=2)
  #stores tabular summary of power curve as dataframe
  ab <- as.data.frame(mpower::summary(bkmr_out, crit = "pip", thres = 0.5, how = "greater"))
  #adds new summary to dataframe of previous summaries
  df <- rbind(df, ab)
  write.csv(df,"data/output_opd_bkmr_small.csv",row.names = TRUE)
}
#saves dataframe to csv
write.csv(df,"data/output_opd_bkmr_small.csv",row.names = TRUE)

#for loop for bkmr model of LARGE effect
df <- data.frame()
for(i in 1:50){
  # run simulation for 100 iterations, for a sample size of 10, using 2 cores
  bkmr_out <- mpower::sim_curve(xmod=xmod, ymod=obs_mod_large, imod=bkmr_mod, s=100, n=(10*i), cores=2)
  #stores tabular summary of power curve as dataframe
  ab <- as.data.frame(mpower::summary(bkmr_out, crit = "pip", thres = 0.5, how = "greater"))
  #adds new summary to dataframe of previous summaries
  df <- rbind(df, ab)
  write.csv(df,"data/output_opd_bkmr_large.csv",row.names = TRUE)
}
#saves dataframe to csv
write.csv(df,"data/output_opd_bkmr_large.csv",row.names = TRUE)

