#libraries packages needed for code
library(mpower)
library(readxl)
library(bws)
library(stats)
library(reshape2)
library(tidyverse)
library(bkmr)
#names variables we want to study
chems <- c("UrinaryBisphenolA", "UrinaryBenzophenone3",
           "Methylparaben", "Propylparaben",
           "dichlorophenol25", "dichlorophenol24",
           "MBzP", "MEP", "MiBP")

#reads in nhanes data
nhanes<- read_excel('12940_2020_642_MOESM2_ESM.xlsx', sheet = "Sheet1")
#creates MixtureModel obkect
xmod <- mpower::MixtureModel(data = nhanes[, chems], method = "resampling")
#creates OutcomeModel object based on given relationship between variables (select between small and large effect)
obs_mod_small <- mpower::OutcomeModel(f = "0.16*dichlorophenol25 + 0.12*MEP", family = "gaussian")
obs_mod_large <- mpower::OutcomeModel(f = "0.32*dichlorophenol25 + 0.24*MEP", family = "gaussian")

##inference models
# define the BKMR inference model
bkmr_mod <- mpower::InferenceModel(model = "bkmr", iter = 2000, varsel = TRUE, family = "gaussian")

##code to run bkmr model small effect size for sample sizes 550 and 600 
#df <- as.data.frame(read.csv("data/dataBKMR_small.csv"))
#df <- df[,2:6]
# run simulation for sample size of 550 using 2 cores
#bkmr_out <- mpower::sim_curve(xmod=xmod, ymod=obs_mod_small, imod=bkmr_mod, s=100, n=550, cores=2)
#stores tabular summary of power curve as dataframe
#ab <- as.data.frame(mpower::summary(bkmr_out, crit = "pip", thres = 0.5, how = "greater"))
#adds new summary to dataframe of previous summaries
#df <- rbind(df, ab)
# run simulation for sample size of 600 using 2 cores
#bkmr_out <- mpower::sim_curve(xmod=xmod, ymod=obs_mod_small, imod=bkmr_mod, s=100, n=600, cores=2)
#stores tabular summary of power curve as dataframe
#ab <- as.data.frame(mpower::summary(bkmr_out, crit = "pip", thres = 0.5, how = "greater"))
#adds new summary to dataframe of previous summaries
#df <- rbind(df, ab)
#saves dataframe to csv
#write.csv(df,"data/dataBKMR_small.csv",row.names = TRUE)

#for loop for bkmr model of small effect
df <- data.frame()
for(i in 1:50){
        # run simulation for 100 iterations, for a sample size of 10, using 2 cores
        bkmr_out <- mpower::sim_curve(xmod=xmod, ymod=obs_mod_small, imod=bkmr_mod, s=100, n=(10*i), cores=2)
        #stores tabular summary of power curve as dataframe
        ab <- as.data.frame(mpower::summary(bkmr_out, crit = "pip", thres = 0.5, how = "greater"))
        #adds new summary to dataframe of previous summaries
        df <- rbind(df, ab)
}
#saves dataframe to csv
write.csv(df,"data/dataBKMR_small.csv",row.names = TRUE)


##code to run bkmr model large effect size for sample sizes 550 and 600 
#df <- as.data.frame(read.csv("data/dataBKMR_large.csv"))
#df <- df[,2:6]
# run simulation for sample size of 550 using 2 cores
#bkmr_out <- mpower::sim_curve(xmod=xmod, ymod=obs_mod_large, imod=bkmr_mod, s=100, n=550, cores=2)
#stores tabular summary of power curve as dataframe
#ab <- as.data.frame(mpower::summary(bkmr_out, crit = "pip", thres = 0.5, how = "greater"))
#adds new summary to dataframe of previous summaries
#df <- rbind(df, ab)
# run simulation for sample size of 600 using 2 cores
#bkmr_out <- mpower::sim_curve(xmod=xmod, ymod=obs_mod_large, imod=bkmr_mod, s=100, n=600, cores=2)
#stores tabular summary of power curve as dataframe
#ab <- as.data.frame(mpower::summary(bkmr_out, crit = "pip", thres = 0.5, how = "greater"))
#adds new summary to dataframe of previous summaries
#df <- rbind(df, ab)
#saves dataframe to csv
#write.csv(df,"data/dataBKMR_large.csv",row.names = TRUE)

#for loop for bkmr model of large effect
df <- data.frame()
for(i in 1:50){
        # run simulation for 100 iterations, for a sample size of 10, using 2 cores
        bkmr_out <- mpower::sim_curve(xmod=xmod, ymod=obs_mod_large, imod=bkmr_mod, s=100, n=(10*i), cores=2)
        #stores tabular summary of power curve as dataframe
        ab <- as.data.frame(mpower::summary(bkmr_out, crit = "pip", thres = 0.5, how = "greater"))
        #adds new summary to dataframe of previous summaries
        df <- rbind(df, ab)
}
#saves dataframe to csv
write.csv(df,"data/dataBKMR_large.csv",row.names = TRUE)

#creates InferenceModel for glm
glm_mod <- InferenceModel(model = "glm", family = "gaussian")
n_cores <- 2
s <- 1000
df <- data.frame()

#n can't be greater than 2372 (bc that's the number of observations in dataset)
#for loop for glm model for small effect
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
write.csv(df,"data/dataGLM_small.csv",row.names = TRUE)

#n can't be greater than 2372 (bc that's the number of observations in dataset)
#for loop for glm model for large effect
df <- data.frame()
for(i in 2:200){
        #creates power curve for glm model
        glm_out <- mpower::sim_curve(xmod=xmod, ymod=obs_mod_large, imod=glm_mod, s=s, n=(i*10), cores=2)
        #stores tabular summary of power curve as dataframe
        ab <- as.data.frame(summary(glm_out, crit = "pval", thres = 0.05, how = "lesser"))
        #adds new summary to dataframe of previous summaries
        df <- rbind(df, ab)
}
#saves dataframe to csv
write.csv(df,"data/dataGLM_large.csv",row.names = TRUE)

#change chains in bws_mod InferenceModel to chains = 1 when using bws_mod for for loop 
#create inference model for BWS
bws_mod <- InferenceModel(model = "bws", iter = 5000, chains = 2, refresh = 0, family = "gaussian")
n_cores <- 2
s <- 1000
df <- data.frame()

#creates power curve for bws model for small effect
bws_out <- mpower::sim_curve(xmod=xmod, ymod=obs_mod_small, imod=bws_mod, s=100, n=10, cores=2)
View(summary(bws_out, crit = "beta", thres = 0.05, how = "lesser"))
#n can't be greater than 2372 (bc that's the number of observations in dataset)
#for loop for bws model for small effect
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
        write.csv(dfa,"data/dataBWS_small.csv",row.names = TRUE)
}
#save to csv
write.csv(dfa,"data/dataBWS_small.csv",row.names = TRUE)

#creates power curve for bws model for large effect
bws_out <- mpower::sim_curve(xmod=xmod, ymod=obs_mod_large, imod=bws_mod, s=100, n=10, cores=2)
View(summary(bws_out, crit = "beta", thres = 0.05, how = "lesser"))
#n can't be greater than 2372 (bc that's the number of observations in dataset)
#for loop for bws model for small effect
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
        write.csv(dfa,"data/dataBWS_large.csv",row.names = TRUE)
}
#save to csv
write.csv(dfa,"data/dataBWS_large.csv",row.names = TRUE)

##create correlation matrices
#creates list of chemical names
chems <- c("UrinaryBisphenolA", "UrinaryBenzophenone3",
           "Methylparaben", "Propylparaben",
           "dichlorophenol25", "dichlorophenol24",
           "MBzP", "MEP", "MiBP")
#reads in nhanes data
nhanes<- read_excel('12940_2020_642_MOESM2_ESM.xlsx', sheet = "Sheet1")
#selects for nhanes data with those chemicals
nhanes1 <- nhanes[,chems]
#read nhanes1 into a csv
write.csv(nhanes1,"data/nhanes-data.csv",row.names = TRUE)
#create correlation matrix
nhanes1 %>%
        cor(method = "spearman") %>%
        reshape2::melt() %>%
        ggplot(aes(!!sym("Var2"), !!sym("Var1"), fill = !!sym("value"))) + 
        geom_tile() +
        scale_fill_gradient2(low = "#0072B2", mid = "white", high = "#d55E00",
                             limit = c(-1, 1), name = "Spearman\nCorrelation") + 
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              panel.background = element_blank()) +
        coord_fixed() + labs(x = "", y = "", title = "Spearman correlation matrix of original data")
