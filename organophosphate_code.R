library(nhanesA)
# download 2015 data for 
# Organophosphate Insecticides - Dialkyl Phosphate Metabolites - Urine (OPD_I)
# see https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/OPD_I.htm for more info
data2015 <- nhanes('OPD_I')
# download nhanes codebook
codebook <- nhanesCodebook('OPD_I')
# remove non-numeric or irrelevant variables (SEQN, WTSC2YR, URD24DLC, URDCPMLC,URD4FPLC, URDOPMLC,URDOXYLC,URDPARLC,URDTCCLC)
data <- subset(data2015, select = -c(SEQN,WTSB2YR,URDOP1LC,URDOP2LC,URDOP3LC,URDOP4LC,URDOP5LC,URDOP6LC))
# make all variables numeric
data1 <- sapply(data, as.numeric)
# write data to csv file
write.csv(data1,"data/opd.csv",row.names = TRUE)

# libraries packages needed for code
library(mpower)
library(readxl)
library(bws)
library(stats)
library(reshape2)
library(tidyverse)
library(bkmr)
# read in data
opd <- read.csv("data/opd.csv")
# creates MixtureModel obkect
mixture <- mpower::MixtureModel(data = opd, method = "resampling")
# creates OutcomeModel object based on given relationship between variables (select between small and large effect)
obs_mod_small <- mpower::OutcomeModel(f = "", family = "gaussian")
obs_mod_large <- mpower::OutcomeModel(f = "", family = "gaussian")


