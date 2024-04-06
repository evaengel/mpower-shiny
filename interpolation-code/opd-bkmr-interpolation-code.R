##for large effect size
#read in bkmr simulation results for large effect size
models <- read.csv("data/output_opd_bkmr_large.csv")
models2 <- as.data.frame(models)
#select useful columns in dataframe
models3 <- models2[,2:6]

##Interpolation for URXOP2
#select for URXOP2
UR <- models3[(models3$test == "URXOP2"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:500, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"opd-bkmr-interpolation-results/URXOP2_large.csv",row.names = TRUE)

##Interpolation for URXOP4
#select for URXOP4
UR <- models3[(models3$test == "URXOP4"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:500, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"opd-bkmr-interpolation-results/URXOP4_large.csv",row.names = TRUE)

##Interpolation for URXOP6
#select for URXOP6
UR <- models3[(models3$test == "URXOP6"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:500, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"opd-bkmr-interpolation-results/URXOP6_large.csv",row.names = TRUE)


##Interpolation for URXOP1
#select for URXOP1
UR <- models3[(models3$test == "URXOP1"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:500, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"opd-bkmr-interpolation-results/URXOP1_large.csv",row.names = TRUE)


##Interpolation for URXOP3
#select for URXOP3
UR <- models3[(models3$test == "URXOP3"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:500, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"opd-bkmr-interpolation-results/URXOP3_large.csv",row.names = TRUE)


##Interpolation for URXOP5
#select for URXOP5
UR <- models3[(models3$test == "URXOP5"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:500, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"opd-bkmr-interpolation-results/URXOP5_large.csv",row.names = TRUE)

##merging data frames

#reads in csv
URXOP2 <- read.csv("opd-bkmr-interpolation-results/URXOP2_large.csv")
#labels dataframe with chemical name
URXOP2 <- cbind(rep("URXOP2", times = 491),URXOP2[,c(2,3)])
colnames(URXOP2)[1] <- "Chemical"

#reads in csv
URXOP4 <- read.csv("opd-bkmr-interpolation-results/URXOP4_large.csv")
#labels dataframe with chemical name
URXOP4 <- cbind(rep("URXOP4", times = 491),URXOP4[,c(2,3)])
colnames(URXOP4)[1] <- "Chemical"

#reads in csv
URXOP1 <- read.csv("opd-bkmr-interpolation-results/URXOP1_large.csv")
#labels dataframe with chemical name
URXOP1 <- cbind(rep("URXOP1", times = 491),URXOP1[,c(2,3)])
colnames(URXOP1)[1] <- "Chemical"

#reads in csv
URXOP3 <- read.csv("opd-bkmr-interpolation-results/URXOP3_large.csv")
#labels dataframe with chemical name
URXOP3 <- cbind(rep("URXOP3", times = 491),URXOP3[,c(2,3)])
colnames(URXOP3)[1] <- "Chemical"

#reads in csv
URXOP5 <- read.csv("opd-bkmr-interpolation-results/URXOP5_large.csv")
#labels dataframe with chemical name
URXOP5 <- cbind(rep("URXOP5", times = 491),URXOP5[,c(2,3)])
colnames(URXOP5)[1] <- "Chemical"

#reads in csv
URXOP6 <- read.csv("opd-bkmr-interpolation-results/URXOP6_large.csv")
#labels dataframe with chemical name
URXOP6 <- cbind(rep("URXOP6", times = 491),URXOP6[,c(2,3)])
colnames(URXOP6)[1] <- "Chemical"

#combines individual chemicals' dataframes into one big dataframe
combined <- rbind(URXOP2,URXOP4,URXOP1,URXOP3,URXOP5,URXOP6)
#writes big dataframe into a csv
write.csv(combined,"opd-bkmr-interpolation-results/opd-bkmr-interpolation-large.csv",row.names = TRUE)


##for small effect size
#read in bkmr simulation results for small effect size
models <- read.csv("data/output_opd_bkmr_small.csv")
models2 <- as.data.frame(models)
#select useful columns in dataframe
models3 <- models2[,2:6]

##Interpolation for URXOP2
#select for URXOP2
UR <- models3[(models3$test == "URXOP2"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:500, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"opd-bkmr-interpolation-results/URXOP2_small.csv",row.names = TRUE)

##Interpolation for URXOP4
#select for URXOP4
UR <- models3[(models3$test == "URXOP4"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:500, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"opd-bkmr-interpolation-results/URXOP4_small.csv",row.names = TRUE)

##Interpolation for URXOP6
#select for URXOP6
UR <- models3[(models3$test == "URXOP6"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:500, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"opd-bkmr-interpolation-results/URXOP6_small.csv",row.names = TRUE)


##Interpolation for URXOP1
#select for URXOP1
UR <- models3[(models3$test == "URXOP1"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:500, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"opd-bkmr-interpolation-results/URXOP1_small.csv",row.names = TRUE)


##Interpolation for URXOP3
#select for URXOP3
UR <- models3[(models3$test == "URXOP3"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:500, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"opd-bkmr-interpolation-results/URXOP3_small.csv",row.names = TRUE)


##Interpolation for URXOP5
#select for URXOP5
UR <- models3[(models3$test == "URXOP5"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:500, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"opd-bkmr-interpolation-results/URXOP5_small.csv",row.names = TRUE)

##merging data frames

#reads in csv
URXOP2 <- read.csv("opd-bkmr-interpolation-results/URXOP2_small.csv")
#labels dataframe with chemical name
URXOP2 <- cbind(rep("URXOP2", times = 491),URXOP2[,c(2,3)])
colnames(URXOP2)[1] <- "Chemical"

#reads in csv
URXOP4 <- read.csv("opd-bkmr-interpolation-results/URXOP4_small.csv")
#labels dataframe with chemical name
URXOP4 <- cbind(rep("URXOP4", times = 491),URXOP4[,c(2,3)])
colnames(URXOP4)[1] <- "Chemical"

#reads in csv
URXOP1 <- read.csv("opd-bkmr-interpolation-results/URXOP1_small.csv")
#labels dataframe with chemical name
URXOP1 <- cbind(rep("URXOP1", times = 491),URXOP1[,c(2,3)])
colnames(URXOP1)[1] <- "Chemical"

#reads in csv
URXOP3 <- read.csv("opd-bkmr-interpolation-results/URXOP3_small.csv")
#labels dataframe with chemical name
URXOP3 <- cbind(rep("URXOP3", times = 491),URXOP3[,c(2,3)])
colnames(URXOP3)[1] <- "Chemical"

#reads in csv
URXOP5 <- read.csv("opd-bkmr-interpolation-results/URXOP5_small.csv")
#labels dataframe with chemical name
URXOP5 <- cbind(rep("URXOP5", times = 491),URXOP5[,c(2,3)])
colnames(URXOP5)[1] <- "Chemical"

#reads in csv
URXOP6 <- read.csv("opd-bkmr-interpolation-results/URXOP6_small.csv")
#labels dataframe with chemical name
URXOP6 <- cbind(rep("URXOP6", times = 491),URXOP6[,c(2,3)])
colnames(URXOP6)[1] <- "Chemical"

#combines individual chemicals' dataframes into one big dataframe
combined <- rbind(URXOP2,URXOP4,URXOP1,URXOP3,URXOP5,URXOP6)
#writes big dataframe into a csv
write.csv(combined,"opd-bkmr-interpolation-results/opd-bkmr-interpolation-small.csv",row.names = TRUE)

