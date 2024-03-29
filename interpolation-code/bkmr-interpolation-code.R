
##for large effect size
#read in bkmr simulation results for large effect size
models <- read.csv("data/dataBKMR_large.csv")
models2 <- as.data.frame(models)
#select useful columns in dataframe
models3 <- models2[,2:6]

##Interpolation for UrinaryBisphenolA
#select for UrinaryBisphenolA
UR <- models3[(models3$test == "UrinaryBisphenolA"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:600, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/UrinaryBisphenolA_large.csv",row.names = TRUE)

##Interpolation for UrinaryBenzophenone3
#select for UrinaryBenzophenone3
UR <- models3[(models3$test == "UrinaryBenzophenone3"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:600, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/UrinaryBenzophenone3_large.csv",row.names = TRUE)

##Interpolation for Methylparaben
#select for Methylparaben
UR <- models3[(models3$test == "Methylparaben"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:600, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/Methylparaben_large.csv",row.names = TRUE)

##Interpolation for Propylparaben
#select for Propylparaben
UR <- models3[(models3$test == "Propylparaben"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:600, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/Propylparaben_large.csv",row.names = TRUE)

##Interpolation for dichlorophenol25
#select for dichlorophenol25
UR <- models3[(models3$test == "dichlorophenol25"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:600, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/dichlorophenol25_large.csv",row.names = TRUE)

##Interpolation for dichlorophenol24
#select for dichlorophenol24
UR <- models3[(models3$test == "dichlorophenol24"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:600, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/dichlorophenol24_large.csv",row.names = TRUE)

##Interpolation for MBzP
#select for MBzP
UR <- models3[(models3$test == "MBzP"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:600, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/MBzP_large.csv",row.names = TRUE)

##Interpolation for MEP
#select for MEP
UR <- models3[(models3$test == "MEP"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:600, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/MEP_large.csv",row.names = TRUE)

##Interpolation for MiBP
#select for MiBP
UR <- models3[(models3$test == "MiBP"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:600, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/MiBP_large.csv",row.names = TRUE)

##merging data frames

#reads in csv
UrinaryBisphenolA1 <- read.csv("bkmr-interpolation-results/UrinaryBisphenolA_large.csv")
#labels dataframe with chemical name
UrinaryBisphenolA2 <- cbind(rep("UrinaryBisphenolA", times = 591),UrinaryBisphenolA1[,c(2,3)])
colnames(UrinaryBisphenolA2)[1] <- "Chemical"

#reads in csv
UrinaryBenzophenone31 <- read.csv("bkmr-interpolation-results/UrinaryBenzophenone3_large.csv")
#labels dataframe with chemical name
UrinaryBenzophenone32 <- cbind(rep("UrinaryBenzophenone3", times = 591),UrinaryBenzophenone31[,c(2,3)])
colnames(UrinaryBenzophenone32)[1] <- "Chemical"

#reads in csv
Methylparaben1 <- read.csv("bkmr-interpolation-results/Methylparaben_large.csv")
#labels dataframe with chemical name
Methylparaben2 <- cbind(rep("Methylparaben", times = 591),Methylparaben1[,c(2,3)])
#names column with chemical name "Chemical"
colnames(Methylparaben2)[1] <- "Chemical"

#reads in csv
Propylparaben1 <- read.csv("bkmr-interpolation-results/Propylparaben_large.csv")
#labels dataframe with chemical name
Propylparaben2 <- cbind(rep("Propylparaben", times = 591),Propylparaben1[,c(2,3)])
#names column with chemical name "Chemical"
colnames(Propylparaben2)[1] <- "Chemical"

#reads in csv
dichlorophenol251 <- read.csv("bkmr-interpolation-results/dichlorophenol25_large.csv")
#labels dataframe with chemical name
dichlorophenol252 <- cbind(rep("dichlorophenol25", times = 591),dichlorophenol251[,c(2,3)])
#names column with chemical name "Chemical"
colnames(dichlorophenol252)[1] <- "Chemical"

#reads in csv
dichlorophenol241 <- read.csv("bkmr-interpolation-results/dichlorophenol24_large.csv")
#labels dataframe with chemical name
dichlorophenol242 <- cbind(rep("dichlorophenol24", times = 591),dichlorophenol241[,c(2,3)])
#names column with chemical name "Chemical"
colnames(dichlorophenol242)[1] <- "Chemical"

#reads in csv
MBzP1 <- read.csv("bkmr-interpolation-results/MBzP_large.csv")
#labels dataframe with chemical name
MBzP2 <- cbind(rep("MBzP", times = 591),MBzP1[,c(2,3)])
#names column with chemical name "Chemical"
colnames(MBzP2)[1] <- "Chemical"

#reads in csv
MEP1 <- read.csv("bkmr-interpolation-results/MEP_large.csv")
#labels dataframe with chemical name
MEP2 <- cbind(rep("MEP", times = 591),MEP1[,c(2,3)])
#names column with chemical name "Chemical"
colnames(MEP2)[1] <- "Chemical"

#reads in csv
MiBP1 <- read.csv("bkmr-interpolation-results/MiBP_large.csv")
#labels dataframe with chemical name
MiBP2 <- cbind(rep("MiBP", times = 591),MiBP1[,c(2,3)])
#names column with chemical name "Chemical"
colnames(MiBP2)[1] <- "Chemical"

#combines individual chemicals' dataframes into one big dataframe
combined <- rbind(UrinaryBisphenolA2,UrinaryBenzophenone32,Methylparaben2,Propylparaben2,dichlorophenol252,dichlorophenol242,MBzP2,MEP2,MiBP2)
#writes big dataframe into a csv
write.csv(combined,"bkmr-interpolation-results/bkmr-interpolation-results-large.csv",row.names = TRUE)


##for small effect size
#read in bkmr simulation results for small effect size
models <- read.csv("data/dataBKMR_small.csv")
models2 <- as.data.frame(models)
#select useful columns in dataframe
models3 <- models2[,2:6]

##Interpolation for UrinaryBisphenolA
#select for UrinaryBisphenolA
UR <- models3[(models3$test == "UrinaryBisphenolA"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:1000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/UrinaryBisphenolA_small.csv",row.names = TRUE)

##Interpolation for UrinaryBenzophenone3
#select for UrinaryBenzophenone3
UR <- models3[(models3$test == "UrinaryBenzophenone3"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:1000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/UrinaryBenzophenone3_small.csv",row.names = TRUE)

##Interpolation for Methylparaben
#select for Methylparaben
UR <- models3[(models3$test == "Methylparaben"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:1000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/Methylparaben_small.csv",row.names = TRUE)

##Interpolation for Propylparaben
#select for Propylparaben
UR <- models3[(models3$test == "Propylparaben"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:1000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/Propylparaben_small.csv",row.names = TRUE)

##Interpolation for dichlorophenol25
#select for dichlorophenol25
UR <- models3[(models3$test == "dichlorophenol25"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:1000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/dichlorophenol25_small.csv",row.names = TRUE)

##Interpolation for dichlorophenol24
#select for dichlorophenol24
UR <- models3[(models3$test == "dichlorophenol24"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:1000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/dichlorophenol24_small.csv",row.names = TRUE)

##Interpolation for MBzP
#select for MBzP
UR <- models3[(models3$test == "MBzP"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:1000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/MBzP_small.csv",row.names = TRUE)

##Interpolation for MEP
#select for MEP
UR <- models3[(models3$test == "MEP"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:1000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/MEP_small.csv",row.names = TRUE)

##Interpolation for MiBP
#select for MiBP
UR <- models3[(models3$test == "MiBP"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=10:1000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bkmr-interpolation-results/MiBP_small.csv",row.names = TRUE)

##merging data frames

#reads in csv
UrinaryBisphenolA1 <- read.csv("bkmr-interpolation-results/UrinaryBisphenolA_small.csv")
#labels dataframe with chemical name
UrinaryBisphenolA2 <- cbind(rep("UrinaryBisphenolA", times = 991),UrinaryBisphenolA1[,c(2,3)])
colnames(UrinaryBisphenolA2)[1] <- "Chemical"

#reads in csv
UrinaryBenzophenone31 <- read.csv("bkmr-interpolation-results/UrinaryBenzophenone3_small.csv")
#labels dataframe with chemical name
UrinaryBenzophenone32 <- cbind(rep("UrinaryBenzophenone3", times = 991),UrinaryBenzophenone31[,c(2,3)])
colnames(UrinaryBenzophenone32)[1] <- "Chemical"

#reads in csv
Methylparaben1 <- read.csv("bkmr-interpolation-results/Methylparaben_small.csv")
#labels dataframe with chemical name
Methylparaben2 <- cbind(rep("Methylparaben", times = 991),Methylparaben1[,c(2,3)])
#names column with chemical name "Chemical"
colnames(Methylparaben2)[1] <- "Chemical"

#reads in csv
Propylparaben1 <- read.csv("bkmr-interpolation-results/Propylparaben_small.csv")
#labels dataframe with chemical name
Propylparaben2 <- cbind(rep("Propylparaben", times = 991),Propylparaben1[,c(2,3)])
#names column with chemical name "Chemical"
colnames(Propylparaben2)[1] <- "Chemical"

#reads in csv
dichlorophenol251 <- read.csv("bkmr-interpolation-results/dichlorophenol25_small.csv")
#labels dataframe with chemical name
dichlorophenol252 <- cbind(rep("dichlorophenol25", times = 991),dichlorophenol251[,c(2,3)])
#names column with chemical name "Chemical"
colnames(dichlorophenol252)[1] <- "Chemical"

#reads in csv
dichlorophenol241 <- read.csv("bkmr-interpolation-results/dichlorophenol24_small.csv")
#labels dataframe with chemical name
dichlorophenol242 <- cbind(rep("dichlorophenol24", times = 991),dichlorophenol241[,c(2,3)])
#names column with chemical name "Chemical"
colnames(dichlorophenol242)[1] <- "Chemical"

#reads in csv
MBzP1 <- read.csv("bkmr-interpolation-results/MBzP_small.csv")
#labels dataframe with chemical name
MBzP2 <- cbind(rep("MBzP", times = 991),MBzP1[,c(2,3)])
#names column with chemical name "Chemical"
colnames(MBzP2)[1] <- "Chemical"

#reads in csv
MEP1 <- read.csv("bkmr-interpolation-results/MEP_small.csv")
#labels dataframe with chemical name
MEP2 <- cbind(rep("MEP", times = 991),MEP1[,c(2,3)])
#names column with chemical name "Chemical"
colnames(MEP2)[1] <- "Chemical"

#reads in csv
MiBP1 <- read.csv("bkmr-interpolation-results/MiBP_small.csv")
#labels dataframe with chemical name
MiBP2 <- cbind(rep("MiBP", times = 991),MiBP1[,c(2,3)])
#names column with chemical name "Chemical"
colnames(MiBP2)[1] <- "Chemical"

#combines individual chemicals' dataframes into one big dataframe
combined <- rbind(UrinaryBisphenolA2,UrinaryBenzophenone32,Methylparaben2,Propylparaben2,dichlorophenol252,dichlorophenol242,MBzP2,MEP2,MiBP2)
#writes big dataframe into a csv
write.csv(combined,"bkmr-interpolation-results/bkmr-interpolation-results-small.csv",row.names = TRUE)

