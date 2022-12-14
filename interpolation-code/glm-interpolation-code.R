#read in glm simulation results
models <- read.csv("C:\\Users\\eva\\Documents\\Mpower - Duke Project\\datay.csv")
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
z <- approx(samplesize, pow, xout=20:2000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"C:\\Users\\eva\\Documents\\Mpower - Duke Project\\UrinaryBisphenolA.csv",row.names = TRUE)

##Interpolation for UrinaryBenzophenone3
#select for UrinaryBenzophenone3
UR <- models3[(models3$test == "UrinaryBenzophenone3"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=20:2000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"C:\\Users\\eva\\Documents\\Mpower - Duke Project\\UrinaryBenzophenone3.csv",row.names = TRUE)

##Interpolation for Methylparaben
#select for Methylparaben
UR <- models3[(models3$test == "Methylparaben"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=20:2000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"C:\\Users\\eva\\Documents\\Mpower - Duke Project\\Methylparaben.csv",row.names = TRUE)

##Interpolation for Propylparaben
#select for Propylparaben
UR <- models3[(models3$test == "Propylparaben"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=20:2000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"C:\\Users\\eva\\Documents\\Mpower - Duke Project\\Propylparaben.csv",row.names = TRUE)

##Interpolation for dichlorophenol25
#select for dichlorophenol25
UR <- models3[(models3$test == "dichlorophenol25"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=20:2000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"C:\\Users\\eva\\Documents\\Mpower - Duke Project\\dichlorophenol25.csv",row.names = TRUE)

##Interpolation for dichlorophenol24
#select for dichlorophenol24
UR <- models3[(models3$test == "dichlorophenol24"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=20:2000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"C:\\Users\\eva\\Documents\\Mpower - Duke Project\\dichlorophenol24.csv",row.names = TRUE)

##Interpolation for MBzP
#select for MBzP
UR <- models3[(models3$test == "MBzP"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=20:2000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"C:\\Users\\eva\\Documents\\Mpower - Duke Project\\MBzP.csv",row.names = TRUE)

##Interpolation for MEP
#select for MEP
UR <- models3[(models3$test == "MEP"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=20:2000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"C:\\Users\\eva\\Documents\\Mpower - Duke Project\\MEP.csv",row.names = TRUE)

##Interpolation for MiBP
#select for MiBP
UR <- models3[(models3$test == "MiBP"),]
#select sample sizes
samplesize <- UR$n
#select power
pow <- UR$power
#interpolate for power
z <- approx(samplesize, pow, xout=20:2000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"C:\\Users\\eva\\Documents\\Mpower - Duke Project\\MiBP.csv",row.names = TRUE)

##merging data frames

#reads in csv
UrinaryBisphenolA1 <- read.csv("C:\\Users\\eva\\Documents\\Mpower - Duke Project\\UrinaryBisphenolA.csv")
#labels dataframe with chemical name
UrinaryBisphenolA2 <- cbind(rep("UrinaryBisphenolA", times = 1981),UrinaryBisphenolA1[,c(2,3)])
colnames(UrinaryBisphenolA2)[1] <- "Chemical"

#reads in csv
UrinaryBenzophenone31 <- read.csv("C:\\Users\\eva\\Documents\\Mpower - Duke Project\\UrinaryBenzophenone3.csv")
#labels dataframe with chemical name
UrinaryBenzophenone32 <- cbind(rep("UrinaryBenzophenone3", times = 1981),UrinaryBenzophenone31[,c(2,3)])
colnames(UrinaryBenzophenone32)[1] <- "Chemical"

#reads in csv
Methylparaben1 <- read.csv("C:\\Users\\eva\\Documents\\Mpower - Duke Project\\Methylparaben.csv")
#labels dataframe with chemical name
Methylparaben2 <- cbind(rep("Methylparaben", times = 1981),Methylparaben1[,c(2,3)])
#names column with chemical name "Chemical"
colnames(Methylparaben2)[1] <- "Chemical"

#reads in csv
Propylparaben1 <- read.csv("C:\\Users\\eva\\Documents\\Mpower - Duke Project\\Propylparaben.csv")
#labels dataframe with chemical name
Propylparaben2 <- cbind(rep("Propylparaben", times = 1981),Propylparaben1[,c(2,3)])
#names column with chemical name "Chemical"
colnames(Propylparaben2)[1] <- "Chemical"

#reads in csv
dichlorophenol251 <- read.csv("C:\\Users\\eva\\Documents\\Mpower - Duke Project\\dichlorophenol25.csv")
#labels dataframe with chemical name
dichlorophenol252 <- cbind(rep("dichlorophenol25", times = 1981),dichlorophenol251[,c(2,3)])
#names column with chemical name "Chemical"
colnames(dichlorophenol252)[1] <- "Chemical"

#reads in csv
dichlorophenol241 <- read.csv("C:\\Users\\eva\\Documents\\Mpower - Duke Project\\dichlorophenol24.csv")
#labels dataframe with chemical name
dichlorophenol242 <- cbind(rep("dichlorophenol24", times = 1981),dichlorophenol241[,c(2,3)])
#names column with chemical name "Chemical"
colnames(dichlorophenol242)[1] <- "Chemical"

#reads in csv
MBzP1 <- read.csv("C:\\Users\\eva\\Documents\\Mpower - Duke Project\\MBzP.csv")
#labels dataframe with chemical name
MBzP2 <- cbind(rep("MBzP", times = 1981),MBzP1[,c(2,3)])
#names column with chemical name "Chemical"
colnames(MBzP2)[1] <- "Chemical"

#reads in csv
MEP1 <- read.csv("C:\\Users\\eva\\Documents\\Mpower - Duke Project\\MEP.csv")
#labels dataframe with chemical name
MEP2 <- cbind(rep("MEP", times = 1981),MEP1[,c(2,3)])
#names column with chemical name "Chemical"
colnames(MEP2)[1] <- "Chemical"

#reads in csv
MiBP1 <- read.csv("C:\\Users\\eva\\Documents\\Mpower - Duke Project\\MiBP.csv")
#labels dataframe with chemical name
MiBP2 <- cbind(rep("MiBP", times = 1981),MiBP1[,c(2,3)])
#names column with chemical name "Chemical"
colnames(MiBP2)[1] <- "Chemical"

#combines individual chemicals' dataframes into one big dataframe
combined <- rbind(UrinaryBisphenolA2,UrinaryBenzophenone32,Methylparaben2,Propylparaben2,dichlorophenol252,dichlorophenol242,MBzP2,MEP2,MiBP2)
#writes big dataframe into a csv
write.csv(combined,"glm-simulation-data.csv",row.names = TRUE)

