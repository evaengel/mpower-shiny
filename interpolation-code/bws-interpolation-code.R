## for small effect sizes
#read in bws simulation results for small effect sizes
modelsa <- read.csv("data/dataBWS_small.csv")
modelsb <- as.data.frame(modelsa)
#select useful columns in dataframe
modelsc <- modelsb[,2:6]

##Interpolation for bws simulations
#select sample sizes
samplesize <- c(modelsc$Sample.Size,2000)
#select power
pow <- c(modelsc$power,1)
#interpolate for power
z <- approx(samplesize, pow, xout=20:2000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
#labels z$x column as Sample_Size and z$y column as Power
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bws-interpolation-results/bws-interpolation-small.csv",row.names = TRUE)

## for large effect sizes
#read in bws simulation results for large effect sizes
modelsa <- read.csv("data/dataBWS_large.csv")
modelsb <- as.data.frame(modelsa)
#select useful columns in dataframe
modelsc <- modelsb[,2:6]

##Interpolation for bws simulations
#select sample sizes
samplesize <- c(modelsc$Sample.Size,2000)
#select power
pow <- c(modelsc$power,1)
#interpolate for power
z <- approx(samplesize, pow, xout=20:2000, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
#labels z$x column as Sample_Size and z$y column as Power
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"bws-interpolation-results/bws-interpolation-large.csv",row.names = TRUE)
