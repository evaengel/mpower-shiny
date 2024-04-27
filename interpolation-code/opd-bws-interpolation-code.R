#read in bws simulation results for small effect sizes
modelsa <- read.csv("data/output_opd_bws_small.csv")
modelsb <- as.data.frame(modelsa)
#select useful columns in dataframe
modelsc <- modelsb[,2:6]

##Interpolation for bws simulations
#select sample sizes
samplesize <- modelsc$Sample.Size
#select power
pow <- modelsc$power
#interpolate for power
z <- approx(samplesize, pow, xout=20:2220, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
#labels z$x column as Sample_Size and z$y column as Power
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"opd-bws-interpolation-results/opd-bws-interpolation-small.csv",row.names = TRUE)

## for large effect sizes
#read in bws simulation results for large effect sizes
modelsa <- read.csv("data/output_opd_bws_large.csv")
modelsb <- as.data.frame(modelsa)
#select useful columns in dataframe
modelsc <- modelsb[,2:6]

##Interpolation for bws simulations
#select sample sizes
samplesize <- modelsc$Sample.Size
#select power
pow <- modelsc$power
#interpolate for power
z <- approx(samplesize, pow, xout=20:2220, method= 'linear')
#store interpolation results as data frame
zdataframe <- data.frame(x = z$x, y = z$y)
#labels z$x column as Sample_Size and z$y column as Power
colnames(zdataframe) <- c("Sample_Size","Power")
#write results into csv
write.csv(zdataframe,"opd-bws-interpolation-results/opd-bws-interpolation-large.csv",row.names = TRUE)
