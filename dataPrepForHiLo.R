# AFGR September 1st 2016

# This script is going to be used to perform all of the data prep for the Ruben Hi-Lo paper
# This will involve several tasks which are:
# 1.) Average Left and Right ROI's
# 2.) Regress out age-age^2-age^3 & volume data
# 3.) Regress out all other ROI's wihtin a modality within a ROI 

# I am going to try to build functions for each of these tasks because how many times
# have I had to re-do these freaking tasks...... toooo many.

# Load library(s)
source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
source('/home/adrose/dataPrepForHiLoPaper/scripts/functions.R')

# Now load the data
#data.values <- read.csv('/home/analysis/redcap_data/201602/go1/n1601_go1_datarel_020716.csv')
data.values <- read.csv('/home/analysis/redcap_data/201511/go1/n1601_go1_datarel_113015.csv')
modal.scores <- read.csv('/home/tymoore/GO1_GO2_CNB_Factor_Scores.csv',header=TRUE)
volume.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_antsCtVol_jlfVol.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCbf.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfGMD.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCt.csv')
rest.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/REST_QA.csv')


# Now create a really quick freesurfer volumes df
tmp <- merge(data.values, volume.data, by=c('bblid', 'scanid'))
attach(tmp)
fs.values <- cbind(bblid, scanid, t1Exclude, mpragefsFinalExclude, data.values[,106:175])
fs.values <- cbind(bblid, scanid, t1Exclude, mpragefsFinalExclude, tmp[,2296:2363], tmp[,2500:2544])
fs.values.seg <- cbind(bblid, scanid, t1Exclude, mpragefsFinalExclude, tmp[,2500:2544])
detach(tmp)
rm(tmp)

# Now limit the reho data to the analysis' that we are interested in
subj.rest.data.cols <- seq(1,28)
reho.data.cols <- seq(331, 481)
alff.data.cols <- seq(29,179)
reho.cols <- c(subj.rest.data.cols, reho.data.cols)
alff.cols <- c(subj.rest.data.cols, alff.data.cols)
reho.data <- rest.data[,reho.cols]
alff.data <- rest.data[,alff.cols]


# First thing I need to do is get an index of subjects
# that had their CNB wihtin one year of their scan
scan.Value <- data.values$ageAtGo1Scan
cnb.values <- data.values$ageAtGo1Cnb
diff.values <- scan.Value - cnb.values
acceptable.subjs <- which(diff.values <12)
bblid.index <- data.values$bblid[acceptable.subjs]
scanid.index <- data.values$scanid[acceptable.subjs]

## Now limit our data values to acceptable subjects 
# First apply the cnb restriction
volume.data <- volume.data[volume.data$bblid %in% bblid.index,]
cbf.data <- cbf.data[cbf.data$bblid %in% bblid.index,]
gmd.data <- gmd.data[gmd.data$bblid %in% bblid.index,]
ct.data <- ct.data[ct.data$bblid %in% bblid.index,]
fs.values <- fs.values[fs.values$bblid %in% bblid.index,]
reho.data <- reho.data[reho.data$bblid %in% bblid.index,]
alff.data <- alff.data[alff.data$bblid %in% bblid.index,]


# Now apply any data quality restrictions 
volume.data <- volume.data[which(volume.data$t1Exclude!=1),]
cbf.data <- cbf.data[which(cbf.data$pcaslExclude!=1),]
gmd.data <- gmd.data[which(gmd.data$t1Exclude!=1),]
ct.data <- ct.data[which(ct.data$t1Exclude!=1),]
fs.values <- fs.values[which(fs.values$t1Exclude==0 & fs.values$mpragefsFinalExclude==0),]
fs.values <- fs.values[complete.cases(fs.values),]
reho.data <- reho.data[which(reho.data$restExclude==0),]
alff.data <- alff.data[which(alff.data$restExclude==0),]


# Now lets attach the factor scrores
modal.scores <- modal.scores[which(modal.scores$Visit==1),]
volume.data <- merge(modal.scores, volume.data, by='bblid')
cbf.data <- merge(modal.scores, cbf.data, by='bblid')
gmd.data <- merge(modal.scores, gmd.data, by='bblid')
ct.data <- merge(modal.scores, ct.data, by='bblid')
fs.values <- merge(modal.scores, fs.values, by='bblid')
reho.data <- merge(modal.scores, reho.data, by='bblid')
alff.data <- merge(modal.scores, alff.data, by='bblid')


# Now average left and right
volume.data <- averageLeftAndRight(volume.data)
cbf.data <- averageLeftAndRight(cbf.data)
gmd.data <- averageLeftAndRight(gmd.data)
ct.data <- averageLeftAndRight(ct.data)
fs.values <- averageLeftAndRight(fs.values)


# Now super quickly rm extra cbf areas
namesToRm <- c('Ventral_DC', 'Lat_Vent', 'Inf_Lat_Vent', 'Cerebellum_White_Matter', 
                'Cerebellum_Exterior', 'OpticChiasm','CSF', 'Ventricle', 'Brain_Stem',
                'CerVerLob')
colsToRm <- NULL
for(value in namesToRm){
  valuesToRm <- grep(value, names(cbf.data))
  colsToRm <- append(colsToRm, valuesToRm)
}
cbf.data <- cbf.data[,-colsToRm]


# This is acceptable for the first developmental graphs
# so we should write some csv's here
# But really quickly attach age to our csv
volume.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(volume.data$bblid, data.values$bblid)]
volume.data$sex <- data.values$sex[match(volume.data$bblid, data.values$bblid)]
cbf.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(cbf.data$bblid, data.values$bblid)]
cbf.data$sex <- data.values$sex[match(cbf.data$bblid, data.values$bblid)]
gmd.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(gmd.data$bblid, data.values$bblid)]
gmd.data$sex <- data.values$sex[match(gmd.data$bblid, data.values$bblid)]
ct.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(ct.data$bblid, data.values$bblid)]
ct.data$sex <- data.values$sex[match(ct.data$bblid, data.values$bblid)]
fs.values$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(fs.values$bblid, data.values$bblid)]
fs.values$sex <- data.values$sex[match(fs.values$bblid, data.values$bblid)]

write.csv(volume.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightNoAgeRegression/meanLRVolume.data.csv', quote=F, row.names=F)
write.csv(cbf.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightNoAgeRegression/meanLRCbf.data.csv', quote=F, row.names=F) 
write.csv(gmd.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightNoAgeRegression/meanLRGMD.data.csv', quote=F, row.names=F)
write.csv(ct.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightNoAgeRegression/meanLRCT.data.csv', quote=F, row.names=F)
write.csv(fs.values, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightNoAgeRegression/meanLRFsVolume.data.csv', quote=F, row.names=F)


# And now I will produce the age regressed values
cbf.data[,32:90] <- apply(cbf.data[,32:90], 2, function(x) regressOutAge(x, cbf.data$ageAtGo1Scan))
volume.data[,27:98] <- apply(volume.data[,27:98], 2, function(x) regressOutAge(x, volume.data$ageAtGo1Scan))
gmd.data[,21:82] <- apply(gmd.data[,21:82], 2, function(x) regressOutAge(x, gmd.data$ageAtGo1Scan))
ct.data[,21:70] <- apply(ct.data[,21:70], 2, function(x) regressOutAge(x, ct.data$ageAtGo1Scan))
fs.values[,20:83] <- apply(fs.values[,20:83], 2, function(x) regressOutAge(x, fs.values$ageAtGo1Scan))

write.csv(volume.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightWithAgeRgression/meanLRAgeRegVolume.csv', quote=F, row.names=F)
write.csv(cbf.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightWithAgeRgression/meanLRAgeRegCbf.csv', quote=F, row.names=F)
write.csv(gmd.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightWithAgeRgression/meanLRAgeRegGmd.csv', quote=F, row.names=F)
write.csv(ct.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightWithAgeRgression/meanLRAgeRegCT.csv', quote=F, row.names=F)
write.csv(fs.values, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightWithAgeRgression/meanLRAgeRegFsVolume.csv', quote=F, row.names=F)

# Now I will produce the volume regressed data
# I am only testing the the function witht these values:
#	1.) CBF
#	2.) GMD
#	3.) CT
# I don't think GMD or CT will be included in the paper, and cbf should not be volume regressed
#cbf.data <- regressOutVolume(volume.data, cbf.data, 'pcasl_jlf_cbf')
gmd.data <- regressOutVolume(volume.data, gmd.data, 'mprage_jlf_gmd')
ct.data <- regressOutVolume(volume.data, ct.data, 'mprage_jlf_ct')



# Now I will produce the regressed within modality data
# Regressing wihtin modality is used to make the output 
# of the final variable selection linear model interperatble
# These values should not be used for anything excpet the final vairbale selection and model
volume.data <- regressWithinModality(volume.data, 'mprage_jlf_vol')
cbf.data <- regressWithinModality(cbf.data, 'pcasl_jlf_cbf')
gmd.data <- regressWithinModality(gmd.data, 'mprage_jlf_gmd')
ct.data <- regressWithinModality(ct.data, 'mprage_jlf_ct')

# Now write out the csv's 
write.csv(volume.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightWithAgeRgressionAndWithinModalRegression/meanLRAgeRegModRegVOL.csv', quote=F, row.names=F)
write.csv(cbf.data, '/home/adrose/dataPrepForHiLoPaper/data/meanLeftAndRightWithAgeRgressionAndWithinModalRegression/meanLRAgeRegModRegCBF.csv', quote=F, row.names=F)
