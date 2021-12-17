##########################################################################################
######################### Checking multicolinearity using body dimension data
######################### Author: Shuying Sun and Ryan Zamora 
######################### Date modified: Dec 16, 2021
######################### File name: Reproducible.R.Script.v1.0.R
#########################           modified based on MsCode.Apr3.2020.NL2.50p10levs.R
######################### Data download: http://jse.amstat.org/datasets/body.dat.txt
########################
##########################################################################################

# Dec 16, 2021 Notes by SS: 
# This Dec 16, 2021version "Reproducible.R.Script.R" is revised based on 
# "MsCode.Apr3.2020.NL2.50p10levs.R" which has more detailed information. I cleaned it 
# to make it easier for users. Later I will still use "MsCode.Apr3.2020.NL2.50p10levs.R"
# because the original version has some debugged notes:

# Clears Environment
rm(list=ls())

##########################################################################################
######################### Set a directory/folders to save all tables and figures
##########################################################################################

# This one should be set by the user. 
# The following is an exaample folder/directory set by the user.

figure.table.dir= "/Users/ssun/Desktop/Work/Mar24.2020.mcperturb/Mar23.2020.Mon9pm.mcperturb/FigureTable.Apr3.2020.NL2.50p10step/"

##########################################################################################
######################### Sourcing the "mcperturb" functions
##########################################################################################

# After downloading these 18 files from https://github.com/ss355/mcperturb (mcperturb.v1.0), 
# save it in a director, and then read/load it into R using the following code. 

# Open R and then run the following R scripts

code.dir<-"/home/user/mcperturb/" # Suppose this is the folder saves all 18 R code files, 
# Code directory should be set by the user. 
# For example, the following one is used by the author

# code.dir = "/Users/ssun/Desktop/Work/Mar24.2020.mcperturb/Mar23.2020.Mon9pm.mcperturb/R.code/mcperturb_12_30_19"

source(paste(code.dir, "densPlots.R", sep = "/"))
source(paste(code.dir, "diagOut.R", sep = "/"))
source(paste(code.dir, "implausStats.R", sep = "/"))
source(paste(code.dir, "rsqdPlots.R", sep = "/"))
source(paste(code.dir, "randomNoiseMat.R", sep = "/"))
source(paste(code.dir, "regModelStats.R", sep = "/"))
source(paste(code.dir, "noiseLevelDiagOutList.R", sep = "/"))
source(paste(code.dir, "noiseLevelsList.R", sep = "/"))
source(paste(code.dir, "overallDiagsDiffs.R", sep = "/"))
source(paste(code.dir, "overallDiagsout.R", sep = "/"))
source(paste(code.dir, "overallDiagsRank.R", sep = "/"))
source(paste(code.dir, "overallDiagsPlots.R", sep = "/"))
source(paste(code.dir, "boxplotAllVars.R", sep = "/"))
source(paste(code.dir, "boxplotAllPerc.R", sep = "/"))
source(paste(code.dir, "rateOfChange.R", sep = "/"))
source(paste(code.dir, "isBestFit.R", sep = "/"))
source(paste(code.dir, "isRateofChange.R", sep = "/"))
source(paste(code.dir, "varDecomp.R", sep = "/"))

##########################################################################################
######################### Initiating library packages
##########################################################################################

# install.packages("corrgram"); 	library(corrgram)
# install.packages("perturb") ;   library(perturb)
# install.packages("mctest") ;  	library(mctest)
# install.packages("car");      	library(car)
# install.packages("readr");      library(readr)
save.image()

##########################################################################################
######################### Initiating the parameters and data variable description 
##########################################################################################

# Note: this dataset is also posted on mcperturb githup web page:
# That is, Data.zip at https://github.com/ss355/mcperturb

# Reading in the data
body_dat = read.table("http://jse.amstat.org/datasets/body.dat.txt", header=F) 
dim( body_dat) # $ 507 * 25

# Response variable: weight in kg
y = body_dat[,23]

# X-matrix
x = body_dat[,c(10, 11, 16, 17, 21, 22, 24)]
colnames(x) = c("shoulder", "chest", "bicep", "forearm", "wrist", "age", "height")

fullx = body_dat[,c(1:22,24)]
colnames(fullx) = c("bicromial", "biiliac", "bitrochanteric", "chestDepth",
                    "chestDim", "elbowDim", "wristDim", "kneeDim", "ankleDim", 
                    "shoulderDim", "chestGir", "waistGir", "navelGir", "hipGir", 
                    "thighGir", "bicepGir", "forearmGir", "kneeGir", "calfGir", 
                    "ankleGir", "wistGir", "age", "height")
                    
special.Var = c("shoulder")

# Note for the user: It is better to set up the noise level once and do not change
# In order to generate reproducible results, please use "set.seed" to set up a random seed. 

# Making the noiselevels
noiseStart.2 = 0.05
noiseEnd.2 = 0.5
noiseSteps.2 = 0.05
NL2.50p10levs = seq(noiseStart.2, noiseEnd.2, by = noiseSteps.2) 
NL2.50p10levs

#  > NL2.50p10levs  
# #  [1] 0.05 0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50
#  > length(NL2.50p10levs)  # [1] 10

# The amount of iterations at each noise level
iteration = 50

# > head(x)
#   shoulder chest bicep forearm wrist age height
# 1    106.2  89.5  32.5    26.0  16.5  21  174.0
# 2    110.5  97.0  34.4    28.0  17.0  23  175.3
# 3    115.1  97.5  33.4    28.8  16.9  28  193.5

save.image()

# ------------------------------------------------------------
# Variable descrition: http://jse.amstat.org/datasets/body.txt
# NAME: Exploring Relationships in Body Dimensions
# TYPE: Observational
# SIZE: 507 Observations, 25 Variables

# DESCRIPTIVE ABSTRACT:
# This dataset contains 21 body dimension measurements as well as age, 
# weight, height, and gender on 507 individuals. The 247 men and 260 
# women were primarily individuals in their twenties and thirties, with a 
# scattering of older men and women, all exercising several hours a week.

# VARIABLE DESCRIPTIONS:
# Columns     Variable
#             Skeletal Measurements:
# column 1:    1 -  4   Biacromial diameter (see Fig. 2) 
# column 2:    6 -  9   Biiliac diameter, or "pelvic breadth" (see Fig. 2)
# column 3:   11 - 14   Bitrochanteric diameter (see Fig. 2)
# column 4:   16 - 19   Chest depth between spine and sternum at nipple level, mid-expiration
# column 5:   21 - 24   Chest diameter at nipple level, mid-expiration
# column 6:   26 - 29   Elbow diameter, sum of two elbows
# column 7:   31 - 34   Wrist diameter, sum of two wrists
# column 8:   36 - 39   Knee diameter, sum of two knees
# column 9:   41 - 44   Ankle diameter, sum of two ankles
#            Girth Measurements:
# column 10:  46 - 50   Shoulder girth over deltoid muscles
# column 11:  52 - 56   Chest girth, nipple line in males and just above breast tissue in females, mid-expiration
# column 12:  58 - 62   Waist girth, narrowest part of torso below the rib cage,  average of contracted and relaxed position
# column 13:  64 - 68   Navel (or "Abdominal") girth at umbilicus and iliac crest, iliac crest as a landmark
# column 14:  70 - 74   Hip girth at level of bitrochanteric diameter
# column 15:  76 - 79   Thigh girth below gluteal fold, average of right and left girths
# column 16:  81 - 84   Bicep girth, flexed, average of right and left girths
# column 17:  86 - 89   Forearm girth, extended, palm up, average of right and left girths
# column 18:  91 - 94   Knee girth over patella, slightly flexed position, average of right and left girths
# column 19:  96 - 99   Calf maximum girth, average of right and left girths
# column 20: 101 -104   Ankle minimum girth, average of right and left girths
# column 21: 106 -109   Wrist minimum girth, average of right and left girths
#           Other Measurements:
# column 22: 111-114    Age (years)
# column 23: 116-120    Weight (kg)
# column 24: 122-126    Height (cm)
# column 25: 128        Gender (1 - male, 0 - female)

# The first 21 variables are all measured in centimeters (cm).
# Values are separated by blanks. There are no missing values.
# ------------------------------------------------------------

##########################################################################################
######################### Performing the Perturb function analysis
##########################################################################################

data(consumption)
attach(consumption)
ct1 <- with(consumption, c(NA,cons[-length(cons)]))
C_regmod <- lm(cons~ct1+dpi+rate+d_dpi,data = consumption)

# Supp Table 1
sink(paste(figure.table.dir,"sup_Table1.for.perturb.txt",sep="/"))
summary(C_regmod)
sink()

# Mar 30, 2020 Note: should set up a random seed before calling perturb. 
# Supp Figure 1

set.seed(6677) # random seed added on Mar 30, 2020

perturb1 <- perturb(C_regmod, pvars = c("dpi","rate", "d_dpi"), prange=c(1,1,1))
pdf(paste(figure.table.dir, "sup_figure1.for.perturb.pdf", sep = "/"), onefile=T, width=11, height=11)
boxplot(perturb1$coeff.table,  ylab = "Coefficients", xlab = "Regressors")
dev.off()

# Supp Table 2
sink(paste(figure.table.dir,"sup_Table2.for.perturb.txt", sep="/"))
cd<-colldiag(C_regmod)
print(cd,fuzz=.3)
sink()

# Note: in the above "paste" command, without 'sep="/"', the figures and tables 
# will be generated with the name like "Figure.Tables sup_Table1.for.perturb.txt". 

##########################################################################################
######################### Performing the mctest analysis
##########################################################################################

data(Hald)
x1<-Hald[,-1]
y1<-Hald[,1]

# Supp Table 3
# Overall diagnostic measures and eigenvalues without intercept term

set.seed(6677)
sink(paste(figure.table.dir,"sup_Table3.for.mctest.txt", sep = "/"))
mctest(x1, y1, type="o", Inter=FALSE)
sink()

# Supp Table 4
# sink(paste(figure.table.dir,"sup_Table5.for.mctest.txt", sep = "/"))
sink(paste(figure.table.dir,"sup_Table4.for.mctest.txt", sep = "/"))
eigprop(x1)
sink()

# Supp Table 5
# all individual diagnostic measures
# sink(paste(figure.table.dir,"sup_Table4.for.mctest.txt", sep = "/"))
sink(paste(figure.table.dir,"sup_Table5.for.mctest.txt", sep = "/"))
mctest(x1, y1, type="i")
sink()

# Figure 2
pdf(paste(figure.table.dir, "sup_figure2.for.mctest.pdf", sep = "/"), onefile=T, width=11, height=11)
mc.plot(x1, y1)
dev.off()

##########################################################################################
######################### Summary Statistics for the x matrix
##########################################################################################
# For Table 4 of main text 

sink(paste(figure.table.dir,"Table4.txt", sep = "/"))
summary(x)
sink()

##########################################################################################
######################### Calculating the parwise correlation matrix
##########################################################################################

# Main Text Figure 1
pdf(paste(figure.table.dir, "figure1.pdf", sep = "/"), onefile=T, width=11, height=11)
corrgram(x=x, 
         lower.panel=panel.shade, 
         upper.panel=panel.cor, 
         col.regions = colorRampPalette(c("orange","yellow", "green", "blue", "black")))
dev.off()

##########################################################################################
######################### Calling the density plots function
########################################################################################## 

# Main Text Figure 2
pdf(paste(figure.table.dir, "figure2.pdf", sep = "/"), onefile=T, width=11, height=11)
densPlots(x, meanCent = T)
dev.off()

# Main Text Figure 3
pdf(paste(figure.table.dir, "figure3.pdf", sep = "/"), onefile=T, width=11, height=11)
densPlots(x[,1:2], meanCent = T)
dev.off()

# For fucntion documentation 
pdf(paste("densPlots.pdf", sep = "/"), onefile=T, width=11, height=11)
densPlots(x, meanCent = T)
densPlots(x[,1:2], meanCent = T)
dev.off()

##########################################################################################
#########################  Calling the implausestats function
##########################################################################################

# main text Table 5
set.seed(6677)
sink(paste(figure.table.dir,"Table5.txt", sep = "/"))
implausStats(xmat = x, response = y)
sink()

##########################################################################################
######################### Calling the rsqdPlots function
##########################################################################################

# Main Text Figure 4
set.seed(6677)
pdf(paste(figure.table.dir, "figure4.pdf", sep = "/"), onefile=T, width=11, height=11)
rsqdPlots(x,y)
rsqdPlots(x,y,T)
dev.off()

##########################################################################################
######################### Generating Overall Diagnostics plots
##########################################################################################

# Main Text Figure 5
# Note, need to make sure the special variale ("special.Var") is well set up. 

date()
set.seed(6677)
summaryresults = diagout(xmat=x, response = y, noiseLevels =NL2.50p10levs, special.Var, iteration)
diagnames = names(summaryresults)
var = list()
diagMat = list()

over = omcdiag(x, y)$odiags
date()
# > omcdiag(x, y)$odiags
#                            results detection
# Determinant           1.582362e-04         1
# Farrar Chi-Square     4.400506e+03         1
# Red Indicator         6.953597e-01         1
# sum of Lambda Invers  5.528373e+01         1
# Theil Indicator      -5.832893e-02         0
# Condition Number      1.207045e+02         1

# overallDiagsDiffs = function(xmat = x, y = y, special.Vars = special.Var, 
# noiseLevs = NL.0, # iteration = iteration, mainLev = length(NL.0) )

##############################################
# sub_figure3 and figure 5
# mainLev = 1 when calling "overallDiagsDiffs"
##############################################

# It takes about 4 minutes to draw to sup_figure3
date()
set.seed(6677)
h.7var = c("a.", "b.", "c.", "d.", "e.", "f.", "g.")
pdf(paste(figure.table.dir, "sup_figure3.", special.Var, ".deter7var.50p10levs.mainLev1.pdf", sep=""), onefile=T, width=17, height=17)
par(cex.main = 2.5, cex.lab=2, cex.axis=1.5, mfrow=c(4,2), mar = c(4.6, 5.1, 3.1, 2.1))
for(k in 1:7)
{ 
  overdiag.7var  = overallDiagsDiffs(x, y, colnames(x)[k], noiseLevs = NL2.50p10levs, iteration, mainLev = 1)
  boxplot(overdiag.7var[[2]], ylim = c(min(overdiag.7var[[2]], over[1, 1]), max(overdiag.7var[[2]],over[1, 1])),
          main = paste(h.7var[k], as.character(colnames(x)[k])),
          xlab = "Noise Levels %", ylab = "Determinant")
  abline(h = over[1, 1], lwd=4)  
}
dev.off()
date()

# Note, need to make sure the special variale ("special.Var") is well set up. 

set.seed(6677)
h.4var = c("a.", "b.", "c.", "d.")
pdf(paste(figure.table.dir, "figure5.", special.Var, ".deter4var.50p10levs.mainLev1.pdf", sep=""), onefile=T, width=17, height=17)
par(cex.main = 2.5, cex.lab=2, cex.axis=1.5, mfrow=c(2,2), mar = c(4.6, 5.1, 3.1, 2.1))
for (k in 1:4)
{ 
  overdiag.4var  = overallDiagsDiffs(x, y, colnames(x)[k], noiseLevs = NL2.50p10levs, iteration, mainLev = 1)
  boxplot(overdiag.4var[[2]], ylim = c(min(overdiag.4var[[2]], over[1, 1]), max(overdiag.4var[[2]],over[1, 1])), main = paste(h.4var[k], as.character(colnames(x)[k])),
          xlab = "Noise Levels %", ylab = "Determinant")
  abline(h = over[1, 1], lwd=4)  
}
dev.off()
date()

# In the folowing code, "over[1, 1]" is 1.582362e-04. It is the original determant value. 

##############################################
# sub_figure3 and figure 5
# mainLev =  when calling "overallDiagsDiffs"
##############################################

date()
set.seed(6677)
h.7var = c("a.", "b.", "c.", "d.", "e.", "f.", "g.")
pdf(paste(figure.table.dir, "sup_figure3.", special.Var, ".deter7var.50p10levs.mainLev10.pdf", sep=""), onefile=T, width=17, height=17)
par(cex.main = 2.5, cex.lab=2, cex.axis=1.5, mfrow=c(4,2), mar = c(4.6, 5.1, 3.1, 2.1))
for(k in 1:7)
{ 
  overdiag.7var  = overallDiagsDiffs(x, y, colnames(x)[k], noiseLevs = NL2.50p10levs, iteration, mainLev = length(NL2.50p10levs))
  boxplot(overdiag.7var[[2]], ylim = c(min(overdiag.7var[[2]], over[1, 1]), max(overdiag.7var[[2]],over[1, 1])),
          main = paste(h.7var[k], as.character(colnames(x)[k])),
          xlab = "Noise Levels", ylab = "Determinant")
  abline(h = over[1, 1], lwd=4)  
}
dev.off()
date()

# Note, need to make sure the special variale ("special.Var") is well set up. 
date()
set.seed(6677)
h.4var = c("a.", "b.", "c.", "d.")
pdf(paste(figure.table.dir, "figure5.", special.Var, ".deter4var.50p10levs.mainLev10.pdf", sep=""), onefile=T, width=17, height=17)
par(cex.main = 2.5, cex.lab=2, cex.axis=1.5, mfrow=c(2,2), mar = c(4.6, 5.1, 3.1, 2.1))
for (k in 1:4)
{ 
  overdiag.4var  = overallDiagsDiffs(x, y, colnames(x)[k], noiseLevs= NL2.50p10levs, iteration, mainLev = length(NL2.50p10levs))
  boxplot(overdiag.4var[[2]], ylim = c(min(overdiag.4var[[2]], over[1, 1]), max(overdiag.4var[[2]],over[1, 1])),  main = paste(h.4var[k], as.character(colnames(x)[k])), xlab = "Noise Levels", ylab = "Determinant")
  abline(h = over[1, 1], lwd=4)  
}
dev.off()
date()

##########################################################################################
######################### Generate Boxplots all percentage function
##########################################################################################
# Main Text Figure 6

set.seed(6677)
# summaryresults = diagout(x, y, noiseLevs, special.Var, iteration)
diagnames = names(summaryresults)
var = list()
diagMat = list()

# Note: I do not need to regenerate "summaryresutls" using diagout, 
# but I still need to reset up var, diagmat. 

h.7var = c("a.", "b.", "c.", "d.", "e.", "f.", "g.")

pdf(paste(figure.table.dir, "sup_figure4.", special.Var, ".AllPerc.50p10levs.7var.pdf", sep=""), onefile=T, width=17, height=17)
par(cex.main = 2.5, cex.lab=2,cex.axis=1.5,mfrow=c(4,2),mar = c(4.6, 5.1, 3.1, 2.1))
for(k in 1:7)
{
  # J=4 is the VIF list
  j=4
  for(i in 1:length(NL2.50p10levs))
  {  # should go through a sequence of noise levels 10  
     var[[i]] = summaryresults[[j]][[i]][k]
  }
  diagMat[[k]] = var
  boxplot(as.data.frame(diagMat[[k]]), main = paste(h.7var[k], as.character(colnames(x)[k])), 
          xlab = paste("Noise Level %"), ylab = paste(as.character(colnames(x)[k]),diagnames[[j]]), names = NL2.50p10levs*100)
}
dev.off()


h.4var = c("a.", "b.", "c.", "d.")
pdf(paste(figure.table.dir, "figure6.", special.Var, ".AllPerc.50p10levs.4var.pdf", sep=""), onefile=T, width=17, height=17)
par(cex.main = 2.5, cex.lab=2,cex.axis=1.5,mfrow=c(2,2),mar = c(4.6, 5.1, 3.1, 2.1))
for(k in 1:4)
{
  # J=4 is the VIF list
  j=4
  for(i in 1:length(NL2.50p10levs))
  {# should go through a sequence of noise levels 10  
    var[[i]] = summaryresults[[j]][[i]][k]
  }
  diagMat[[k]] = var
  boxplot(as.data.frame(diagMat[[k]]), main = paste(h.4var[k], as.character(colnames(x)[k])), 
          xlab = paste("Noise Level %"), ylab = paste(as.character(colnames(x)[k]),diagnames[[j]]), names = NL2.50p10levs*100)
}
dev.off()

# This is used for the funtion documentation
# BoxplotAllPerc(xmatrix = x, y = y, noiseLevs = noiseLevs, special.Vars = special.Var, iteration = iteration)


##########################################################################################
######################### Generating Boxplots all variables function
##########################################################################################
# Main Text Figure 7

set.seed(6677)
# summaryresults = diagout(x, y, noiseLevs, special.Var, iteration)
diagnames = names(summaryresults)

h.10 = c("a.", "b.", "c.", "d.", "e.", "f.", "g.","h.","i.","j.")

pdf(paste(figure.table.dir, "sup_figure5.", special.Var, ".AllVars.50p10levs.pdf", sep=""), onefile=T, width=17, height=17)
par(cex.main = 2.5, cex.lab=2,cex.axis=1.5,mfrow=c(5,2),mar = c(4.6, 5.1, 3.1, 2.1))
j=4
# for(i in 1:10)
for (i in 1:length(NL2.50p10levs))
{ 
  boxplot(summaryresults[[j]][[i]], 
          main = paste(h.10[i],as.character(NL2.50p10levs[i]*100),"%"),  
          xlab = paste("Variables"), ylab = paste(diagnames[j]), names = names(x))
}
dev.off()

set.seed(6677)

h.4 = c("a.", "b.", "c.", "d.")

pdf(paste(figure.table.dir, "figure7.", special.Var, ".AllVars.50p10levs.pdf", sep=""), onefile=T, width=17, height=17)
par(cex.main = 2.5, cex.lab=2,cex.axis=1.5,mfrow=c(2,2),mar = c(4.6, 5.1, 3.1, 2.1))
j=4
for(i in 1:4)
{ 
  boxplot(summaryresults[[j]][[i]], 
          main = paste(h.4[i],as.character(NL2.50p10levs[i]*100),"%"),  
          xlab = paste("Variables"), ylab = paste(diagnames[j]), names = names(x))
}
dev.off()


# THis is used for funtion documentaion
# boxplotsAllVars(xmat = x, y = y, noiseLevs = noiseLevs, special.Vars = special.Var, iteration = iteration)
# noiseLevelDiagOutList(x = x, y = y, special.Vars = special.Var, noiseLevels = noiseLevs, iter = iteration)

##########################################################################################
######################### Calling OverallDiagsOut 
##########################################################################################
# main text Table 6

date()
set.seed(6677)
ov = overallDiagsOut(x = x, y = y, noiseLevs = NL2.50p10levs, iteration = iteration)
colnames(ov[[1]]) = c("Noise.Var", "Min.Mean", "Max.Mean", "Difference")
date()
Table6.50p10levs<-ov[[1]]
sink(paste(figure.table.dir,"Table6.50p10levs.txt", sep = "/"))
Table6.50p10levs
sink(); date()

# Table 6 is as shown below:
     Noise.Var  Min.Mean               Max.Mean              
[1,] "shoulder" "0.000158236233098198" "0.000235173346985677"
[2,] "chest"    "0.000158236233098198" "0.000236806899701762"
[3,] "bicep"    "0.000158236233098198" "0.000259614876904143"
[4,] "forearm"  "0.000158236233098198" "0.000282737490225862"
[5,] "wrist"    "0.000158236233098198" "0.000206093197084801"
[6,] "age"      "0.000158236233098198" "0.000159535453137921"
[7,] "height"   "0.000158236233098198" "0.000168406039960643"
     Difference            
[1,] "7.6937113887479e-05" 
[2,] "7.85706666035634e-05"
[3,] "0.000101378643805944"
[4,] "0.000124501257127663"
[5,] "4.78569639866023e-05"
[6,] "1.29922003972216e-06"
[7,] "1.01698068624441e-05"

# Next I prepare Table 6 to be included in the latex version of the manuscript 
options(digits=10)
table6.v1<-matrix( c(round(as.numeric(Table6.50p10levs[, 2:4]), digits=7)), byrow=F, nrow=7) 
table6.latex<-cbind(Table6.50p10levs[, 1], table6.v1 , rep("\\\\", dim(ov[[1]])[1]))
colnames(table6.latex)<-c("Noise.Var", "Min.Mean", "Max.Mean", "Difference", "seperation")
write.table(table6.latex, "Table6.latex.txt", quote=F, col.names=T, row.names=F, sep="  &  " )
# Then I should manually change "  &   \\" to "\\". 

> table6.v2
> table6.v2
     Noise.Var  Min.Mean   Max.Mean   Difference seperation
[1,] "shoulder" "0.000158" "0.000235" "7.7e-05"  "\\\\"
[2,] "chest"    "0.000158" "0.000237" "7.9e-05"  "\\\\"
[3,] "bicep"    "0.000158" "0.00026"  "0.000101" "\\\\"
[4,] "forearm"  "0.000158" "0.000283" "0.000125" "\\\\"
[5,] "wrist"    "0.000158" "0.000206" "4.8e-05"  "\\\\"
[6,] "age"      "0.000158" "0.00016"  "1e-06"    "\\\\"
[7,] "height"   "0.000158" "0.000168" "1e-05"    "\\\\"

##########################################################################################
######################### Calling rateOfChange 
##########################################################################################
# main Text Table 7

date()
set.seed(6677)
summaryTableList = rateOfChange(x = x, y = y, noiseLevs = NL2.50p10levs, special.Vars = special.Var, iteration = iteration)
Table7.50p10levs<-summaryTableList$`VIF.Table`
sink(paste(figure.table.dir,"Table7.50p10levs.txt", sep = "/"))
Table7.50p10levs 
sink();
date()

# Next I prepare Table 7 to be included in the latex version of the manuscript 

table7.latex<-cbind(rownames(Table7.50p10levs), Table7.50p10levs , rep("\\\\", dim(Table7.50p10levs)[1]))
write.table(table7.latex, "Table7.latex.txt", quote=F, col.names=F, row.names=F, sep="  &  " )

##########################################################################################
######################### Calling overallDiagsRank 
##########################################################################################
# main Text Table 8

date()
set.seed(6677)
# Ranks of mainLev 1
Table8.50p10levs.mainLev1 = overallDiagsRank(xmat = x, resp = y, noiseL = NL2.50p10levs, itera = iteration, mainLev=1)
sink(paste(figure.table.dir,"Table8.50p10levs.mainLev1.txt", sep = "/"))
Table8.50p10levs.mainLev1
sink()
date()

date()
set.seed(6677)
# Ranks of mainLev 10
Table8.50p10levs.mainLev10 = overallDiagsRank(xmat = x, resp = y, noiseL = NL2.50p10levs, itera = iteration, mainLev=length(NL2.50p10levs))
sink(paste(figure.table.dir,"Table8.50p10levs.mainLev10.txt", sep = "/"))
Table8.50p10levs.mainLev10
sink()
date()

# Next I prepare Table 8 mainLev=10 to be included in the latex version of the manuscript 

table8.latex<-cbind( Table8.50p10levs.mainLev10, rep("\\\\", dim(Table8.50p10levs.mainLev10)[1]))
write.table(table8.latex, "Table8.latex.txt", quote=F, col.names=F, row.names=F, sep="  &  " )

##########################################################################################
######################### Calling isBestfit 
##########################################################################################
# Main text Table 9

date()
set.seed(6677)
bestfitvalues = isBestFit(x.mat = x, y = y, noiseLevs = NL2.50p10levs, special.Vars = special.Var, iteration = iteration)
Table9.50p10levs = bestfitvalues[[1]] #  vifBestFit
sink(paste(figure.table.dir,"Table9.50p10levs.txt", sep = "/"))
Table9.50p10levs 
sink()
date()

# Next I prepare Table 9 to be included in the latex version of the manuscript 

table9.latex<-cbind(rownames(Table9.50p10levs), Table9.50p10levs , rep("\\\\", dim(Table9.50p10levs)[1]))
write.table(table9.latex, "Table9.latex.txt", quote=F, col.names=F, row.names=F, sep="  &  " )

##########################################################################################
######################### Calling isRateofChange
##########################################################################################
# Main text Table 10
date()
set.seed(6677)
rateofchangevalues = isRateofChange(x_mat = x, y = y, noiseLevs = NL2.50p10levs, special.Vars = special.Var, iteration = iteration)
Table10.50p10levs = rateofchangevalues[[1]]  # vifRateofChange
sink(paste(figure.table.dir,"Table10.50p10levs.txt", sep = "/"))
Table10.50p10levs
sink()
date()


# Next I prepare Table 10 to be included in the latex version of the manuscript 

table10.latex<-cbind(rownames(Table10.50p10levs), Table10.50p10levs , rep("\\\\", dim(Table10.50p10levs)[1]))
write.table(table10.latex, "Table10.latex.txt", quote=F, col.names=F, row.names=F, sep="  &  " )


##########################################################################################
######################### Calling isBestFit, isRateofChange
##########################################################################################

# Supp Table 6
set.seed(6677); date();      
bestfitvaluesAll = isBestFit(x.mat = fullx, y = y, noiseLevs = NL2.50p10levs, special.Vars = special.Var, iteration = iteration)
sup_Table6.50p10levs = bestfitvaluesAll[[1]]  # vifBestFitALL
sink(paste(figure.table.dir,"sup_Table6.50p10levs.txt", sep = "/"))
sup_Table6.50p10levs
sink(); date()

# Next I prepare sup_Table6 to be included in the latex version of the manuscript 
# I will split this table to 2 parts: "a" for col 1-11, and "b" for col 12-23.
# Part a:
sup_Table6.a.latex<-cbind(rownames(sup_Table6.50p10levs), sup_Table6.50p10levs[, 1:11] , rep("\\\\", dim(sup_Table6.50p10levs)[1]))
write.table(sup_Table6.a.latex, "sup_Table6a.latex.txt", quote=F, col.names=F, row.names=F, sep="  &  " )
# Part b:
sup_Table6.b.latex<-cbind(sup_Table6.50p10levs[, 12:23] , rep("\\\\", dim(sup_Table6.50p10levs)[1]))
write.table(sup_Table6.b.latex, "sup_Table6b.latex.txt", quote=F, col.names=F, row.names=F, sep="  &  " )

# Supp Table 7
set.seed(6677); date(); 
rateofchangevaluesALL = isRateofChange(x_mat = fullx, y = y, noiseLevs = NL2.50p10levs, special.Vars = special.Var, iteration = iteration)
sup_Table7.50p10levs = rateofchangevaluesALL[[1]]  # vifRateofChangeALL 
sink(paste(figure.table.dir,"sup_Table7.50p10levs.txt", sep = "/"))
sup_Table7.50p10levs
sink(); date()

# Next I prepare sup_Table7 to be included in the latex version of the manuscript 
# I will split this table to 2 parts: "a" for col 1-11, and "b" for col 12-23.
# Part a:
sup_Table7.a.latex<-cbind(rownames(sup_Table7.50p10levs), sup_Table7.50p10levs[, 1:11] , rep("\\\\", dim(sup_Table7.50p10levs)[1]))
write.table(sup_Table7.a.latex, "sup_Table7a.latex.txt", quote=F, col.names=F, row.names=F, sep="  &  " )
# Part b:
sup_Table7.b.latex<-cbind(sup_Table7.50p10levs[, 12:23] , rep("\\\\", dim(sup_Table7.50p10levs)[1]))
write.table(sup_Table7.b.latex, "sup_Table7b.latex.txt", quote=F, col.names=F, row.names=F, sep="  &  " )

##########################################################################################
######################### Perfroming foward stepwise AIC variable selection
##########################################################################################
# Main text Table 11
set.seed(6677)
fullMat = cbind(x,y)
null = lm(y~1,data = fullMat)
fullMod = lm(y~., data = fullMat)
sink(paste(figure.table.dir,"sup_Table8.50p10levs.txt", sep = "/"))
summary(step(null, scope=list(lower=null, upper=fullMod), direction="forward")) 
sink()