#Project: Longitudinal association between body dissatisfaction and mental health outcomes using TEDS
#Author: Ilaria Costantini 
#Association: BD and ED using ACE/AE/E/ADE models
#This script was adapted from the template scripts available here: https://hermine-maes.squarespace.com/onea5/ 

# Make this work reproducible
set.seed(7)

# Load Libraries & Options
rm(list=ls())

setwd("<local_path>/data/")

#call libraries
source('https://vipbg.vcu.edu/vipbg/OpenMx2/software/getOpenMx.R')
require(OpenMx)
mxOption(NULL,"Default optimizer","NPSOL")
library(OpenMx)
library(psych); library(polycor)
library(haven)
library(ggplot2)
library(tidyr)
library(dplyr)
library(mets)
library(patchwork)
library(kableExtra)
library(foreign)
library(summarytools)
library(polycor)
library(moments)
library(psych)
library(bestNormalize)
library(lsr)

source("miFunctions.R")

# Create Output 
filename    <- "two_sat_ace_bdED_July25"
sink(paste(filename,".Ro",sep=""), append=FALSE, split=TRUE)

# ----------------------------------------------------------------------------------------------------------------------
# PREPARE DATA

# Load Data
twinData<-read_dta("data.dta")
dim(twinData)
describe(twinData[,1:71], skew=F)

# Select Variables for Analysis
vars      <- c('bodydissatisf16', 'edscale21_full')    # list of variables names
nv        <- 2                                           # number of variables
ntv       <- nv*2                                        # number of total variables
selVars   <- paste(vars,c(rep(1,nv),rep(2,nv)),sep="")   # This code creates two new variable names by appending 1 and 2 to the original variable name and stores them in the variable selVars.

# Select Data for Analysis
dzData    <- subset(twinData, MZ==0, selVars)  # DZ
mzData    <- subset(twinData, MZ==1, selVars)  # MZ 

# Replace missing values in age16, age211 with values from age212, and vice versa
twinData$age161 <- ifelse(is.na(twinData$age161), twinData$age162, twinData$age161)
twinData$age162 <- ifelse(is.na(twinData$age162), twinData$age161, twinData$age162)
twinData$age211 <- ifelse(is.na(twinData$age211), twinData$age212, twinData$age211)
twinData$age212 <- ifelse(is.na(twinData$age212), twinData$age211, twinData$age212)

# Regress out age and sex for body dissatisfaction
twinData$bodydissatisf161_reg <- resid(lm(data = twinData, bodydissatisf161 ~ age161 + sex1, na.action = na.exclude))
twinData$bodydissatisf162_reg <- resid(lm(data = twinData, bodydissatisf162 ~ age162 + sex2, na.action = na.exclude))

# Regress out age and sex for eating disorders
twinData$eatingdisorder211_reg <- resid(lm(data = twinData, edscale21_full1 ~ age211 + sex1, na.action = na.exclude))
twinData$eatingdisorder212_reg <- resid(lm(data = twinData, edscale21_full2 ~ age212 + sex2, na.action = na.exclude))

hist(twinData$bodydissatisf161_reg)
hist(twinData$bodydissatisf162_reg)
hist(twinData$eatingdisorder211_reg)
hist(twinData$eatingdisorder212_reg)

# Assess skewness
transform_variables <- c("bodydissatisf161_reg", "eatingdisorder211_reg", "bodydissatisf162_reg", "eatingdisorder212_reg")

twinData %>%
  select(transform_variables) %>%
  skewness(., na.rm = T)

# All variables are <1.0 so do not need to be transformed
selVars <- transform_variables

# Select Data for Analysis - Whole sample
dzData    <- subset(twinData, MZ==0, selVars)  # DZ 
mzData    <- subset(twinData, MZ==1, selVars)  # MZ 


#count those with at least both twins with BD and either twin with one outcome
eligible <- rowSums(!is.na(twinData[, c("bodydissatisf161_reg","bodydissatisf162_reg")])) == 2 &
  rowSums(!is.na(twinData[, c("eatingdisorder211_reg","eatingdisorder212_reg")])) > 0

#count
sum(eligible) #827 twin pairs

#count those with at least both twins with BD and each twin with at least one outcome
eligiblebded <- rowSums(!is.na(twinData[, c("bodydissatisf161_reg","bodydissatisf162_reg")])) == 2 &
  (rowSums(!is.na(twinData[, c("eatingdisorder211_reg")])) > 0) &
  (rowSums(!is.na(twinData[, c("eatingdisorder212_reg")])) > 0)

#count
sum(eligiblebded) #626 twin pairs

# Generate Descriptive Statistics
colMeans(mzData,na.rm=TRUE)
colMeans(dzData,na.rm=TRUE)
cov(mzData,use="complete")
cov(dzData,use="complete")
(rMZ<-cor(mzData,use="complete"))
(rDZ<-cor(dzData,use="complete"))

# Corrected Saturated and Constrained Models for Body Dissatisfaction and Eating Disorder

# Set Starting Values
svMe <- c(-0.02, -0.01)  # starting means for each trait
temp_cov <- cov(rbind(mzData, dzData), use = "complete")
svVa <- diag(temp_cov)[1:nv]  # starting variances using pooled empirical estimates
lbVa <- .0001  # lower bound for variances

# Generate labels
labMeMZ <- labVars("meanMZ", selVars)
labMeDZ <- labVars("meanDZ", selVars)
labMeZ  <- labVars("meanZ", selVars)
labCvMZ <- labLower("covMZ", ntv)
labCvDZ <- labLower("covDZ", ntv)
labCvZ  <- labLower("covZ", ntv)
labVaMZ <- labDiag("covMZ", ntv)
labVaDZ <- labDiag("covDZ", ntv)
labVaZ  <- labDiag("covZ", ntv)

# MZ and DZ mean models
meanMZ <- mxMatrix("Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels=labMeMZ, name="meanMZ")
meanDZ <- mxMatrix("Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels=labMeDZ, name="meanDZ")

# MZ and DZ covariance matrices
covMZ <- mxMatrix("Symm", nrow=ntv, ncol=ntv, free=TRUE, 
                  values=valDiag(svVa, ntv), lbound=valDiag(lbVa, ntv),
                  labels=labCvMZ, name="covMZ")
covDZ <- mxMatrix("Symm", nrow=ntv, ncol=ntv, free=TRUE, 
                  values=valDiag(svVa, ntv), lbound=valDiag(lbVa, ntv),
                  labels=labCvDZ, name="covDZ")

# Data and expectations
expMZ <- mxExpectationNormal(covariance="covMZ", means="meanMZ", dimnames=selVars)
expDZ <- mxExpectationNormal(covariance="covDZ", means="meanDZ", dimnames=selVars)
dataMZ <- mxData(observed=mzData, type="raw")
dataDZ <- mxData(observed=dzData, type="raw")
funML <- mxFitFunctionML()

# Model building
modelMZ <- mxModel(meanMZ, covMZ, dataMZ, expMZ, funML, name="MZ")
modelDZ <- mxModel(meanDZ, covDZ, dataDZ, expDZ, funML, name="DZ")
multi <- mxFitFunctionMultigroup(c("MZ", "DZ"))
ciCov <- mxCI(c("MZ.covMZ", "DZ.covDZ"))
ciMean <- mxCI(c("MZ.meanMZ", "DZ.meanDZ"))

# Final model
modelSAT <- mxModel("twoSATc", modelMZ, modelDZ, multi, ciCov, ciMean)
fitSAT <- mxTryHard(modelSAT, intervals = TRUE)
sumSAT <- summary(fitSAT, verbose = TRUE)
fitGofs(fitSAT)
fitEsts(fitSAT)
#number of observations included in the model 
fitSAT$MZ$data@numObs
fitSAT$DZ$data@numObs

# Equality Constrained Models
# Constrain expected Means to be equal across Twin Order
modelEMO <- mxModel( fitSAT, name="twoEMOc" )
for (i in 1:nv) {
  modelEMO <- omxSetParameters( modelEMO, label=c(labMeMZ[nv+i],labMeMZ[i]), free=TRUE, values=svMe[i], newlabels=labMeMZ[i] )
  modelEMO <- omxSetParameters( modelEMO, label=c(labMeDZ[nv+i],labMeDZ[i]), free=TRUE, values=svMe[i], newlabels=labMeDZ[i] ) }
fitEMO <- mxTryHard( modelEMO, intervals=T)
fitGofs(fitEMO); fitEsts(fitEMO)

# Constrain expected Means and Variances to be equal across Twin Order
modelEMVO <- mxModel( fitEMO, name="twoEMVOc" )
for (i in 1:nv) {
  modelEMVO <- omxSetParameters( modelEMVO, label=c(labVaMZ[nv+i],labVaMZ[i]), free=TRUE, values=svVa[i], newlabels=labVaMZ[i] )
  modelEMVO <- omxSetParameters( modelEMVO, label=c(labVaDZ[nv+i],labVaDZ[i]), free=TRUE, values=svVa[i], newlabels=labVaDZ[i] ) }
fitEMVO <- mxTryHard( modelEMVO, intervals=F)
fitGofs(fitEMVO); fitEsts(fitEMVO)

# Because the model fails, run some diagnostics: 
expcovs <- mxGetExpected(modelEMVO, 'covariance')
expcovs
#get eigenvalues and inspect to verify positive definiteness.  Any negative eigenvalues shows a problem with the covariances.  Also, the diagonals of the matrix should approximate or be a bit larger than the observed variances.
eigen(expcovs$MZ)

#get model-implied means
expmeans <- mxGetExpected(modelEMO, 'means')
expmeans
#inspect means and make sure they are within 1sd or so of the true mean.  The sd in question is the square root of the diagonal element of the model-implied covariance corresponding to that variable (they are in the same order).  Change the values for the means if not roughly same as observed.

# Constrain expected Means and Variances to be equal across Twin Order and Zygosity
modelEMVZ <- mxModel( fitEMVO, name="twoEMVZc" )
for (i in 1:nv) {
  modelEMVZ <- omxSetParameters( modelEMVZ, label=c(labMeMZ[i],labMeDZ[i]), free=TRUE, values=svMe[i], newlabels=labMeZ[i] )
  modelEMVZ <- omxSetParameters( modelEMVZ, label=c(labVaMZ[i],labVaDZ[i]), free=TRUE, values=svVa[i], newlabels=labVaZ[i] ) }
fitEMVZ <- mxTryHard( modelEMVZ, intervals=T)
fitGofs(fitEMVZ); fitEsts(fitEMVZ)
# Print Comparative Fit Statistics
(Compare_SAT_bd_edSA<-mxCompare( fitSAT, subs <- list(fitEMO, fitEMVO, fitEMVZ) ))
write.csv(Compare_SAT_bd_edSA, "Compare_SAT_bd_edSA.csv", row.names = FALSE)

#===============================================================================================================================================     
#                                        PRIMARY ANALYSIS: FIT VARIANCE-BASED MODELS 
#===============================================================================================================================================
# Set Starting Values
svMe <- c(-0.02, -0.01) # start value for means
svPa <- 6.88 # start value for path coefficient (I have taken the square root of the averaged variances)
svPe <- .7 # start value for path coefficient for e
lbPa <- .0001 # lower bound for path coefficient
lbPaD <- valDiagLU(lbPa,-10,NA,nv) # lower bound for diagonal, lower & upper elements of covariance matrix

# PREPARE MODEL
# Create Algebra for expected Mean Matrices
meanG <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels=labVars("mean",vars), name="meanG" )
# Create Matrices for Variance Components
covA <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPa,nv), label=labLower("VA",nv), name="VA" )
covC <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPa,nv), label=labLower("VC",nv), name="VC" )
covE <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPa,nv), label=labLower("VE",nv), name="VE" )
# Create Algebra for expected Variance/Covariance Matrices in MZ & DZ twins
covP <- mxAlgebra( expression= VA+VC+VE, name="V" )
covMZ <- mxAlgebra( expression= VA+VC, name="cMZ" )
covDZ <- mxAlgebra( expression= 0.5%x%VA+ VC, name="cDZ" )
expCovMZ <- mxAlgebra( expression= rbind( cbind(V, cMZ), cbind(t(cMZ), V)), name="expCovMZ" )
expCovDZ <- mxAlgebra( expression= rbind( cbind(V, cDZ), cbind(t(cDZ), V)), name="expCovDZ" )
# Create Data Objects for Multiple Groups
dataMZ <- mxData( observed=mzData, type="raw" )
dataDZ <- mxData( observed=dzData, type="raw" )
# Create Expectation Objects for Multiple Groups
expMZ <- mxExpectationNormal( covariance="expCovMZ", means="meanG", dimnames=selVars )
expDZ <- mxExpectationNormal( covariance="expCovDZ", means="meanG", dimnames=selVars )
funML <- mxFitFunctionML()
# Create Model Objects for Multiple Groups
pars <- list( meanG, covA, covC, covE, covP )
modelMZ <- mxModel( pars, covMZ, expCovMZ, dataMZ, expMZ, funML, name="MZ" )
modelDZ <- mxModel( pars, covDZ, expCovDZ, dataDZ, expDZ, funML, name="DZ" )
multi <- mxFitFunctionMultigroup( c("MZ","DZ") )
# Create Algebra for Standardization
matI <- mxMatrix( type="Iden", nrow=nv, ncol=nv, name="I" )
invSD <- mxAlgebra( expression=solve(sqrt(I*V)), name="iSD" )
# Calculate genetic and environmental correlations
corA <- mxAlgebra( expression=solve(sqrt(I*VA))%&%VA, name ="rA" ) #cov2cor()
corC <- mxAlgebra( expression=solve(sqrt(I*VC))%&%VC, name ="rC" )
corE <- mxAlgebra( expression=solve(sqrt(I*VE))%&%VE, name ="rE" )
# Create Algebra for Unstandardized and Standardized Variance Components
rowUS <- rep('US',nv)
colUS <- rep(c('VA','VC','VE','SA','SC','SE'),each=nv)
estUS <- mxAlgebra( expression=cbind(VA,VC,VE,VA/V,VC/V,VE/V), name="US", dimnames=list(rowUS,colUS) )
# Create Confidence Interval Objects
odd <- seq(1+3*nv,2*3*nv,nv)
even <- seq(2+3*nv,2*3*nv,nv)
ciACE <- mxCI( c("US[1,odd]","US[2,odd]","US[2,even]") )
# Build Model with Confidence Intervals
calc <- list( matI, invSD, corA, corC, corE, estUS, ciACE )
modelACE <- mxModel( "twoACEvc", pars, modelMZ, modelDZ, multi, calc )
# ----------------------------------------------------------------------------------------------------------------------
# RUN MODEL
# Run ACE Model
fitACE <- mxTryHard( modelACE, intervals=T )
(sumACE <- summary( fitACE ))
(fitACE$US$result)
# Compare with Saturated Model
mxCompare( fitSAT, fitACE )
# Print Goodness-of-fit Statistics & Parameter Estimates
fitGofs(fitACE)
fitEstCis(fitACE)
fitACE$MZ$data@numObs
fitACE$DZ$data@numObs

# ----------------------------------------------------------------------------------------------------------------------
# RUN SUBMODELS
# RUN AE
# Drop shared environment by fixing VC to 0
modelAE <- mxModel(fitACE, name="twoAEvc")
modelAE <- omxSetParameters(modelAE, labels=labLower("VC", nv), free=FALSE, values=0)

matI   <- mxMatrix(type = "Iden", nrow = nv, ncol = nv, name = "I")
invSD  <- mxAlgebra(expression = solve(sqrt(I * V)), name = "iSD")

corA   <- mxAlgebra(expression = solve(sqrt(I * VA)) %&% VA, name = "rA")
corE   <- mxAlgebra(expression = solve(sqrt(I * VE)) %&% VE, name = "rE")

rowUS  <- rep('US', nv)
colUS  <- rep(c('VA','VC','VE','SA','SC','SE'), each = nv)
estUS  <- mxAlgebra(expression = cbind(VA, VC, VE, VA/V, VC/V, VE/V), 
                    name = "US", dimnames = list(rowUS, colUS))
modelAE <- mxModel(modelAE, matI, invSD, corA, corE, estUS)
n_cols <- 6 * nv
ci_targets <- c(
  paste0("US[1,", 1:n_cols, "]"),
  paste0("US[2,", 1:n_cols, "]")
)

if (nv == 3) {
  ci_targets <- c(ci_targets, paste0("US[3,", 1:n_cols, "]"))
}

if (nv >= 2) {
  r_indices <- combn(nv, 2, simplify = FALSE)
  ra_targets <- sapply(r_indices, function(idx) sprintf("rA[%d,%d]", idx[2], idx[1]))
  re_targets <- sapply(r_indices, function(idx) sprintf("rE[%d,%d]", idx[2], idx[1]))
  ci_targets <- c(ci_targets, ra_targets, re_targets)
}

modelAE <- mxModel(modelAE, mxCI(ci_targets))

fitAE <- mxTryHard(modelAE, intervals = TRUE)
fitGofs(fitAE); fitEstCis(fitAE)

ae_summary <- summary(fitAE)
ci_table <- ae_summary$CI
print(ci_table)
# Run CE model
modelCE <- mxModel( fitACE, name="twoCEvc" )
modelCE <- omxSetParameters( modelCE, labels=labLower("VA",nv), free=FALSE, values=0 )
modelCE <- omxSetParameters( modelCE, labels=labLower("VC",nv), free=TRUE, values=.6 )
fitCE <- mxTryHard( modelCE, intervals=T )
fitGofs(fitCE); fitEstCis(fitCE)
# Run E model
modelE <- mxModel( fitAE, name="twoEvc" )
modelE <- omxSetParameters( modelE, labels=labLower("VA",nv), free=FALSE, values=0 )
fitE <- mxTryHard( modelE, intervals=T )
fitGofs(fitE); fitEstCis(fitE)
# Print Comparative Fit Statistics
(Compare_bd_ed_varSA<-mxCompare( fitACE, nested <- list(fitAE, fitCE, fitE) ))
write.csv(Compare_bd_ed_varSA, "Compare_ACEvarinceModel_bd_ed.csv", row.names = FALSE)
(results_ACEvar_bd_edSA<-round(rbind(fitACE$US$result,fitAE$US$result,fitCE$US$result,fitE$US$result),4))
write.csv(results_ACEvar_bd_edSA, "results_ACEvarinceModel_bd_ed.csv", row.names = FALSE)

#==========================================================================================================================
#                    AS SENSITIVITY ANALYSIS: FIT PATH-BASED MODELS (CHOLESKY) AND SUBMODELS 
#==========================================================================================================================

# ----------------------------------------------------------------------------------------------------------------------
# PREPARE MODEL
# ACE Model
# Create Algebra for expected Mean Matrices
meanG <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels=labVars("mean",vars), name="meanG" )
# Create Matrices for Path Coefficients
pathA <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPa,nv), labels=labLower("a",nv), lbound=lbPaD,
                   name="a" )
pathC <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPa,nv), labels=labLower("c",nv), lbound=lbPaD,
                   name="c" )
pathE <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPe,nv), labels=labLower("e",nv), lbound=lbPaD,
                   name="e" )
# Create Algebra for Variance Comptwonts
covA <- mxAlgebra( expression=a %*% t(a), name="A" )
covC <- mxAlgebra( expression=c %*% t(c), name="C" )
covE <- mxAlgebra( expression=e %*% t(e), name="E" )
# Create Algebra for expected Variance/Covariance Matrices in MZ & DZ twins
covP <- mxAlgebra( expression= A+C+E, name="V" )
covMZ <- mxAlgebra( expression= A+C, name="cMZ" )
covDZ <- mxAlgebra( expression= 0.5%x%A+ C, name="cDZ" )
expCovMZ <- mxAlgebra( expression= rbind( cbind(V, cMZ), cbind(t(cMZ), V)), name="expCovMZ" )
expCovDZ <- mxAlgebra( expression= rbind( cbind(V, cDZ), cbind(t(cDZ), V)), name="expCovDZ" )
# Create Data Objects for Multiple Groups
dataMZ <- mxData( observed=mzData, type="raw" )
dataDZ <- mxData( observed=dzData, type="raw" )
# Create Expectation Objects for Multiple Groups
expMZ <- mxExpectationNormal( covariance="expCovMZ", means="meanG", dimnames=selVars )
expDZ <- mxExpectationNormal( covariance="expCovDZ", means="meanG", dimnames=selVars )
funML <- mxFitFunctionML()
# Create Model Objects for Multiple Groups
pars <- list( meanG, pathA, pathC, pathE, covA, covC, covE, covP )
modelMZ <- mxModel( pars, covMZ, expCovMZ, dataMZ, expMZ, funML, name="MZ" )
modelDZ <- mxModel( pars, covDZ, expCovDZ, dataDZ, expDZ, funML, name="DZ" )
multi <- mxFitFunctionMultigroup( c("MZ","DZ") )
# Create Algebra for Standardization
matI <- mxMatrix( type="Iden", nrow=nv, ncol=nv, name="I" )
invSD <- mxAlgebra( expression=solve(sqrt(I*V)), name="iSD" )
# Calculate genetic and environmental correlations
corA <- mxAlgebra( expression=solve(sqrt(I*A))%&%A, name ="rA" ) #cov2cor()
corC <- mxAlgebra( expression=solve(sqrt(I*C))%&%C, name ="rC" )
corE <- mxAlgebra( expression=solve(sqrt(I*E))%&%E, name ="rE" )
# Create Algebra for Variance Components
rowUS <- rep('US',nv)
colUS <- rep(c('A','C','E','SA','SC','SE'),each=nv)
estUS <- mxAlgebra( expression=cbind(A,C,E,A/V,C/V,E/V), name="US", dimnames=list(rowUS,colUS) )
# Create Confidence Interval Objects
odd <- seq(1+3*nv,2*3*nv,nv)
even <- seq(2+3*nv,2*3*nv,nv)
ciACE <- mxCI( c("US[1,odd]","US[2,odd]","US[2,even]") )
# Build Model with Confidence Intervals
calc <- list( matI, invSD, corA, corC, corE, estUS, ciACE )
modelACE <- mxModel( "twoACEc", pars, modelMZ, modelDZ, multi, calc, ciACE )
# ----------------------------------------------------------------------------------------------------------------------
# RUN MODEL
# Run ACE Model
fitACE <- mxTryHard( modelACE, intervals=T )
(sumACE <- summary( fitACE, verbose = T ))
# Compare with Saturated Model
mxCompare( fitSAT, fitACE )
# Print Goodness-of-fit Statistics & Parameter Estimates
fitGofs(fitACE); fitEstCis(fitACE)
fitACE$US$result
# ----------------------------------------------------------------------------------------------------------------------
# RUN SUBMODELS
# Run AE model
modelAE <- mxModel( fitACE, name="twoAEc" )
modelAE <- omxSetParameters( modelAE, labels=labLower("c",nv), free=FALSE, values=0 )
fitAE <- mxTryHard( modelAE, intervals=T )
fitGofs(fitAE); fitEstCis(fitAE)
# Run CE model
modelCE <- mxModel( fitACE, name="twoCEc" )
modelCE <- omxSetParameters( modelCE, labels=labLower("a",nv), free=FALSE, values=0 )
fitCE <- mxTryHard( modelCE, intervals=T )
fitGofs(fitCE); fitEstCis(fitCE)
# Run E model
modelE <- mxModel( fitAE, name="twoEc" )
modelE <- omxSetParameters( modelE, labels=labLower("a",nv), free=FALSE, values=0 )
fitE <- mxTryHard( modelE, intervals=T )
fitGofs(fitE); fitEstCis(fitE)
# Print Comparative Fit Statistics
(Compare_ACE_bd_edSA<-mxCompare( fitACE, nested <- list(fitAE, fitCE, fitE) ))
write.csv(Compare_ACE_bd_edSA, "Compare_ACE_bd_ed.csv", row.names = FALSE)
(results_ACE_bd_edSA<-round(rbind(fitACE$US$result,fitAE$US$result,fitCE$US$result,fitE$US$result),4))
write.csv(results_ACE_bd_edSA, "results_ACE_bd_ed.csv", row.names = FALSE)

# ----------------------------------------------------------------------------------------------------------------------
# Fit the ADE model + its submodels! 
# ----------------------------------------------------------------------------------------------------------------------
# PREPARE MODEL
# ACE Model
# Create Algebra for expected Mean Matrices
meanG <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels=labVars("mean",vars), name="meanG" )
# Create Matrices for Path Coefficients
pathA <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPa,nv), labels=labLower("a",nv), lbound=lbPaD,
                   name="a" )
pathD <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPa,nv), labels=labLower("d",nv), lbound=lbPaD,
                   name="d" )
pathE <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPe,nv), labels=labLower("e",nv), lbound=lbPaD,
                   name="e" )
# Create Algebra for Variance Comptwonts
covA <- mxAlgebra( expression=a %*% t(a), name="A" )
covD <- mxAlgebra( expression=d %*% t(d), name="D" )
covE <- mxAlgebra( expression=e %*% t(e), name="E" )
# Create Algebra for expected Variance/Covariance Matrices in MZ & DZ twins
covP <- mxAlgebra( expression= A+D+E, name="V" )
covMZ <- mxAlgebra( expression= A+D, name="cMZ" )
covDZ <- mxAlgebra( expression= 0.5%x%A+ 0.25%x%D, name="cDZ" )
expCovMZ <- mxAlgebra( expression= rbind( cbind(V, cMZ), cbind(t(cMZ), V)), name="expCovMZ" )
expCovDZ <- mxAlgebra( expression= rbind( cbind(V, cDZ), cbind(t(cDZ), V)), name="expCovDZ" )
# Create Data Objects for Multiple Groups
dataMZ <- mxData( observed=mzData, type="raw" )
dataDZ <- mxData( observed=dzData, type="raw" )
# Create Expectation Objects for Multiple Groups
expMZ <- mxExpectationNormal( covariance="expCovMZ", means="meanG", dimnames=selVars )
expDZ <- mxExpectationNormal( covariance="expCovDZ", means="meanG", dimnames=selVars )
funML <- mxFitFunctionML()
# Create Model Objects for Multiple Groups
pars <- list(meanG, pathA, pathD, pathE, covA, covD, covE, covP )
modelMZ <- mxModel( pars, covMZ, expCovMZ, dataMZ, expMZ, funML, name="MZ" )
modelDZ <- mxModel( pars, covDZ, expCovDZ, dataDZ, expDZ, funML, name="DZ" )
multi <- mxFitFunctionMultigroup( c("MZ","DZ") )
# Create Algebra for Standardization
matI <- mxMatrix( type="Iden", nrow=nv, ncol=nv, name="I" )
invSD <- mxAlgebra( expression=solve(sqrt(I*V)), name="iSD" )
# Calculate genetic and environmental correlations
corA <- mxAlgebra( expression=solve(sqrt(I*A))%&%A, name ="rA" ) #cov2cor()
corD <- mxAlgebra( expression=solve(sqrt(I*D))%&%D, name ="rD" )
corE <- mxAlgebra( expression=solve(sqrt(I*E))%&%E, name ="rE" )
# Create Algebra for Variance Components
rowUS <- rep('US',nv)
colUS <- rep(c('A','D','E','SA','SD','SE'),each=nv)
estUS <- mxAlgebra( expression=cbind(A,D,E,A/V,D/V,E/V), name="US", dimnames=list(rowUS,colUS) )
# Create Confidence Interval Objects
odd <- seq(1+3*nv,2*3*nv,nv)
even <- seq(2+3*nv,2*3*nv,nv)
ciADE <- mxCI( c("US[1,odd]","US[2,odd]","US[2,even]") )
# Build Model with Confidence Intervals
calc <- list( matI, invSD, corA, corD, corE, estUS, ciADE )
modelADE <- mxModel( "twoADEc", pars, modelMZ, modelDZ, multi, calc )
# ----------------------------------------------------------------------------------------------------------------------
# RUN MODEL
# Run ADE Model
fitADE <- mxTryHard( modelADE, intervals=T )
sumADE <- summary( fitADE )
# Compare with Saturated Model
#mxCompare( fitSAT, fitADE )
# Print Goodness-of-fit Statistics & Parameter Estimates
fitGofs(fitADE)
fitEstCis(fitADE)
# ----------------------------------------------------------------------------------------------------------------------
# RUN SUBMODELS
# Run AE model
modelAE <- mxModel( fitADE, name="twoAEc" )
modelAE <- omxSetParameters( modelAE, labels=labLower("d",nv), free=FALSE, values=0 )
fitAE <- mxTryHard( modelAE, intervals=T )
fitGofs(fitAE); fitEstCis(fitAE)
# Run E model
modelE <- mxModel( fitAE, name="twoEc" )
modelE <- omxSetParameters( modelE, labels=labLower("a",nv), free=FALSE, values=0 )
fitE <- mxTryHard( modelE, intervals=T )
fitGofs(fitE); fitEstCis(fitE)
# Print Comparative Fit Statistics
(Compare_ADE_bd_edSA<-mxCompare( fitADE, nested <- list(fitAE, fitE) ))
write.csv(Compare_ADE_bd_edSA, "Compare_ADE_bd_ed.csv", row.names = FALSE)
(ADE_results_bd_edSA<-round(rbind(fitADE$US$result,fitAE$US$result,fitE$US$result),4))
write.csv(ADE_results_bd_edSA, "ADE_results_bd_ed.csv", row.names = FALSE)
sink()

