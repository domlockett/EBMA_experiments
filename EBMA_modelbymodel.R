library(glmnet)
library(FindIt)
library(arm)
library(mboost)
library(GAMBoost)
library(BayesTree)
library(randomForest)
library(KRLS)
library(rJava)
# Set java's max memory to 4 gigs:
.jinit(parameters="-Xmx4g")
library(RWeka)

setwd("C:/Users/dl0ck/Dropbox/EBMA_experiments")
jan <- read.csv('January Wave/Data_0219/jan_HL.csv')
oct <- read.csv('October Wave/Data_1018/oct_HL.csv')

dat <- as.matrix(jan[, c("HL_accuracy","treat", "totalfakenewscount_new", "female", "college", "agecat", "dem_leaners","polint", "ideology", "conspiracy_mean", "proD", "proR", "highProm","lowProm","hyper","real", "weight")])

#Get rid of NA's because GLMNET is not friendly with NA's
dat <- na.omit(dat)
train_index <- sample(1:nrow(dat), 0.8 * nrow(dat))
test_index <- setdiff(1:nrow(dat), train_index)

# Compute standard deviations to rescale data:

if(is.null(X)==F){
  SDsToRescaleX <- apply(X, 2, sd, na.rm=T)
  
  # if sd=0, make rescaling factor 1 (so it's not NaN)
  SDsToRescaleX[SDsToRescaleX==0] <- 1 
  SDsToRescaleXt <- apply(Xt, 2, sd, na.rm=T) 
  SDsToRescaleXt[SDsToRescaleXt==0] <- 1 
  
  # standardize coeffs store result in new matrix
  Xstd <- apply(X, 2, mkstand)
  Xtstd <- apply(Xt, 2, mkstand)
  
  # Need to make ints for inclusion in original X matrix that is passed in
  # (This will be used for every model besides FindIt)
  Xfull <- model.matrix(~X*treat)
  Xtfull <- model.matrix(~Xt*treatt)	
}
if(is.null(X)==T){
  Xfull<- model.matrix(~treat)
  Xtfull<- model.matrix(~treatt)
}


## the first methods are based on cv.glmnet
# varying alpha to observe lasso (a=1), elastic net (a=.5), and ridge (alpha =0) 
fit1<- cv.glmnet(y =Y, x= Xfull, alpha=1, family='multinomial', typ='mse')
fit2<- cv.glmnet(y = Y, x= Xfull, alpha=0.5, family='multinomial', type='mse')
fit3<- cv.glmnet(y = Y, x= Xfull, alpha=0, family='multinomial', type='mse')
