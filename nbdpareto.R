## SETTING A WORKING DIRECTORY
setwd("insert your working directory here")

## LOADING THE NECESSARY PACKAGES
library(dplyr)
library(tidyr)
library(ggplot2)
library(BTYDplus)

## LOADING THE DATA
cdnow <- read.table("C:/Users/Andini Eka F/Documents/R/Business Analytics EDX Course/CDNOW_master.txt",header=FALSE)
head(cdnow)
#    V1     V2   V3   V4
# 1  1 19970101  1 11.77
# 2  2 19970112  1 12.00
# 3  2 19970112  5 77.00
# 4  3 19970102  2 20.76
# 5  3 19970330  2 20.76
# 6  3 19970402  2 19.54

# "V1" = customer ID
# "V2" = purchase date
# "V3" = purchased quantity
# "V4" = total spend

cdnow$V3 <- NULL # removing "V3" variable since it will not be applicable for this kind of analysis
names(cdnow) <- c("cust","date","spend")
str(cdnow) #checking the data type of each variable
cdnow$date <- as.Date(as.character(cdnow$date),"%Y%m%d")

## DATA PREPARATION FOR MODELLING

# The main data set will be cut at purchase date "1997-09-30"
# Observation with purchase date before or on "1997-09-30" will be included in the training data, otherwise it will be included in the test data.

cutoff <- "1997-09-30" #setting the cutoff date

# In order that the data can qualify to proceed with the modelling, it needs to be converted into a customer-by-sufficient-statistic matrix
# Customer-by-sufficient-statistic matrix produces the below features:
# cust = the unique customer Id
# x = the number of repeat transactions (frequency) - if a customer only purchases the product once and never repurchases, it will be counted as 0 repeat transaction.
# t.x = the time of the last recorded transaction given in weeks elapsed since first (recency)
# litt = the sum over logarithmic intertransaction times (required for estimating regularity)
# first = the date of the first transaction,
# T.cal = the duration between the first transaction and the end of the cut off period in weeks
# T.star = the length of the holdout period in weeks
# x.star = the number of repeat transactions during the holdout period

 cbs <- elog2cbs(cdnow, T.cal=cutoff) # producing the customer-by-sufficient-statistic matrix
 summary(cbs$x)
 #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 # 0.000   0.000   0.000   1.033   1.000  80.000 
 
 # Customers in observation purchase the product only once, in average

## MODEL BUILDING: PARAMETER ESTIMATION
# NBD Pareto Model is based on these assumptions:
# 1. Heterogeneity in transaction rates across customers follows a gamma distribution with shape parameter r and scale parameter α
# 2. Heterogeneity in dropout rates across customers follows a gamma distribution with shape parameter s and scale parameter β

# We are going to estimate these four parameters: r, α, s, β

params.cbs <- pnbd.EstimateParameters(cbs)
names(params.cbs) <- c("r", "alpha", "s", "beta")
round(params.cbs, 3)
#      r  alpha      s   beta 
#  0.597 11.585  0.522  8.833 

# The estimated parameters above will be fitted into the test data to predict the number of transactions customers will make during the test period

cbs$xstar.predict <- pnbd.ConditionalExpectedTransactions(params = params.pnbd, T.star = cbs$T.star, x = cbs$x, t.x = cbs$t.x, T.cal = cbs$T.cal)

## MODEL EVALUATION

sqrt(mean((cbs$xstar.predict-cbs$x.star)^2)) # calculating the RMSE

test.actual <- sum(cbs$x.star)
test.predict <- round(sum(cbs$xstar.predict)) 
compare <- rbind(test.actual,test.predict)
colnames(compare) <- "Sum of Transaction in The Test Period"
rownames(compare) <- c("Actual","Predict")

#           Sum of Transaction in The Test Period
# Actual                                  19684
# Predict                                 17152

(test.actual/test.predict) - 1 #calculating the magnitude of difference from the transaction sum angle
# [1] 0.1476213

sum(cbs$x.star == cbs$xstar.predict) / nrow(CBS2) 
# [1] 0.6398388
# 63% of the predicted purchase frequency for a certain customer exactly matches with the actual value


