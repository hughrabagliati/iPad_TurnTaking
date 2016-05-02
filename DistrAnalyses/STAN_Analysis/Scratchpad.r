# rDat <- read.table("gibsonwu2012data.txt", header = T)
# rDat <- subset(rDat, region == "headnoun")
# rDat$Subj <- as.factor(rDat$Subj)
# rDat$Item <- as.factor(rDat$Item)
# rDat$so <- ifelse(rDat$type == "subj-ext",-1,1)
# stanDat <- list(rt = rDat$rt,so = rDat$so,N = nrow(rDat))
# fixeffit <- stan(file = "fixEf.stan",data = stanDat,iter = 2000, chains = 4)

library(retimes)
library(rstan)
adult <- read.csv("Adult_R.csv")
adult$rt <- adult$RTms
adult$N_Match <- ifelse(adult$Match == "Match",-1,1)
adult$N_Pred <- ifelse(adult$Pred == "Pred",-1,1)
adult$N_M_P_Interact <- adult$N_Pred * adult$N_Match
adult$rt <- adult$rt + abs(min(adult$rt))
# Fit Ex-Gaussian using ML (retimes library) 
eg_ml <- timefit(adult$rt)
print(eg_ml)
# Calculate expected value (mu + tau)
ev_eg_ml <- eg_ml@par[1] + eg_ml@par[3]
print(ev_eg_ml)

# Now we try to fit the same distribution with STAN

# STAN model for ex-Gaussian fit
stanDat <- list(rt = adult$rt,factor1 = adult$N_Match,factor2 = adult$N_Pred,factor3 = adult$N_M_P_Interact,N = nrow(adult))

eg_stan <- stan(file="fixEf.stan",
                data=stanDat,
                iter=5000, warmup = 2000, chains = 1)

timefit(subset(adult, Match == "Match")$rt)
timefit(subset(adult, Match == "Mismatch")$rt)
timefit(subset(adult, Pred == "Pred")$rt)
timefit(subset(adult, Pred == "Unpred")$rt)
summary(eg_stan)