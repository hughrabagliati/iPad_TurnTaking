library(retimes)
library(rstan)
ads <- read.csv("Adult_R.csv")
fives <- read.csv("5yo_R.csv")
threes <- read.csv("3yo_R.csv")

ads$Age <- "Adult"
fives$Age <- "Five"
threes$Age <- "Three"

tt <- rbind(ads,fives,threes)
tt$Age <- as.factor(tt$Age)
tt$Subject <- paste(tt$Age,tt$Participant, sep = "")

tt<- subset(tt, RTms <= 8000)
tt$rt <- tt$RTms
tt$N_Match <- ifelse(tt$Match == "Match",0,1)
tt$N_Pred <- ifelse(tt$Pred == "Pred",0,1)
tt$N_AgeFive <- model.matrix(~tt$Age)[,2]
tt$N_AgeThree <- model.matrix(~tt$Age)[,3]
tt$N_M_P_Interact <- tt$N_Pred * tt$N_Match
tt$N_Match_AgeFive_Interact <-  tt$N_Match * tt$N_AgeFive
tt$N_Match_AgeThree_Interact <- tt$N_Match * tt$N_AgeThree
tt$N_Pred_AgeFive_Interact <- tt$N_Pred * tt$N_AgeFive
tt$N_Pred_AgeThree_Interact <- tt$N_Pred * tt$N_AgeThree
tt$N_Match_Pred_AgeFive_Interact <- tt$N_Match * tt$N_Pred * tt$N_AgeFive
tt$N_Match_Pred_AgeThree_Interact <- tt$N_Match * tt$N_Pred * tt$N_AgeThree
# For some reason, model won't converge with RTs above zero?
# tt$rt_scale <- (tt$rt - mean(tt$rt,na.rm = T))/sd(tt$rt, na.rm = T)
tt$rt <- tt$rt + abs(min(tt$rt))


# Fit Ex-Gaussian using ML (retimes library) 
eg_ml <- timefit(tt$rt)
print(eg_ml)





# STAN model for ex-Gaussian fit - Age and Conds
stanDat <- list(rt = tt$rt,factor1 = tt$N_Match,factor2 = tt$N_Pred,factor3 = tt$N_AgeFive,factor4 = tt$N_AgeThree, N = nrow(tt), J = nlevels(as.factor(tt$Subject)), Subj = as.integer(as.factor(tt$Subject)))

eg_stan <- stan(file="fixEf_Age_and_Conds.stan",
                data=stanDat,
                iter=500, warmup = 200, chains = 1)
print(eg_stan, pars = c("beta","beta_s","beta_t"), probs = c(0.025,0.5,0.975))



timefit(subset(tt, Match == "Match")$rt)
timefit(subset(tt, Match == "Mismatch")$rt)
timefit(subset(tt, Pred == "Pred")$rt)
timefit(subset(tt, Pred == "Unpred")$rt)

timefit(subset(tt, Age == "Adult")$rt)
timefit(subset(tt, Age == "Three")$rt)
timefit(subset(tt, Age == "Five")$rt)
summary(eg_stan)
print(eg_stan, pars = c("beta","beta_s","beta_t"), probs = c(0.025,0.5,0.975))