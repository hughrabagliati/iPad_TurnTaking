library(retimes)
library(rstan)
library(ggplot2)
library(retimes)
library(rstan)
library(ggplot2)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(123)

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
# tt$rt_scale <- (tt$rt - mean(tt$rt,na.rm = T))/sd(tt$rt, na.rm = T)

ggplot(tt,aes(x=rt,..density..,col=Age))+ geom_freqpoly(alpha=1,lwd =1.5,binwidth=150)+xlab("Response Time (ms)")

# For some reason, model won't converge with RTs above zero?
tt$rt <- tt$rt + abs(min(tt$rt))
# Fit Ex-Gaussian using ML (retimes library) 
eg_ml <- timefit(tt$rt)
print(eg_ml)

# Full regression
# Initial values at 1
initf1 <- function() {
list(beta = c(1,rep(0,11)), beta_t = c(1,rep(0,11)),beta_s = c(1,rep(0,11)))
}

stanDat_full <- list(rt = tt$rt,factor1 = tt$N_Match,factor2 = tt$N_Pred,factor3 = tt$N_AgeFive,factor4 = tt$N_AgeThree, factor5 = tt$N_M_P_Interact, 
					factor6 = tt$N_Match_AgeFive_Interact, factor6a = tt$N_Match_AgeThree_Interact, factor7 = tt$N_Pred_AgeFive_Interact, factor7a = tt$N_Pred_AgeThree_Interact, 
					factor8 = tt$N_Match_Pred_AgeFive_Interact, factor8a = tt$N_Match_Pred_AgeThree_Interact, N = nrow(tt), J = nlevels(as.factor(tt$Subject)), Subj = as.integer(as.factor(tt$Subject)))

eg_stan_full <- stan(file="fixEf_Age_and_Conds_transf.stan",
                data=stanDat_full,
                 chains = 4, init = initf1, control = list(adapt_delta = 0.88))
print(eg_stan_full, pars = c("beta","beta_s","beta_t"), probs = c(0.025,0.5,0.975))