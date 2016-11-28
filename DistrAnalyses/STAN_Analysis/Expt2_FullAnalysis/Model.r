library(retimes)
library(rstan)
library(ggplot2)
library(retimes)
library(rstan)
library(ggplot2)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(123)

ads <- read.csv("exp2_adults.csv")
#fives <- read.csv("exp2_fives.csv")
threes <- read.csv("exp2_threes.csv")

ads$Age <- "Adult"
#fives$Age <- "Five"
threes$Age <- "Three"

tt <- rbind(ads,threes)#fives,threes)
tt$Age <- as.factor(tt$Age)
tt$Subject <- paste(tt$Age,tt$Participant, sep = "")

tt<- subset(tt, RT.ms <= 12000)
# I should really try with a lower cutoff. 4s? Done now; doesn't improve fit.
tt$rt <- tt$RT.ms
tt$N_Early <- ifelse(tt$Early.Late == "early",0,1)
tt$N_Pred <- ifelse(tt$Pred == "Pred",0,1)
#tt$N_AgeFive <- model.matrix(~tt$Age)[,2]
#tt$N_AgeThree <- model.matrix(~tt$Age)[,3]
tt$N_AgeThree <- ifelse(tt$Age == "Adult",0,1)#model.matrix(~tt$Age)[,2]
tt$N_E_P_Interact <- tt$N_Pred * tt$N_Early
#tt$N_Early_AgeFive_Interact <-  tt$N_Early * tt$N_AgeFive
tt$N_Early_AgeThree_Interact <- tt$N_Early * tt$N_AgeThree
#tt$N_Pred_AgeFive_Interact <- tt$N_Pred * tt$N_AgeFive
tt$N_Pred_AgeThree_Interact <- tt$N_Pred * tt$N_AgeThree
#tt$N_Early_Pred_AgeFive_Interact <- tt$N_Early * tt$N_Pred * tt$N_AgeFive
tt$N_Early_Pred_AgeThree_Interact <- tt$N_Early * tt$N_Pred * tt$N_AgeThree
# tt$rt_scale <- (tt$rt - mean(tt$rt,na.rm = T))/sd(tt$rt, na.rm = T)

tt$rt <- (tt$rt - mean(tt$rt))/sd(tt$rt)
ggplot(tt,aes(x=rt,..density..,col=Age))+ geom_freqpoly(alpha=1,lwd =1.5)+xlab("Response Time (ms)")

# For some reason, model won't converge with RTs above zero?
#tt$rt <- tt$rt + abs(min(tt$rt))
# Fit Ex-Gaussian using ML (retimes library) 
eg_ml <- timefit(tt$rt)
print(eg_ml)




eg_stan_exp <- stan(file="fixEf_Age_and_Conds_transf_expt2.stan",
                data=stanDat_full,
                 chains = 1, iter = 50, control = list(adapt_delta = 0.88))
print(eg_stan_exp, pars = c("beta0","beta","beta_s0","beta_s","beta_t0","beta_t"), probs = c(0.025,0.5,0.975))







# Full regression
# Initial values at 1
initf1 <- function() {
list(beta = c(0,rep(0,7)), beta_t = c(0,rep(0,7)),beta_s = c(0,rep(0,7)))
}

stanDat_full <- list(rt = tt$rt,
                     factor1 = tt$N_Early,
                     factor2 = tt$N_Pred,
                     #factor3 = tt$N_AgeFive,
                     factor4 = tt$N_AgeThree, 
                     factor5 = tt$N_E_P_Interact, 
                     #factor6 = tt$N_Early_AgeFive_Interact, 
                        factor6a = tt$N_Early_AgeThree_Interact, 
                     #factor7 = tt$N_Pred_AgeFive_Interact, 
                        factor7a = tt$N_Pred_AgeThree_Interact, 
                     #factor8 = tt$N_Early_Pred_AgeFive_Interact, 
                        factor8a = tt$N_Early_Pred_AgeThree_Interact, 
                     N = nrow(tt), J = nlevels(as.factor(tt$Subject)), Subj = as.integer(as.factor(tt$Subject)))

eg_stan_full <- stan(file="fixEf_Age_and_Conds_transf_expt2.stan",
                data=stanDat_full,
                 chains = 1,iter=50, init = initf1, control = list(adapt_delta = 0.88))
print(eg_stan_full, pars = c("beta0","beta","beta_s0","beta_s","beta_t0","beta_t"), probs = c(0.025,0.5,0.975))