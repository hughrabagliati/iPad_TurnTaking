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
# I should really try with a lower cutoff. 4s? Done now; doesn't improve fit.
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

tt$rt <- (tt$rt - mean(tt$rt))/sd(tt$rt)
ggplot(tt,aes(x=rt,..density..,col=Age))+ geom_freqpoly(alpha=1,lwd =1.5)+xlab("Response Time (ms)")
ggplot(tt,aes(x=rt,..density..,col=Pred))+ geom_freqpoly(alpha=1,lwd =1.5, bins = 50)+xlab("Response Time (ms)")+facet_wrap(Match ~ Age) + xlim(c(-2,4))

# For some reason, model won't converge with RTs above zero?
#tt$rt <- tt$rt + abs(min(tt$rt))
# Fit Ex-Gaussian using ML (retimes library) 
eg_ml <- timefit(tt$rt)
print(eg_ml)


stanDat_full <- list(rt = tt$rt,factor1 = tt$N_Match,factor2 = tt$N_Pred,factor3 = tt$N_AgeFive,factor4 = tt$N_AgeThree, factor5 = tt$N_M_P_Interact, 
                     factor6 = tt$N_Match_AgeFive_Interact, factor6a = tt$N_Match_AgeThree_Interact, factor7 = tt$N_Pred_AgeFive_Interact, factor7a = tt$N_Pred_AgeThree_Interact, 
                     factor8 = tt$N_Match_Pred_AgeFive_Interact, factor8a = tt$N_Match_Pred_AgeThree_Interact, N = nrow(tt), J = nlevels(as.factor(tt$Subject)), Subj = as.integer(as.factor(tt$Subject)))


eg_stan_exp <- stan(file="fixEf_Age_and_Conds_transf_exp2_betas.stan",
                data=stanDat_full,
                 chains = 2, iter = 150, control = list(adapt_delta = 0.95))
print(eg_stan_exp, pars = c("beta0","beta","beta_s0","beta_s","beta_t0","beta_t"), probs = c(0.025,0.5,0.975))


tt_kids <- subset(tt, Age != "Adult")
tt_adults <- subset(tt, Age == "Adult")
stanDat_kids <- list(rt = tt_kids$rt,factor1 = tt_kids$N_Match,factor2 = tt_kids$N_Pred,factor3 = tt_kids$N_M_P_Interact, 
                     N = nrow(tt_kids), J = nlevels(as.factor(tt_kids$Subject)), Subj = as.integer(as.factor(tt_kids$Subject)))

stanDat_adults <- list(rt = tt_adults$rt,factor1 = tt_adults$N_Match,factor2 = tt_adults$N_Pred,factor3 = tt_adults$N_M_P_Interact, 
                     N = nrow(tt_adults), J = nlevels(as.factor(tt_adults$Subject)), Subj = as.integer(as.factor(tt_adults$Subject)))

eg_stan_kids <- stan(file="fixEf_Conds_transf_exp2_betas.stan",
                       data=stanDat_kids,
                       chains = 2, iter = 150, control = list(adapt_delta = 0.95))
print(eg_stan_kids, pars = c("beta0","beta","beta_s0","beta_s","beta_t0","beta_t"), probs = c(0.025,0.5,0.975))


eg_stan_adults <- stan(file="fixEf_Conds_transf_exp2_betas.stan",
                    data=stanDat_adults,
                    chains = 2, iter = 150, control = list(adapt_delta = 0.95))
print(eg_stan_adults, pars = c("beta0","beta","beta_s0","beta_s","beta_t0","beta_t"), probs = c(0.025,0.5,0.975))




# Full regression
# Initial values at 1
initf1 <- function() {
list(beta = c(1,rep(0,11)), beta_t = c(1,rep(0,11)),beta_s = c(1,rep(0,11)))
}


eg_stan_full <- stan(file="fixEf_Age_and_Conds_transf.stan",
                data=stanDat_full,
                 chains = 4, init = initf1, control = list(adapt_delta = 0.88))
print(eg_stan_full, pars = c("beta0","beta","beta_s0","beta_s","beta_t0","beta_t"), probs = c(0.025,0.5,0.975))