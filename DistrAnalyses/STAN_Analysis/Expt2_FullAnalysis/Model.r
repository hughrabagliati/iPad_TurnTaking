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
fives <- read.csv("exp2_fives.csv")
threes <- read.csv("exp2_threes.csv")

ads$Age <- "Adult"
fives$Age <- "Five"
threes$Age <- "Three"

tt <- rbind(ads,threes,fives)
tt$Age <- as.factor(tt$Age)
tt$Subject <- paste(tt$Age,tt$Subject, sep = "")

tt<- subset(tt, RT.ms <= 8000 & RT.ms >= -1000)
# I should really try with a lower cutoff. 4s? Done now; doesn't improve fit.
tt$rt <- tt$RT.ms
tt$N_Early <- ifelse(tt$Early.Late == "early",0,1)
tt$N_Pred <- ifelse(tt$Pred == "Pred",0,1)
tt$N_AgeFive <- model.matrix(~tt$Age)[,2]
tt$N_AgeThree <- model.matrix(~tt$Age)[,3]
#tt$N_AgeThree <- ifelse(tt$Age == "Five",0,1)#model.matrix(~tt$Age)[,2]
tt$N_E_P_Interact <- tt$N_Pred * tt$N_Early
tt$N_Early_AgeFive_Interact <-  tt$N_Early * tt$N_AgeFive
tt$N_Early_AgeThree_Interact <- tt$N_Early * tt$N_AgeThree
tt$N_Pred_AgeFive_Interact <- tt$N_Pred * tt$N_AgeFive
tt$N_Pred_AgeThree_Interact <- tt$N_Pred * tt$N_AgeThree
tt$N_Early_Pred_AgeFive_Interact <- tt$N_Early * tt$N_Pred * tt$N_AgeFive
tt$N_Early_Pred_AgeThree_Interact <- tt$N_Early * tt$N_Pred * tt$N_AgeThree
# tt$rt_scale <- (tt$rt - mean(tt$rt,na.rm = T))/sd(tt$rt, na.rm = T)

summaryBy(rt ~ Subject, data = tt, FUN = length) -> n.trials
tt <- subset(tt, Subject %in% n.trials[n.trials$rt.length >= 20,]$Subject & Accuracy == 1)
tt$rt <- tt$rt + abs(min(tt$rt)) + 0.001

tt$rt <- (tt$rt - mean(tt$rt))/(sd(tt$rt))
tt$scale_character_length <- (tt$CharacterLength.ms - mean(tt$CharacterLength.ms))/(sd(tt$CharacterLength.ms))



ggplot(tt,aes(x=rt,..density..,col=Pred))+ geom_freqpoly(alpha=1,lwd =1.5, bins = 50)+xlab("Response Time (ms)")+facet_wrap(Early.Late ~ Age) + xlim(c(-2,4))

# For some reason, model won't converge with RTs above zero?
#tt$rt <- tt$rt + abs(min(tt$rt))
# Fit Ex-Gaussian using ML (retimes library) 
eg_ml <- timefit(tt$rt)
print(eg_ml)

stanDat_full <- list(rt = tt$rt,
                     factor1 = tt$N_Early,
                     factor2 = tt$N_Pred,
                     factor3 = tt$N_Right,
                     factor4 = tt$N_AgeFive,
                     factor5 = tt$N_AgeThree, 
                     factor6 = tt$N_E_P_Interact, 
                     factor7 = tt$N_Early_AgeFive_Interact, 
                     factor7a = tt$N_Early_AgeThree_Interact, 
                     factor8 = tt$N_Pred_AgeFive_Interact, 
                     factor8a = tt$N_Pred_AgeThree_Interact, 
                     factor9 = tt$N_Early_Pred_AgeFive_Interact, 
                     factor9a = tt$N_Early_Pred_AgeThree_Interact, 
                     N = nrow(tt), J = nlevels(as.factor(tt$Subject)), Subj = as.integer(as.factor(tt$Subject)))

eg_stan_exp2 <- stan(file="fixEf_AllAges_and_Conds_transf_expt2_beta.stan",
                    data=stanDat_full,
                    chains = 3, iter = 750,  control = list(adapt_delta = 0.95))

tt_adult <- subset(tt, Age == "Adult")

stanDat_adult <- list(rt = tt_adult$rt,
                      factor1 = tt_adult$N_Early,
                      factor2 = tt_adult$N_Pred,
                      factor3 = tt_adult$N_Right,
                      factor4 = tt_adult$N_E_P_Interact, 
                      N = nrow(tt_adult), J = nlevels(as.factor(tt_adult$Subject)), Subj = as.integer(as.factor(tt_adult$Subject)))

adult_model2 <- stan(file="fixEf_Conds_transf_expt2.stan",
                    data=stanDat_adult,
                    chains = 3, iter = 750,  control = list(adapt_delta = 0.95))

tt_kids <- subset(tt, Age != "Adult")

stanDat_kids <- list(rt = tt_kids$rt,
                     factor1 = tt_kids$N_Early,
                     factor2 = tt_kids$N_Pred,
                     factor3 = tt_kids$N_Right,
                     factor4 = tt_kids$N_E_P_Interact, 
                     N = nrow(tt_kids), J = nlevels(as.factor(tt_kids$Subject)), Subj = as.integer(as.factor(tt_kids$Subject)))

kid_model <- stan(file="fixEf_Conds_transf_expt2.stan",
                  data=stanDat_kids,
                  chains = 3, iter = 750,  control = list(adapt_delta = 0.95))


print(eg_stan_exp, pars = c("beta0","beta","beta_s0","beta_s","beta_t0","beta_t"), probs = c(0.025,0.5,0.975))