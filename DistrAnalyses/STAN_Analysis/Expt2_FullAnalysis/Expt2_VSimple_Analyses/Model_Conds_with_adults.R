library(retimes)
library(rstan)
library(ggplot2)
library(retimes)
library(rstan)
library(ggplot2)
library(doBy)
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

tt<- subset(tt, RT.ms <= 6000 & RT.ms >= -500)
# I should really try with a lower cutoff. 4s? Done now; doesn't improve fit.
tt$rt <- tt$RT.ms
tt$N_Early <- ifelse(tt$Early.Late == "early",0,1)
tt$N_Pred <- ifelse(tt$Pred == "Pred",0,1)
tt$N_Age <- ifelse(tt$Age == "Adult",0,1)#model.matrix(~tt$Age)[,2]
tt$N_E_P_Interact <- tt$N_Pred * tt$N_Early
tt$N_Early_Age_Interact <- tt$N_Early * tt$N_Age
tt$N_Pred_Age_Interact <- tt$N_Pred * tt$N_Age
tt$N_Early_Pred_Age_Interact <- tt$N_Early * tt$N_Pred * tt$N_Age
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
                     factor3 = tt$N_Age, 
                     factor4 = tt$N_E_P_Interact, 
                    factor5 = tt$N_Early_Age_Interact, 
                     factor6 = tt$N_Pred_Age_Interact, 
                    factor7 = tt$N_Early_Pred_Age_Interact, 
                     N = nrow(tt), J = nlevels(as.factor(tt$Subject)), Subj = as.integer(as.factor(tt$Subject)))

age_conds <- stan(file="fixEf_Conds_Adults_transf_expt2.stan",
                    data=stanDat_full,
                    chains = 3, iter = 50,  control = list(adapt_delta = 0.88))


print(age_conds, pars = c("beta0","beta","beta_s0","beta_s","beta_t0","beta_t"), probs = c(0.025,0.5,0.975))