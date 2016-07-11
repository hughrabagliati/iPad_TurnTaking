library(retimes)
library(rstan)
library(ggplot2)


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(123)

test_data<- data.frame(Subj = rep(1:100, each = 100))
test_data$data = NA
for (i in 1:100){test_data[test_data$Subj ==i,]$data = rexgauss(100,jitter(800),jitter(170),jitter(500))}

# STAN model for ex-Gaussian fit - Age 
stanDat <- list(rt = test_data$data, N = nrow(test_data), J = nlevels(as.factor(test_data$Subj)), Subj = as.integer(as.factor(test_data$Subj)))

eg_stan <- stan(file="fixEf_Rands.stan",
                data=stanDat,
                iter=500, warmup = 200, chains = 1)
print(eg_stan, pars = c("beta","beta_s","beta_t"), probs = c(0.025,0.5,0.975))
