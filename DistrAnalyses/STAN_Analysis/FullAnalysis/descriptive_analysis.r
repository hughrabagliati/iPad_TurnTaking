# Need to read in output of stan fit first 
eg_stan_list <- extract(eg_stan_exp)


subj_params <- data.frame(Subject = unique(as.factor(tt$Subject)), u = colMeans(eg_stan_list$u), 
			u_s = colMeans(eg_stan_list$u_s), u_t = colMeans(eg_stan_list$u_t), mean = tapply(tt$rt,tt$Subject,mean)
			)


descrip <- read.csv("descriptives.csv", header = T)
des <- descrip[order(descrip$Subject),]

des <- merge(des,subj_params, by = "Subject")

des$beta_t0 <- mean(eg_stan_list$beta_t0)
des[,13:23] <- colMeans(eg_stan_list$beta_t)
colnames(des)[13:23] <- paste("beta_t",1:11, sep  = "")

des$beta0 <- mean(eg_stan_list$beta0)
des[,25:35] <- colMeans(eg_stan_list$beta_t)
colnames(des)[25:35] <- paste("beta",1:11, sep  = "")


des$full_t <- des$beta_t0 + des$u_t
des[des$Age == "Five",]$full_t <- des[des$Age == "Five",]$full_t + des[des$Age == "Five",]$beta_t3
des[des$Age == "Three",]$full_t <- des[des$Age == "Three",]$full_t + des[des$Age == "Three",]$beta_t4
des$inv_full_t <- 1/exp(des$full_t)

des$full_mu <- des$beta0 + des$u
des[des$Age == "Five",]$full_mu <- des[des$Age == "Five",]$full_mu + des[des$Age == "Five",]$beta3
des[des$Age == "Three",]$full_t <- des[des$Age == "Three",]$full_mu + des[des$Age == "Three",]$beta4
