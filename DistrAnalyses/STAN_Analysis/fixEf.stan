data {
  int<lower=1> N;
  real rt[N];
  real<lower=-1,upper=1> factor1[N];
  real<lower=-1,upper=1> factor2[N];
  real<lower=-1,upper=1> factor3[N];
  real<lower=-1,upper=1> factor4[N];
  real<lower=-1,upper=1> factor5[N];
  real<lower=-1,upper=1> factor6[N];
  real<lower=-1,upper=1> factor6a[N];
  real<lower=-1,upper=1> factor7[N];
  real<lower=-1,upper=1> factor7a[N];
  real<lower=-1,upper=1> factor8[N];
  real<lower=-1,upper=1> factor8a[N];
  int<lower=1> J; // num of subject
  int<lower=1,upper=J> Subj[N]; //list of subject
}

parameters {
  vector[J] u; //subject intercepts for mean
  vector[J] u_t; //subject intercepts for tau
<<<<<<< Updated upstream
//  vector[J] u_s; //subject intercepts for sigma
=======
  vector[J] u_s; //subject intercepts for sigma
>>>>>>> Stashed changes
  vector[12] beta;
  vector[12] beta_t;
  vector[12] beta_s;
  real<lower=0> sigma_u;
  real<lower=0> sigma_u_t;
<<<<<<< Updated upstream
//  real<lower=0> sigma_u_s;
=======
  real<lower=0> sigma_u_s;
>>>>>>> Stashed changes
//  real<lower=0> sigma_e;
//  real<lower=0> lambda;
}
transformed parameters {
//  real<lower = 0> tau;
//  tau <- inv(lambda);
<<<<<<< Updated upstream
  vector[N] sigma_vector;
  real<lower=0> sigma_e
   for (i in 1:N){
  sigma_e <- beta_s[1] + beta_s[2] * factor1[i]+ beta_s[3] * factor2[i]+ beta_s[4] * factor3[i] + beta_s[5] * factor4[i] + 
  				beta_s[6] * factor5[i]+ beta_s[7] * factor6[i] + beta_s[8] * factor6a[i] + beta_s[9] * factor7[i]+ 
  				beta_s[10] * factor7a[i] + beta_s[11] * factor8[i] + beta_s[12] * factor8a[i];// + u_s[Subj[i]];
  	sigma_vector[i] <- sigma_e;     
   }
=======
//  real<lower=0> sigma_e_bound[N];
//  sigma_e_bound <- sigma_e;
//  real<lower=0> sigma_e_tr[N];
//  real mu[N];
//  real lambda;
//  real tau[N];
//  for (i in 1:N){
//     mu[i] <- beta[1] + beta[2] * factor1[i]+ beta[3] * factor2[i]+ beta[4] * factor3[i] +  beta[5] * factor4[i] + 
//  				beta[6] * factor5[i] + beta[7] * factor6[i] + beta[8] * factor6a[i] + beta[9] * factor7[i]+ 
//  				beta[10] * factor7a[i] + beta[11] * factor8[i] + beta[12] * factor8a[i] + u[Subj[i]];
//    lambda <- beta_t[1] + beta_t[2] * factor1[i]+ beta_t[3] * factor2[i]+ beta_t[4] * factor3[i] + beta_t[5] * factor4[i] + 
 // 				beta_t[6] * factor5[i]+ beta_t[7] * factor6[i] + beta_t[8] * factor6a[i] + beta_t[9] * factor7[i]+ 
//  				beta_t[10] * factor7a[i] + beta_t[11] * factor8[i] + beta_t[12] * factor8a[i];// + u_t[Subj[i]];
//
//    sigma_e_tr[i] <- fabs(beta_s[1] + beta_s[2] * factor1[i]+ beta_s[3] * factor2[i]+ beta_s[4] * factor3[i] + beta_s[5] * factor4[i] + 
//  				beta_s[6] * factor5[i]+ beta_s[7] * factor6[i] + beta_s[8] * factor6a[i] + beta_s[9] * factor7[i]+ 
//  				beta_s[10] * factor7a[i] + beta_s[11] * factor8[i] + beta_s[12] * factor8a[i]);// + u_s[Subj[i]];  			
//    tau[i] <- inv(lambda);
//  }
>>>>>>> Stashed changes
}
model {
  
  real mu[N];
  real lambda;
<<<<<<< Updated upstream
  real tau;
  real sigma_e;
  vector[N] mu_vector;
  vector[N] tau_vector;
=======
  real tau[N];
  real sigma_e[N];
>>>>>>> Stashed changes
  beta ~ double_exponential(0,10);
  beta_t ~ double_exponential(0,10);
  beta_s ~ double_exponential(0,10);
  u ~ double_exponential(0,sigma_u);
  u_t ~ double_exponential(0,sigma_u_t);
<<<<<<< Updated upstream
  //u_s ~ normal(0,sigma_u_s);
=======
  u_s ~ double_exponential(0,sigma_u_s);
>>>>>>> Stashed changes
  for (i in 1:N){
  mu[i] <- beta[1] + beta[2] * factor1[i]+ beta[3] * factor2[i]+ beta[4] * factor3[i] +  beta[5] * factor4[i] + 
  				beta[6] * factor5[i] + beta[7] * factor6[i] + beta[8] * factor6a[i] + beta[9] * factor7[i]+ 
  				beta[10] * factor7a[i] + beta[11] * factor8[i] + beta[12] * factor8a[i] + u[Subj[i]];
<<<<<<< Updated upstream
  mu_vector[i] <- mu;
  lambda <- beta_t[1] + beta_t[2] * factor1[i]+ beta_t[3] * factor2[i]+ beta_t[4] * factor3[i] + beta_t[5] * factor4[i] + 
  				beta_t[6] * factor5[i]+ beta_t[7] * factor6[i] + beta_t[8] * factor6a[i] + beta_t[9] * factor7[i]+ 
  				beta_t[10] * factor7a[i] + beta_t[11] * factor8[i] + beta_t[12] * factor8a[i];// + u_t[Subj[i]];
  				
//  sigma_e <- beta_s[1] + beta_s[2] * factor1[i]+ beta_s[3] * factor2[i]+ beta_s[4] * factor3[i] + beta_s[5] * factor4[i] + 
//  				beta_s[6] * factor5[i]+ beta_s[7] * factor6[i] + beta_s[8] * factor6a[i] + beta_s[9] * factor7[i]+ 
//  				beta_s[10] * factor7a[i] + beta_s[11] * factor8[i] + beta_s[12] * factor8a[i];// + u_s[Subj[i]];  				
  tau <- inv(lambda);
  tau_vector[i] <- tau;
//  if (sigma_e <0){
//  	sigma_e <- 10^-8;
//  	}
  }
  rt ~ exp_mod_normal(mu_vector,sigma_e_vector,tau_vector);

=======
  lambda <- fabs(beta_t[1] + beta_t[2] * factor1[i]+ beta_t[3] * factor2[i]+ beta_t[4] * factor3[i] + beta_t[5] * factor4[i] + 
  				beta_t[6] * factor5[i]+ beta_t[7] * factor6[i] + beta_t[8] * factor6a[i] + beta_t[9] * factor7[i]+ 
  				beta_t[10] * factor7a[i] + beta_t[11] * factor8[i] + beta_t[12] * factor8a[i] + u_t[Subj[i]]);
  sigma_e[i] <- fabs(beta_s[1] + beta_s[2] * factor1[i]+ beta_s[3] * factor2[i]+ beta_s[4] * factor3[i] + beta_s[5] * factor4[i] + 
  				beta_s[6] * factor5[i]+ beta_s[7] * factor6[i] + beta_s[8] * factor6a[i] + beta_s[9] * factor7[i]+ 
  				beta_s[10] * factor7a[i] + beta_s[11] * factor8[i] + beta_s[12] * factor8a[i] + u_s[Subj[i]]);  				
  tau[i] <- inv(lambda);
  }
  rt ~ exp_mod_normal(mu,sigma_e,tau);
>>>>>>> Stashed changes
}