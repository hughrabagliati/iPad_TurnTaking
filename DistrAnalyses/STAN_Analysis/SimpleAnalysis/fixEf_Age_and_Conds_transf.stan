data {
  int<lower=1> N;
  real rt[N];
  real<lower=-1,upper=1> factor1[N];
  real<lower=-1,upper=1> factor2[N];
//  real<lower=-1,upper=1> factor3[N];
//  real<lower=-1,upper=1> factor4[N];
  int<lower=1> J; // num of subject
  int<lower=1,upper=J> Subj[N]; //list of subject
}

parameters {
  vector[J] u; //subject intercepts for mean
  vector[J] u_t; //subject intercepts for tau
  vector[J] u_s; //subject intercepts for sigma
  vector[3] beta;
  vector[3] beta_t;
  vector[3] beta_s;
//  real mu;
//  real<lower=0> lambda;
//  real tau;
//  real<lower=0> sigma_e;

//  real<lower=0> sigma_u;
  //  real<lower=0> sigma_u_t;
  //  real<lower=0> sigma_u_s;
  
  
}
transformed parameters {
  real mu[N];
  real lambda[N];
  real tau[N];
  real sigma_e[N];
  
  for (i in 1:N){
    mu[i] <- beta[1]+ u[Subj[i]]+ beta[2] * factor1[i]+ beta[3] * factor2[i];// + beta[4] * factor3[i]+ beta[5] * factor4[i]; //+ u[Subj[i]];
    lambda[i] <- beta_t[1] + beta_t[2] * factor1[i]+ beta_t[3] * factor2[i];// + beta_t[4] * factor3[i]+ beta_t[5] * factor4[i];// + u_t[Subj[i]];
    sigma_e[i] <- beta_s[1] + beta_s[2] * factor1[i]+ beta_s[3] * factor2[i];// + beta_s[4] * factor3[i]+ beta_s[5] * factor4[i];// + u_s[Subj[i]];  		
  tau[i] <- inv(lambda[i]);
  }
}

model {


  // priors on beta
  beta[1] ~ cauchy(800,10);
  beta_t[1] ~ cauchy(0,10);
  beta_s[1] ~ cauchy(150,10); 

//  mu ~ cauchy(0,10);
//  lambda ~ cauchy(0,10);
//  sigma_e ~ cauchy(0,10); 

  for (i in 2:3){ 
    beta[i] ~ cauchy(0,10);
    beta_t[i] ~ cauchy(0,10);
    beta_s[i] ~ cauchy(0,10); 
  }
  
  //u ~ normal(0,sigma_u);
  //  u_t ~ normal(0,sigma_u_t);
  //u_s ~ normal(0,sigma_u_s);
  


   rt ~ exp_mod_normal(mu,sigma_e,tau);
//}
}