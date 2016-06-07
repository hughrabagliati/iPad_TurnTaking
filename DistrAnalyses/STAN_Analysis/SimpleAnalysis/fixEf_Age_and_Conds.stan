data {
  int<lower=1> N;
  real rt[N];
  real<lower=-1,upper=1> factor1[N];
  real<lower=-1,upper=1> factor2[N];
  real<lower=-1,upper=1> factor3[N];
  real<lower=-1,upper=1> factor4[N];
  int<lower=1> J; // num of subject
  int<lower=1,upper=J> Subj[N]; //list of subject
}

parameters {
  vector[J] u; //subject intercepts for mean
//  vector[J] u_t; //subject intercepts for tau
//  vector[J] u_s; //subject intercepts for sigma
  vector[5] beta;
  vector[5] beta_t;
  vector[5] beta_s;
  
  real<lower=0> sigma_u;
//  real<lower=0> sigma_u_t;
//  real<lower=0> sigma_u_s;

  
}
transformed parameters {
  //  real<lower = 0> tau;
//  tau <- inv(lambda);
}

model {
  real mu;
  real lambda;
  real tau;
  real sigma_e;
  
<<<<<<< Updated upstream
  // priors on beta
  beta ~ double_exponential(0,10);
  beta_t ~ double_exponential(0,10);
  beta_s ~ double_exponential(0,10); 

=======
  // priors on intercepts
  beta[1] ~ double_exponential(800,10);
  beta_t[1] ~ cauchy(0,10);
  beta_s[1] ~ cauchy(0,10); 
  
  // priors on predictor Betas
  for (i in 2:5){ 
    beta[i] ~ double_exponential(0,10);
    beta_t[i] ~ double_exponential(0,10);
    beta_s[i] ~ double_exponential(0,10); 
  }
  
  // priors on by subject effects
  //  sigma_u ~ normal(0,1);
  //  sigma_u_t ~ normal(0,1);
  //  sigma_u_s ~ normal(0,1);
>>>>>>> Stashed changes
  u ~ normal(0,sigma_u);
//  u_t ~ normal(0,sigma_u_t);
  //u_s ~ normal(0,sigma_u_s);

  for (i in 1:N){
    mu <- beta[1] + beta[2] * factor1[i]+ beta[3] * factor2[i] + beta[4] * factor3[i]+ beta[5] * factor4[i] + u[Subj[i]];
    lambda <- beta_t[1] + beta_t[2] * factor1[i]+ beta_t[3] * factor2[i] + beta_t[4] * factor3[i]+ beta_t[5] * factor4[i];
    sigma_e <- beta_s[1] + beta_s[2] * factor1[i]+ beta_s[3] * factor2[i] + beta_s[4] * factor3[i]+ beta_s[5] * factor4[i];  		
    tau <- inv(lambda);
    rt[i] ~ exp_mod_normal(mu,sigma_e,lambda);
  }
}