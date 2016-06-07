data {
  int<lower=1> N;
  real rt[N];
  real<lower=-1,upper=1> factor1[N];
  real<lower=-1,upper=1> factor2[N];
  int<lower=1> J; // num of subject
  int<lower=1,upper=J> Subj[N]; //list of subject
}

transformed data {
	// Uniform parameters
	real L;
	real U;

	L <- min(rt);
	U <- max(rt);
}

parameters {
  vector[J] u; //subject intercepts for mean
//  vector[J] u_t; //subject intercepts for tau
//  vector[J] u_s; //subject intercepts for sigma
  vector[3] beta;
  vector[3] beta_t;
  vector[3] beta_s;
  
  // Mixture weights
  simplex[2] alpha;
  
  real<lower=0> sigma_u;
//  real<lower=0> sigma_u_t;
//  real<lower=0> sigma_u_s;
//  real<lower=0> sigma_e;
//  real<lower=0> lambda;

}
transformed parameters {
//  
//  tau <- inv(lambda);
}
model {
  real mu;
  real lambda;
  real tau;
  real sigma_e;
  beta[1] ~ double_exponential(800,10);
  beta_t[1] ~ cauchy(0,2);
  beta_s[1] ~ cauchy(0,0.5); 
  for (i in 2:3){
    beta[i] ~ double_exponential(0,10);
    beta_t[i] ~ double_exponential(0,10);
    beta_s[i] ~ double_exponential(0,10); 
	}
  u ~ normal(0,sigma_u);
//  u_t ~ normal(0,sigma_u_t);
//  u_s ~ normal(0,sigma_u_s);
  alpha[2] ~ beta(0.01,0.9);
//  sigma_u ~ normal(0,1);
//  sigma_u_t ~ normal(0,1);
  for (i in 1:N){
  mu <- beta[1] + beta[2] * factor1[i]+ beta[3] * factor2[i]+ u[Subj[i]];
  lambda <- beta_t[1] + beta_t[2] * factor1[i]+ beta_t[3] * factor2[i]; //+ u_t[Subj[i]];
  sigma_e <- beta_s[1] + beta_s[2] * factor1[i]+ beta_s[3] * factor2[i];// + u_s[Subj[i]];  		
  tau <- inv(lambda);
  //rt[i] ~ exp_mod_normal(mu,sigma_e,tau);
  increment_log_prob(log_sum_exp(log(alpha[1]) 
		+ exp_mod_normal_log(rt[i], mu, sigma_e, lambda),
		log(alpha[2]) + uniform_log(rt[i], L, U)));
  //increment_log_prob(exp_mod_normal_log(rt[i], mu, sigma_e, lambda));
  }
}

