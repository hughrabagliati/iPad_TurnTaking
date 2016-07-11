data {
  int<lower=1> N;
  real rt[N];

  int<lower=1> J; // num of subject
  int<lower=1,upper=J> Subj[N]; //list of subject
}

parameters {
  real u_e1; //subject intercepts for mean
  vector[J] u_e2; //subject intercepts for mean
  real u_t_e1; //subject intercepts for tau
  vector[J] u_t_e2; //subject intercepts for tau
  real u_s_e1; //subject intercepts for sigma
  vector[J] u_s_e2; //subject intercepts for sigma

  vector[1] beta;
  vector[1] beta_t;
  vector[1] beta_s;
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
  vector[J] u; //subject intercepts for mean
  vector[J] u_t; //subject intercepts for tau
  vector[J] u_s; //subject intercepts for sigma

  u <- u_e1 * u_e2;  
  u_t <- u_t_e1 * u_t_e2;  
  u_s <- u_s_e1 * u_s_e2;  
  for (i in 1:N){

    mu[i] <- beta[1]+ u[Subj[i]];
    lambda[i] <- beta_t[1] + u_t[Subj[i]];
    sigma_e[i] <- beta_s[1] + u_s[Subj[i]];  	 		
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

 // for (i in 2:3){ 
 //   beta[i] ~ cauchy(0,10);
 //   beta_t[i] ~ cauchy(0,10);
 //   beta_s[i] ~ cauchy(0,10); 
 // }
  u_e2 ~ normal(0,1);
  u_e1 ~ cauchy(0,2.5);
  u_t_e2 ~ normal(0,1);
  u_t_e1 ~ cauchy(0,2.5);
  u_s_e2 ~ normal(0,1);
  u_s_e1 ~ cauchy(0,2.5);
  

  //  u_t ~ normal(0,sigma_u_t);
  //u_s ~ normal(0,sigma_u_s);
  


   rt ~ exp_mod_normal(mu,sigma_e,tau);
//}
}