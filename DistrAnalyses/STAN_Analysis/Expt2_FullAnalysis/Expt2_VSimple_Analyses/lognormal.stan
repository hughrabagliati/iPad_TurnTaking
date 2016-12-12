data {
  int<lower=1> N;
  real rt[N];
  real<lower=-1,upper=1> factor1[N];
  real<lower=-1,upper=1> factor2[N];
  // real<lower=-1,upper=1> factor3[N];
  //  real<lower=-1,upper=1> factor4[N];
  real<lower=-1,upper=1> factor5[N];
  //  real<lower=-1,upper=1> factor6[N];
  //  real<lower=-1,upper=1> factor6a[N];
  //  real<lower=-1,upper=1> factor7[N];
  //  real<lower=-1,upper=1> factor7a[N];
  //  real<lower=-1,upper=1> factor8[N];
  //  real<lower=-1,upper=1> factor8a[N];
  //  real factor9[N];
  int<lower=1> J; // num of subject
  int<lower=1,upper=J> Subj[N]; //list of subject
}

parameters {
  real beta0;
//  real beta_t0;
  real beta_s0;
  
  vector[3] beta;
  //vector[3] beta_t;
  vector[3] beta_s;
  
  real u_e1; //subject intercepts for mean
  vector[J] u_e2; //subject intercepts for mean
 // real u_t_e1; //subject intercepts for tau
//  vector[J] u_t_e2; //subject intercepts for tau
  real u_s_e1; //subject intercepts for sigma
  vector[J] u_s_e2; //subject intercepts for sigma
  
  
  real<lower=0> sigma_u_1;
  //real<lower=0> sigma_u_t_1;
  real<lower=0> sigma_u_s_1;
  
  
  // beta variance priors
  real<lower=0> sigma_beta_1;
  //real<lower=0> sigma_beta_t_1;
  real<lower=0> sigma_beta_s_1;   
  
  //  real<lower=0> sigma_beta_continuous;
  //  real<lower=0> sigma_beta_t_continuous;
  //  real<lower=0> sigma_beta_s_continuous;   
  
  // beta0 variance priors
  real<lower=0> sigma_beta0_1;
  //real<lower=0> sigma_beta0_t_1;
  real<lower=0> sigma_beta0_s_1;     
  
  // for subtraction to stay positive
  //	real<lower=0> c_t;
  //	real<lower=0> c_s;
  
}



transformed parameters {
  real mu[N];
  //real<lower=0> lambda[N];
  //real tau[N];
  real<lower=0> sigma_e[N];
  
  
  vector[J] u; //subject intercepts for mean
  //vector[J] u_t; //subject intercepts for tau
  vector[J] u_s; //subject intercepts for sigma
  
  
  // build by subject intercepts. I don't know why, but this technique of multiplying
  // a vector by a scalar works to ensure that the model can be fit. If you just try to 
  // estimate by subject intercepts for sigma and tau otherwise, you get too many exceptions
  // due to impossible starting values, such as negative sigmas. 
  // I learned this trick from http://www.maths.bath.ac.uk/~jjf23/stan/
  
  u = u_e1 * u_e2;  
  //u_t = u_t_e1 * u_t_e2;  
  u_s = u_s_e1 * u_s_e2;   
  
  
  // build mu, lambda and sigma
  for (i in 1:N){
  mu[i] =    log1p_exp(beta0 + 
  beta[1] * factor1[i]+ 
  beta[2] * factor2[i]+ 
  //              beta[3] * factor3[i]+ 
  //             beta[3] * factor4[i] + 
  beta[3] * factor5[i] + 
  //  			     	beta[6] * factor6[i] + 
  //  			     	beta[5] * factor6a[i] + 
  //  			     	beta[8] * factor7[i] + 
  //  			     	beta[6] * factor7a[i] + 
  //  			     	beta[10] * factor8[i] + 
  //  			     	beta[7] * factor8a[i] + 
  //  			     	beta[8] * factor9[i] + 
  u[Subj[i]]); // maybe replace u here by u_e[Subj[i]] * u_e2 ?
  
  
  //lambda[i] = log1p_exp(beta_t0 + 
  //beta_t[1] * factor1[i]+ 
  //beta_t[2] * factor2[i]+ 
  //              beta_t[3] * factor3[i]+ 
  //              beta_t[3] * factor4[i] + 
  //beta_t[3] * factor5[i] + 
  //  			     	beta_t[6] * factor6[i] + 
  //  			     	beta_t[5] * factor6a[i] + 
  //  			     	beta_t[8] * factor7[i] + 
  //			     	beta_t[6] * factor7a[i] + 
  //  			     	beta_t[10] * factor8[i] + 
  //		     	beta_t[7] * factor8a[i] + 
  //  			     	beta_t[8] * factor9[i] + 
 // u_t[Subj[i]]) ; 				
  
  
  sigma_e[i] =  log1p_exp(beta_s0 + 
  beta_s[1] * factor1[i]+ 
  beta_s[2] * factor2[i]+ 
  //              beta_s[3] * factor3[i]+ 
  //              beta_s[3] * factor4[i] + 
  beta_s[3] * factor5[i] + 
  //  			     	beta_s[6] * factor6[i] + 
  //  			     	beta_s[5] * factor6a[i] + 
  //  			     	beta_s[8] * factor7[i] + 
  //  			     	beta_s[6] * factor7a[i] + 
  //  			     	beta_s[10] * factor8[i] + 
  //  			     	beta_s[7] * factor8a[i] + 
  //  			     	beta_s[12] * factor9[i] + 
  u_s[Subj[i]]); 
  
  
  //tau[i] =    inv(lambda[i]);
  }
  
}

model {



// priors on  variability in by subject intercepts
sigma_u_1 ~ normal(0,1);
//sigma_u_t_1 ~ normal(0,1);
sigma_u_s_1 ~ normal(0,1);

// priors on variability in beta
sigma_beta_1 ~ normal(0,1);
//sigma_beta_t_1 ~ normal(0,1);
sigma_beta_s_1 ~ normal(0,1);

// priors on variability in beta
sigma_beta0_1 ~ normal(0,1);
//sigma_beta0_t_1 ~ normal(0,1);
sigma_beta0_s_1 ~ normal(0,1);

// sigma_beta_continuous ~ normal(0,1);
//  sigma_beta_t_continuous ~ normal(0,1);
//  sigma_beta_t_continuous ~ normal(0,1);

// parameters for generating by subject intercepts
u_e2 ~ normal(0,sigma_u_1); // normal(0,1)
u_e1 ~ cauchy(0,2.5); //u_e1 ~ cauchy(0,sigma_u or 2.5);
//u_t_e2 ~ normal(0,sigma_u_t_1);
//u_t_e1 ~ cauchy(0,2.5);
u_s_e2 ~ normal(0,sigma_u_s_1);
u_s_e1 ~ cauchy(0,2.5);  



// priors on intercepts
beta0 ~ normal(0,sigma_beta0_1); //double_exponential
//beta_t0 ~ normal(0,sigma_beta0_t_1);
beta_s0 ~ normal(0,sigma_beta0_s_1);//cauchy(150,10); 

// priors on remaining betas. got worse fit when i estimated a separate variance parameter for each beta.
for (i in 1:3){ //11){ 
beta[i] ~ normal(0,sigma_beta_1);//cauchy(0,10);
//beta_t[i] ~ normal(0,sigma_beta_t_1);//cauchy(0,10);
beta_s[i] ~ normal(0,sigma_beta_s_1);//cauchy(0,10); 
}
//   beta[8] ~ normal(0,sigma_beta_continuous);//cauchy(0,10);
//    beta_t[8] ~ normal(0,sigma_beta_t_continuous);//cauchy(0,10);
//    beta_s[12] ~ normal(0,sigma_beta_s_continuous);//cauchy(0,10); 

//  estimate rts
rt ~ lognormal(mu,sigma_e);//,tau);
//}
}
