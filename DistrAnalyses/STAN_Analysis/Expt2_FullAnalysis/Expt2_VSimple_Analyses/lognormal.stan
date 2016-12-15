data {
  int<lower=1> N;
  real rt[N];
  real<lower=-1,upper=1> factor1[N];
  real<lower=-1,upper=1> factor2[N];
  real<lower=-1,upper=1> factor3[N];
  real<lower=-1,upper=1> factor5[N];
  int<lower=1> J; // num of subject
  int<lower=1,upper=J> Subj[N]; //list of subject
}

parameters {
  real beta0;
  real beta_s0;
  
  vector[4] beta;
  vector[4] beta_s;
  
  real u_e1; //subject intercepts for mean
  vector[J] u_e2; //subject intercepts for mean
  real u_s_e1; //subject intercepts for sigma
  vector[J] u_s_e2; //subject intercepts for sigma
  
  real<lower=0> sigma_u_1;
  real<lower=0> sigma_u_s_1;
  
  
  // beta variance priors
  real<lower=0> sigma_beta_1;
  real<lower=0> sigma_beta_s_1;   
  
  // beta0 variance priors
 // real<lower=0> sigma_beta0_1;
//  real<lower=0> sigma_beta0_s_1;     
  

}



transformed parameters {
  real mu[N];
  real<lower=0> sigma_e[N];

  vector[J] u; //subject intercepts for mean
  vector[J] u_s; //subject intercepts for sigma
  
  
  // build by subject intercepts. I don't know why, but this technique of multiplying
  // a vector by a scalar works to ensure that the model can be fit. If you just try to 
  // estimate by subject intercepts for sigma and tau otherwise, you get too many exceptions
  // due to impossible starting values, such as negative sigmas. 
  // I learned this trick from http://www.maths.bath.ac.uk/~jjf23/stan/
  
  u = u_e1 * u_e2;  
  u_s = u_s_e1 * u_s_e2;   
  
  
  // build mu, lambda and sigma
  for (i in 1:N){
  mu[i] =    (beta0 + 
  beta[1] * factor1[i]+ 
  beta[2] * factor2[i]+ 
  beta[3] * factor3[i]+ 
  beta[4] * factor5[i]  + 
  u[Subj[i]]); 
  
  		
  
  
  sigma_e[i] =  log1p_exp((beta_s0 +
  beta_s[1] * factor1[i]+
  beta_s[2] * factor2[i]+
  beta_s[3] * factor3[i]+
  beta_s[4] * factor5[i] +
  u_s[Subj[i]]));
  
  
  }
  
}

model {



// priors on  variability in by subject intercepts
sigma_u_1 ~ normal(0,1);
sigma_u_s_1 ~ normal(0,1);

// priors on variability in beta
sigma_beta_1 ~ normal(0,1);
sigma_beta_s_1 ~ normal(0,1);

// priors on variability in beta
//sigma_beta0_1 ~ normal(0,1);
//sigma_beta0_s_1 ~ normal(0,1);


// parameters for generating by subject intercepts
u_e2 ~ normal(0,sigma_u_1); // normal(0,1)
u_e1 ~ cauchy(0,2.5); //u_e1 ~ cauchy(0,sigma_u or 2.5);
u_s_e2 ~ normal(0,sigma_u_s_1);
u_s_e1 ~ cauchy(0,2.5);  



// priors on intercepts
beta0 ~ normal(0,sigma_beta_1); //double_exponential
beta_s0 ~ normal(0,sigma_beta_s_1);//cauchy(150,10); 

// priors on remaining betas. got worse fit when i estimated a separate variance parameter for each beta.
for (i in 1:4){ //11){ 
beta[i] ~ normal(0,sigma_beta_1);//cauchy(0,10);
beta_s[i] ~ normal(0,sigma_beta_s_1);//cauchy(0,10); 
}

//  estimate rts
#for (i in 1:N){
rt ~ normal(mu,sigma_e);//,tau);
#}
//}
}
