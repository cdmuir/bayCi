data {
  
  // Empty chamber data
  int<lower=0> n_empty;
  vector[n_empty] A_empty;
  vector[n_empty] Pci_empty;
  
  // RACiR data
  
}
parameters {
  
  // Correction curve
  real b0;
  real b1;
  real<lower=0> sigma_empty;
  
}
model {
  
  // Empty chamber priors
  b0 ~ normal(0, 1);
  b1 ~ normal(0, 10);
  sigma_empty ~ cauchy(0, 1);
  A_empty ~ normal(b0 + b1 * Pci_empty, sigma_empty);
  
}

