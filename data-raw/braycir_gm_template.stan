// This is simply a copy of braycir_template.stan as a placeholder. Does not work.
data {
  
  // Empty chamber data
  int<lower=0> n_empty;
  vector[n_empty] A_empty;
  vector[n_empty] Cr_empty;
  vector[n_empty] Cr2_empty;
  
  // Fixed values
  real gamma_star;
  real Km;

  // RACiR data
  int<lower=0> n_data;
  vector[n_data] A_data;   // uncorrected A [umol / m^2 / s]
  vector[n_data] Cr_data;  // Reference CO2 [umol / mol]
  vector[n_data] Cr2_data; // Reference CO2 [umol / mol], squared
  vector[n_data] Cs_data;  // Sensor CO2 [umol / mol]
  vector[n_data] E_data;   // Evaporation [mol / m^2 / s]
  vector[n_data] gsc_data; // Stomatal conductance to CO2 [mol / m^2 / s]
  vector[n_data] gtc_data; // Total conductance to CO2 [mol / m^2 / s]
  vector[n_data] Pa_data;  // Atmospheric pressure kPa

}
parameters {
  
  // Correction curve parameters
  parameters_b0
  parameters_b1
  parameters_b2
  parameters_sigma_empty
  parameters_phi
  
  // ACi curve parameters
  // real<lower=0> gamma_star;
  parameters_J
  // real<lower=0> Km;
  // parameters_Rd
  real<lower=0> Rd;
  parameters_Vcmax
  real<lower=0> sigma_data;
  simplex[2] w; // mixing proportions

}
transformed parameters{

  // Regression with correlated residuals, adopted from:
  // https://github.com/nwfsc-timeseries/atsar
  vector[n_empty] epsilon;
  vector[n_empty] pred;
  real sigma_cor;
  pred[1] = b0 + b1 * Cr_empty[1] + b2 * Cr2_empty[1];
  epsilon[1] = A_empty[1] - pred[1];
  for(i in 2:n_empty) {
    pred[i] = b0 + b1 * Cr_empty[i] + b2 * Cr2_empty[i];
    epsilon[i] = (A_empty[i] - pred[i]) - phi * epsilon[i - 1];
  }
  // Var = sigma2 * (1-rho^2)
  sigma_cor = sqrt(sigma_empty * sigma_empty * (1 - phi * phi));
  
}
model {
  
  // Finite mixture based on: https://mc-stan.org/docs/2_20/stan-users-guide/mixture-modeling-chapter.html
  
  vector[2] log_w = log(w);
  // DESCRIBE
  // real a;
  // real bJ;
  // real cJ;
  // real bV;
  // real cV;
  // vector[n_data] Am;
  vector[n_data] Ac;
  vector[n_data] Aj;

  vector[n_data] A_corrected;
  vector[n_data] Ci_corrected;
  // vector[n_data] Pci_corrected;
  
  // Empty chamber priors
  priors_b0
  priors_b1
  priors_b2
  phi ~ normal(0, 1);
  sigma_empty ~ cauchy(0, 1);
  
  // ACi curve priors
  // gamma_star ~ normal(35.91, 0.1);
  // Km ~ normal(661.453, 1);
  priors_J
  // priors_Rd
  Rd ~ normal(0, 10);
  priors_Vcmax
  sigma_data ~ cauchy(0, 1);

  // Correct A
  A_corrected = A_data - (b0 + b1 * Cr_data + b2 * Cr2_data);
  
  for (i in 1:n_data) {
    
    vector[2] lps = log_w;
    
    // Eq. C-16 (page C-11) of LI6800 manual
    // Converted to Pa by multiplying by (Pa_data / 1000)
    // 
    // Pci_corrected[i] = (Pa_data[i] / 1000) *
    //   ((gtc_data[i] - E_data[i] / 2) * Cs_data[i] - A_corrected[i]) /
    //   (gtc_data[i] + E_data[i] / 2);
    Ci_corrected[i] = ((gtc_data[i] - E_data[i] / 2) *
      Cs_data[i] - A_corrected[i]) /
      (gtc_data[i] + E_data[i] / 2);
    
    // Is this necessary?
    // if (Ci_corrected[i] <= gamma_star) Ci_corrected[i] = gamma_star;
    
    // Infinite g_mc
    Ac[i] = Vcmax * (Ci_corrected[i] - gamma_star) / (Ci_corrected[i] + Km);
    Aj[i] = (J / 4) * (Ci_corrected[i] - gamma_star) / 
      (Ci_corrected[i] + 2 * gamma_star);
    lps[1] += normal_lpdf(A_corrected[i] | Ac[i] - Rd, sigma_data);
    lps[2] += normal_lpdf(A_corrected[i] | Aj[i] - Rd, sigma_data);
    target += log_sum_exp(lps);
    
    // Estimate g_mc
    // a = -1 / gmc;
    // bJ = (J / 4 - Rd) / gmc + Ci_corrected[i] + 2 * gamma_star;
    // cJ = Rd * (Ci_corrected[i] + 2 * gamma_star) -
    //   J / 4 * (Ci_corrected[i] - gamma_star);
    // 
    // bV = (Vcmax - Rd) / gmc + Ci_corrected[i] + Km;
    // cV = Rd * (Ci_corrected[i] + Km) -
    //   Vcmax * (Ci_corrected[i] - gamma_star);
    // 
    // Ac[i] = (sqrt(bV^2 - 4 * a * cV) - bV) / (2 * a) + Rd;
    // Aj[i] = (sqrt(bJ^2 - 4 * a * cJ) - bJ) / (2 * a) + Rd;
    
    // Am[i] = (Ac[i] + Aj[i] - sqrt((Ac[i] + Aj[i]) ^ 2) - 4 * theta * Ac[i] * Aj[i]) / (2 * theta) - Rd;

  }

  // Likelihood
  A_empty ~ normal(pred, sigma_cor);
  // A_corrected ~ normal(Ac , sigma_data);
  
}
generated quantities {
  
  vector[n_data] predict_Ac;
  vector[n_data] predict_Aj;
  vector[n_data] predict_Am;
  vector[n_data] A_corrected;
  vector[n_data] Ci_corrected;

  A_corrected = A_data - (b0 + b1 * Cr_data + b2 * Cr2_data);

  for (i in 1:n_data) {
    
    Ci_corrected[i] = ((gtc_data[i] - E_data[i] / 2) *
      Cs_data[i] - A_corrected[i]) /
      (gtc_data[i] + E_data[i] / 2);
    
    // Is this necessary?
    // if (Ci_corrected[i] <= gamma_star) Ci_corrected[i] = gamma_star;
    
    // Infinite g_mc
    predict_Ac[i] = Vcmax * (Ci_corrected[i] - gamma_star) / 
      (Ci_corrected[i] + Km) - Rd;
    predict_Aj[i] = (J / 4) * (Ci_corrected[i] - gamma_star) / 
      (Ci_corrected[i] + 2 * gamma_star) - Rd;
    predict_Am[i] = fmin(predict_Ac[i], predict_Aj[i]);

  }

}