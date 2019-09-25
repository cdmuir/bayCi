data {
  
  // Empty chamber data
  int<lower=0> n_empty;
  int<lower=1> Q_empty;
  vector[n_empty] A_empty;
  vector[n_empty] Cr_empty;
  
  // RACiR data
  // int<lower=0> n_data;
  // vector[n_data] A_data;   // uncorrected A [umol / m^2 / s] 
  // vector[n_data] Cr_data;  // Reference CO2 [umol / mol] 
  // vector[n_data] Cs_data;  // Sensor CO2 [umol / mol] 
  // vector[n_data] E_data;   // Evaporation [mol / m^2 / s]
  // vector[n_data] gsc_data; // Stomatal conductance to CO2 [mol / m^2 / s]
  // vector[n_data] gtc_data; // Total conductance to CO2 [mol / m^2 / s]
  // vector[n_data] Pa_data;  // Atmospheric pressure kPa
  // vector[n_data] Pcr_data; // Reference CO2 [Pa]
  
}
parameters {
  
  // Correction curve parameters
  real b0;
  real b1;
  real<lower=0> sigma_empty;
  vector[Q_empty] theta; // lag coefficients
  
  // ACi curve parameters
  // real<lower=0> gamma_star;
  // // real log_gmc;
  // real<lower=0> J;
  // real<lower=0> Km;
  // // real log_Rd;
  // real<lower=0> Rd;
  // real<lower=0> Vcmax;
  // real<lower=0> sigma_data;

}
transformed parameters{

  // real gmc;
  // real Rd;
  // gmc = exp(log_gmc);
  // Rd = exp(Rd);
  vector[n_empty] epsilon; // error terms
  for (t in 1:n_empty) {
    epsilon[t] = A_empty[t] - (b0 + b1 * Cr_empty[t]);
    for (q in 1:min(t - 1, Q_empty))
      epsilon[t] = epsilon[t] - theta[q] * epsilon[t - q];
  }
}
model {
  
  vector[n_empty] eta;

  // DESCRIBE
  // real a;
  // real bJ;
  // real cJ;
  // real bV;
  // real cV;
  // vector[n_data] Am;
  // vector[n_data] Ac;
  // vector[n_data] Aj;

  // real theta;
  // 
  // vector[n_data] A_corrected;
  // vector[n_data] Ci_corrected;
  // vector[n_data] Pci_corrected;
  
  // Empty chamber priors
  b0 ~ normal(0, 1);
  b1 ~ normal(0, 10);
  sigma_empty ~ cauchy(0, 1);
  
  // ACi curve priors
  // gamma_star ~ normal(35.91, 0.1);
  // log_gmc ~ normal(0, 4);
  // Km ~ normal(661.453, 1);
  // Vcmax ~ normal(117.5, 20);
  // J ~ normal(224.4, 20);
  // log_Rd ~ normal(0, 1);
  // Rd ~ normal(0, 10);
  // sigma_data ~ cauchy(0, 1);

  // Correct A_data and Pci_data (NEED TO CHECK EQUATIONS)
  // A_corrected = A_data - (b0 + b1 * Cr_data);
  
  // theta= 0.9999;

  for (t in 1:n_empty) {
    eta[t] = b0 + b1 * Cr_empty[t];
    for (q in 1:min(t - 1, Q_empty))
      eta[t] = eta[t] + theta[q] * epsilon[t - q];
  }
  
  // NEED TO CHECK EQUATIONS
  // for (i in 1:n_data) {

    // Eq. C-16 (page C-11) of LI6800 manual
    // Converted to Pa by multiplying by (Pa_data / 1000)
    // 
    // Pci_corrected[i] = (Pa_data[i] / 1000) *
    //   ((gtc_data[i] - E_data[i] / 2) * Cs_data[i] - A_corrected[i]) /
    //   (gtc_data[i] + E_data[i] / 2);
    // Ci_corrected[i] = ((gtc_data[i] - E_data[i] / 2) *
    //   Cs_data[i] - A_corrected[i]) /
    //   (gtc_data[i] + E_data[i] / 2);
    
    // Is this necessary?
    // if (Ci_corrected[i] <= gamma_star) Ci_corrected[i] = gamma_star;
    
    // Infinite g_mc
    // Ac[i] = Vcmax * (Ci_corrected[i] - gamma_star)/(Ci_corrected[i] + Km);
    // Aj[i] = (J / 4) * (Ci_corrected[i] - gamma_star)/(Ci_corrected[i] + 2 * gamma_star);
    
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

  // }

  // Likelihood
  A_empty ~ normal(eta, sigma_empty);
  // A_corrected ~ normal(Am , sigma_data);
  
}
generated quantities {
  
  // vector[n_data] A_corrected;
  // vector[n_data] Ci_corrected;

  // A_corrected = A_data - (b0 + b1 * Cr_data);
    
  // for (i in 1:n_data) {
  //   Ci_corrected[i] = ((gtc_data[i] - E_data[i] / 2) * 
  //     Cs_data[i] - A_corrected[i]) /
  //     (gtc_data[i] + E_data[i] / 2);
  // }

}
