data {
  
  // Empty chamber data
  int<lower=0> n_empty;
  vector[n_empty] A_empty;
  vector[n_empty] Pcr_empty;
  
  // RACiR data
  int<lower=0> n_data;
  vector[n_data] A_data;   // uncorrected A [umol / m^2 / s] 
  vector[n_data] Cs_data;  // Sensor CO2 [umol / mol] 
  vector[n_data] E_data;   // Evaporation [mol / m^2 / s]
  vector[n_data] gsc_data; // Stomatal conductance to CO2 [mol / m^2 / s]
  vector[n_data] gtc_data; // Total conductance to CO2 [mol / m^2 / s]
  vector[n_data] Pa_data;  // Atmospheric pressure kPa
  vector[n_data] Pcr_data; // Reference CO2 [Pa]
}
parameters {
  
  // Correction curve parameters
  real b0;
  real b1;
  real<lower=0> sigma_empty;
  
  // ACi curve parameters
  real gamma_star;
  real log_gmc;
  real J;
  real Km;
  real log_Rd;
  real Vcmax;
  real<lower=0> sigma_data;

}
transformed parameters{

  real gmc;
  real Rd;
  gmc = exp(log_gmc);
  Rd = exp(Rd);

}
model {
  
  // DESCRIBE
  vector[n_data] cJ;
  vector[n_data] bJ;
  vector[n_data] aJ;
  vector[n_data] cV;
  vector[n_data] bV;
  vector[n_data] aV;
  vector[n_data] mu;
  vector[n_data] muV;
  vector[n_data] muJ;

  vector[n_data] A_corrected;
  vector[n_data] Pci_corrected;
  
  // Empty chamber priors
  b0 ~ normal(0, 1);
  b1 ~ normal(0, 10);
  sigma_empty ~ cauchy(0, 1);
  
  // ACi curve priors
  gamma_star ~ normal(3.743, 0.1);
  log_gmc ~ normal(0, 4);
  Km ~ normal(62.067, 1);
  Vcmax ~ normal(30, 20);
  J ~ normal(50, 20);
  log_Rd ~ normal(1.39, 1.39);
  sigma_data ~ cauchy(0, 1);

  // Correct A_data and Pci_data (NEED TO CHECK EQUATIONS)
  A_corrected = A_data - (b0 + b1 * Pcr_data);
  
  // NEED TO CHECK EQUATIONS
  for (i in 1:n_data) {

    // Eq. C-16 (page C-11) of LI6800 manual
    // Converted to Pa by multiplying by (Pa_data / 1000)
    // 
    Pci_corrected[i] = (Pa_data[i] / 1000) *
      ((gtc_data[i] - E_data[i] / 2) * Cs_data[i] - A_corrected[i]) /
      (gtc_data[i] + E_data[i] / 2);

    cJ[i] = Rd * (Pci_corrected[i] + 2 * gamma_star) -
      J / 4 * (Pci_corrected[i] - gamma_star);
    bJ[i] = (J / 4 - Rd) / gmc + Pci_corrected[i] + 2 * gamma_star;
    aJ[i] = -1 / gmc;

    cV[i] = Rd * (Pci_corrected[i] + Km) -
      Vcmax * (Pci_corrected[i] - gamma_star);
    bV[i] = (Vcmax - Rd) / gmc + Pci_corrected[i] + Km;
    aV[i] = -1 / gmc;

    muV[i] = (sqrt(bV[i]^2 - 4 * aV[i] * cV[i]) - bV[i])/(2 * aV[i]);
    muJ[i] = (sqrt(bJ[i]^2 - 4 * aJ[i] * cJ[i]) - bJ[i])/(2 * aJ[i]);
    mu[i] = fmin(muV[i], muJ[i]);

  }

  // Likelihood
  A_empty ~ normal(b0 + b1 * Pcr_empty, sigma_empty);
  A_corrected ~ normal(mu , sigma_data);
  
}
