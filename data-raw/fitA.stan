data{
    int<lower=1> N;
    int<lower=1> N_id;
    real A[N];
    real Ci[N];
    int id[N];
}
parameters{
    real m_log_gm;
    real m_Vcmax;
    real m_J;
    real m_log_Rd;
    vector[N_id] s_log_gm;
    vector[N_id] s_Vcmax;
    vector[N_id] s_J;
    vector[N_id] s_log_Rd;
    real Km;
    real gammaStar;
    real<lower=0> sigmaGm;
    real<lower=0> sigmaRd;
    real<lower=0> sigmaVcmax;
    real<lower=0> sigmaJ;
    real<lower=0> sigmaResid;
}
model{
    vector[N] log_Rd;
    vector[N] Vcmax;
    vector[N] J;
    vector[N] log_gm;
    vector[N] cV;
    vector[N] bV;
    vector[N] aV;
    vector[N] cJ;
    vector[N] bJ;
    vector[N] aJ;
    vector[N] muV;
    vector[N] muJ;
    vector[N] mu;
    sigmaResid ~ cauchy( 0 , 1 );
    sigmaVcmax ~ cauchy(0, 1);
    sigmaJ ~ cauchy(0, 1);
    sigmaRd ~ cauchy(0, 1);
    sigmaGm ~ cauchy(0, 1);
    gammaStar ~ normal(3.743, 0.1);
    Km ~ normal(62.067, 1);
    s_log_Rd ~ normal(0, sigmaRd);
    s_Vcmax ~ normal(0, sigmaVcmax);
    s_J ~ normal(0, sigmaJ);
    s_log_gm ~ normal(0, sigmaGm);
    m_log_Rd ~ normal(1.39, 1.39);
    m_Vcmax ~ normal(30, 20);
    m_J ~ normal(50, 20);
    m_log_gm ~ normal(0, 5);
    for ( i in 1:N ) {
        log_Rd[i] = m_log_Rd + s_log_Rd[id[i]];
    }
    for ( i in 1:N ) {
        Vcmax[i] = m_Vcmax + s_Vcmax[id[i]];
    }
    for ( i in 1:N ) {
        J[i] = m_J + s_J[id[i]];
    }
    for ( i in 1:N ) {
        log_gm[i] = m_log_gm + s_log_gm[id[i]];
    }
    for ( i in 1:N ) {
        cV[i] = exp(log_Rd[i]) * (Ci[i] + Km) - Vcmax[i] * (Ci[i] - gammaStar);
    }
    for ( i in 1:N ) {
        bV[i] = (Vcmax[i] - exp(log_Rd[i])) / exp(log_gm[i]) + Ci[i] + Km;
    }
    for ( i in 1:N ) {
        aV[i] = -1 / exp(log_gm[i]);
    }
    for ( i in 1:N ) {
        cJ[i] = exp(log_Rd[i]) * (Ci[i] + 2 * gammaStar) - J[i] / 4 * (Ci[i] - gammaStar);
    }
    for ( i in 1:N ) {
        bJ[i] =  (J[i] / 4 - exp(log_Rd[i])) / exp(log_gm[i]) + Ci[i] + 2 * gammaStar;
    }
    for ( i in 1:N ) {
        aJ[i] = - 1 / exp(log_gm[i]);
    }

    for ( i in 1:N ) {
        muV[i] = (sqrt(bV[i]^2 - 4 * aV[i] * cV[i]) - bV[i])/(2 * aV[i]);
        muJ[i] = (sqrt(bJ[i]^2 - 4 * aJ[i] * cJ[i]) - bJ[i])/(2 * aJ[i]);
        mu[i] = fmin(muV[i], muJ[i]);
    }
    A ~ normal( mu , sigmaResid );
}
generated quantities{
    vector[N] log_Rd;
    vector[N] Vcmax;
    vector[N] J;
    vector[N] log_gm;
    vector[N] cV;
    vector[N] bV;
    vector[N] aV;
    vector[N] cJ;
    vector[N] bJ;
    vector[N] aJ;
    vector[N] muV;
    vector[N] muJ;
    vector[N] mu;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        log_Rd[i] = m_log_Rd + s_log_Rd[id[i]];
    }
    for ( i in 1:N ) {
        Vcmax[i] = m_Vcmax + s_Vcmax[id[i]];
    }
    for ( i in 1:N ) {
        J[i] = m_J + s_J[id[i]];
    }
    for ( i in 1:N ) {
        log_gm[i] = m_log_gm + s_log_gm[id[i]];
    }
    for ( i in 1:N ) {
        cV[i] = exp(log_Rd[i]) * (Ci[i] + Km) - Vcmax[i] * (Ci[i] - gammaStar);
    }
    for ( i in 1:N ) {
        bV[i] = (Vcmax[i] - exp(log_Rd[i])) / exp(log_gm[i]) + Ci[i] + Km;
    }
    for ( i in 1:N ) {
        aV[i] = -1 / exp(log_gm[i]);
    }
    for ( i in 1:N ) {
        cJ[i] = exp(log_Rd[i]) * (Ci[i] + 2 * gammaStar) - J[i] / 4 * (Ci[i] - gammaStar);
    }
    for ( i in 1:N ) {
        bJ[i] = (J[i] / 4 - exp(log_Rd[i])) / exp(log_gm[i]) + Ci[i] + 2 * gammaStar;
    }
    for ( i in 1:N ) {
        aJ[i] = -1 / exp(log_gm[i]);
    }
    for ( i in 1:N ) {
        muV[i] = (sqrt(bV[i]^2 - 4 * aV[i] * cV[i]) - bV[i])/(2 * aV[i]);
        muJ[i] = (sqrt(bJ[i]^2 - 4 * aJ[i] * cJ[i]) - bJ[i])/(2 * aJ[i]);
        mu[i] = fmin(muV[i], muJ[i]);
    }
    dev = dev + (-2)*normal_lpdf( A | mu , sigmaResid );
}