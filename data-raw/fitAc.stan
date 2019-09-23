data{
    int<lower=1> N;
    int<lower=1> N_id;
    real Ac[N];
    real Ci[N];
    int id[N];
}
parameters{
    real m_log_rm;
    real m_Vcmax;
    real m_log_Rd;
    vector[N_id] s_log_rm;
    vector[N_id] s_Vcmax;
    vector[N_id] s_log_Rd;
    real Km;
    real gammaStar;
    real<lower=0> sigmaRm;
    real<lower=0> sigmaRd;
    real<lower=0> sigmaVcmax;
    real<lower=0> sigmaResid;
}
model{
    vector[N] log_Rd;
    vector[N] Vcmax;
    vector[N] log_rm;
    vector[N] c;
    vector[N] b;
    vector[N] a;
    vector[N] mu;
    sigmaResid ~ cauchy( 0 , 1 );
    sigmaVcmax ~ cauchy(0, 1);
    sigmaRd ~ cauchy(0, 1);
    sigmaRm ~ cauchy(0, 1);
    gammaStar ~ normal(3.743, 0.1);
    Km ~ normal(62.067, 1);
    s_log_Rd ~ normal(0, sigmaRd);
    s_Vcmax ~ normal(0, sigmaVcmax);
    s_log_rm ~ normal(0, sigmaRm);
    m_log_Rd ~ normal(1.39, 1.39);
    m_Vcmax ~ normal(30, 20);
    m_log_rm ~ normal(1.1, 1.62);
    for ( i in 1:N ) {
        log_Rd[i] = m_log_Rd + s_log_Rd[id[i]];
    }
    for ( i in 1:N ) {
        Vcmax[i] = m_Vcmax + s_Vcmax[id[i]];
    }
    for ( i in 1:N ) {
        log_rm[i] = m_log_rm + s_log_rm[id[i]];
    }
    for ( i in 1:N ) {
        c[i] = exp(log_Rd[i]) * (Ci[i] + Km) - Vcmax[i] * (Ci[i] - gammaStar);
    }
    for ( i in 1:N ) {
        b[i] = exp(log_rm[i]) * (Vcmax[i] - exp(log_Rd[i])) + Ci[i] + Km;
    }
    for ( i in 1:N ) {
        a[i] = -exp(log_rm[i]);
    }
    for ( i in 1:N ) {
        mu[i] = (sqrt(b[i]^2 - 4 * a[i] * c[i]) - b[i])/(2 * a[i]);
    }
    Ac ~ normal( mu , sigmaResid );
}
generated quantities{
    vector[N] log_Rd;
    vector[N] Vcmax;
    vector[N] log_rm;
    vector[N] c;
    vector[N] b;
    vector[N] a;
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
        log_rm[i] = m_log_rm + s_log_rm[id[i]];
    }
    for ( i in 1:N ) {
        c[i] = exp(log_Rd[i]) * (Ci[i] + Km) - Vcmax[i] * (Ci[i] - gammaStar);
    }
    for ( i in 1:N ) {
        b[i] = exp(log_rm[i]) * (Vcmax[i] - exp(log_Rd[i])) + Ci[i] + Km;
    }
    for ( i in 1:N ) {
        a[i] = -exp(log_rm[i]);
    }
    for ( i in 1:N ) {
        mu[i] = (sqrt(b[i]^2 - 4 * a[i] * c[i]) - b[i])/(2 * a[i]);
    }
    dev = dev + (-2)*normal_lpdf( Ac | mu , sigmaResid );
}