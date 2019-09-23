data{
    int<lower=1> N;
    int<lower=1> N_id;
    real Aj[N];
    real Ci[N];
    int id[N];
}
parameters{
    real m_log_gm;
    real m_J;
    real m_log_Rd;
    vector[N_id] s_log_gm;
    vector[N_id] s_J;
    vector[N_id] s_log_Rd;
    real Km;
    real gammaStar;
    real<lower=0> sigmaGm;
    real<lower=0> sigmaRd;
    real<lower=0> sigmaJ;
    real<lower=0> sigmaResid;
}
model{
    vector[N] log_Rd;
    vector[N] J;
    vector[N] log_gm;
    vector[N] c;
    vector[N] b;
    vector[N] a;
    vector[N] mu;
    sigmaResid ~ cauchy( 0 , 1 );
    sigmaJPrior;
    sigmaRdPrior;
    sigmaGmPrior;
    gammaStarPrior;
    s_log_RdPrior;
    s_JPrior;
    s_log_gmPrior;
    m_log_RdPrior;
    m_JPrior;
    m_log_gmPrior;
    for ( i in 1:N ) {
        log_Rd[i] = m_log_Rd + s_log_Rd[id[i]];
    }
    for ( i in 1:N ) {
        J[i] = m_J + s_J[id[i]];
    }
    for ( i in 1:N ) {
        log_gm[i] = m_log_gm + s_log_gm[id[i]];
    }
    for ( i in 1:N ) {
        c[i] = exp(log_Rd[i]) * (Ci[i] + 2 * gammaStar) - (J[i] / 4) * (Ci[i] - gammaStar);
    }
    for ( i in 1:N ) {
        b[i] = (J[i] / 4 - exp(log_Rd[i])) / exp(log_gm[i]) + Ci[i] + 2 * gammaStar;
    }
    for ( i in 1:N ) {
        a[i] = -1 / exp(log_gm[i]);
    }
    for ( i in 1:N ) {
        mu[i] = (sqrt(b[i]^2 - 4 * a[i] * c[i]) - b[i])/(2 * a[i]);
    }
    Aj ~ normal( mu , sigmaResid );
}
generated quantities{
    vector[N] log_Rd;
    vector[N] J;
    vector[N] log_gm;
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
        J[i] = m_J + s_J[id[i]];
    }
    for ( i in 1:N ) {
        log_gm[i] = m_log_gm + s_log_gm[id[i]];
    }
    for ( i in 1:N ) {
        c[i] = exp(log_Rd[i]) * (Ci[i] + 2 * gammaStar) - J[i] / 4 * (Ci[i] - gammaStar);
    }
    for ( i in 1:N ) {
        b[i] = (J[i] / 4 - exp(log_Rd[i])) / exp(log_gm[i]) + Ci[i] + 2 * gammaStar;
    }
    for ( i in 1:N ) {
        a[i] = -1 / exp(log_gm[i]);
    }
    for ( i in 1:N ) {
        mu[i] = (sqrt(b[i]^2 - 4 * a[i] * c[i]) - b[i])/(2 * a[i]);
    }
    dev = dev + (-2)*normal_lpdf( Aj | mu , sigmaResid );
}