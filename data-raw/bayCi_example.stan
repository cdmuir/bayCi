data{
    int<lower=1> N;
    real A[N];
    real Ci[N];
}
parameters{
    real log_gm;
    real Vcmax;
    real J;
    real log_Rd;
    real Km;
    real gammaStar;
    real<lower=0> sigmaResid;
}
model{
    vector[N] cJ;
    vector[N] bJ;
    vector[N] aJ;
    vector[N] cV;
    vector[N] bV;
    vector[N] aV;
    vector[N] mu;
    vector[N] muV;
    vector[N] muJ;
    sigmaResid ~ cauchy(0, 1);
    gammaStar ~ normal(3.743, 0.1);
    Km ~ normal(62.067, 1);
    log_gm ~ normal(0, 4);
    Vcmax ~ normal(30, 20);
    J ~ normal(50, 20);
    log_Rd ~ normal(1.39, 1.39);
    for ( i in 1:N ) {
        cJ[i] = exp(log_Rd) * (Ci[i] + 2 * gammaStar) - J/4 * (Ci[i] - gammaStar);
    }
    for ( i in 1:N ) {
        bJ[i] = (J/4 - exp(log_Rd))/exp(log_gm) + Ci[i] + 2 * gammaStar;
    }
    for ( i in 1:N ) {
        aJ[i] = -1/exp(log_gm);
    }
    for ( i in 1:N ) {
        cV[i] = exp(log_Rd) * (Ci[i] + Km) - Vcmax * (Ci[i] - gammaStar);
    }
    for ( i in 1:N ) {
        bV[i] = (Vcmax - exp(log_Rd))/exp(log_gm) + Ci[i] + Km;
    }
    for ( i in 1:N ) {
        aV[i] = -1/exp(log_gm);
    }
    for ( i in 1:N ) {
        muV[i] = (sqrt(bV[i]^2 - 4 * aV[i] * cV[i]) - bV[i])/(2 * aV[i]);
        muJ[i] = (sqrt(bJ[i]^2 - 4 * aJ[i] * cJ[i]) - bJ[i])/(2 * aJ[i]);
        mu[i] = fmin(muV[i], muJ[i]);
    }
    A ~ normal( mu , sigmaResid );
}
generated quantities{
    vector[N] cJ;
    vector[N] bJ;
    vector[N] aJ;
    vector[N] cV;
    vector[N] bV;
    vector[N] aV;
    vector[N] mu;
    vector[N] muV;
    vector[N] muJ;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        cJ[i] = exp(log_Rd) * (Ci[i] + 2 * gammaStar) - J/4 * (Ci[i] - gammaStar);
    }
    for ( i in 1:N ) {
        bJ[i] = (J/4 - exp(log_Rd))/exp(log_gm) + Ci[i] + 2 * gammaStar;
    }
    for ( i in 1:N ) {
        aJ[i] = -1/exp(log_gm);
    }
    for ( i in 1:N ) {
        cV[i] = exp(log_Rd) * (Ci[i] + Km) - Vcmax * (Ci[i] - gammaStar);
    }
    for ( i in 1:N ) {
        bV[i] = (Vcmax - exp(log_Rd))/exp(log_gm) + Ci[i] + Km;
    }
    for ( i in 1:N ) {
        aV[i] = -1/exp(log_gm);
    }
    for ( i in 1:N ) {
        muV[i] = (sqrt(bV[i]^2 - 4 * aV[i] * cV[i]) - bV[i])/(2 * aV[i]);
        muJ[i] = (sqrt(bJ[i]^2 - 4 * aJ[i] * cJ[i]) - bJ[i])/(2 * aJ[i]);
        mu[i] = fmin(muV[i], muJ[i]);
    }
    dev = dev + (-2)*normal_lpdf( A | mu , sigmaResid );
}