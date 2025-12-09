data {
  int<lower=1> N;               // número de instituciones
  int<lower=1> T;               // número de tiempos (3 años)
  matrix[N, T] y;               // puntajes observados

  matrix[N, N] A;               // matriz de adyacencia 0/1
  vector[N] d;                  // grados: d_i = sum_j A_ij

  vector[N] rating;             // rating observado o dummy (0 si faltante)
  int<lower=0, upper=1> is_obs[N];   // 1 si rating observado, 0 si faltante
}

parameters {
  // Efectos fijos
  real beta0;
  real beta1;

  // Efecto espacial
  vector[N] u;
  real<lower=0> tau_u;                // precisión espacial
  real<lower=0, upper=1> rho;        // dependencia espacial

  // Efecto temporal
  vector[T] gamma;
  real<lower=0> sigma_gamma;

  // Covariable latente x*_i
  vector[N] x_star;
  real mu_x;
  real<lower=0> sigma_x;

  // Ruido de observación
  real<lower=0> sigma;
}

transformed parameters {
  matrix[N, N] Q;   // matriz de precisión del CAR propio

  // Q = D - rho * A, donde D = diag(d)
  Q = diag_matrix(d) - rho * A;
}

model {
  // --- Priors ---
  beta0 ~ normal(0, 100);
  beta1 ~ normal(0, 10);

  mu_x   ~ normal(0, 10);
  sigma_x ~ cauchy(0, 2.5);

  sigma ~ cauchy(0, 2.5);
  sigma_gamma ~ cauchy(0, 2.5);

  tau_u ~ gamma(1, 0.1);   // puedes ajustar estos hiperparámetros
  rho   ~ beta(2, 2);      // centrado en 0.5, soporta [0,1]

  // --- Efecto temporal: RW1 ---
  gamma[1] ~ normal(0, 10);
  for (t in 2:T) {
    gamma[t] ~ normal(gamma[t-1], sigma_gamma);
  }

  // --- Covariable latente x_star ---
  for (i in 1:N) {
    if (is_obs[i] == 1) {
      // rating observado: x*_i cercano al rating real
      x_star[i] ~ normal(rating[i], 0.1);   // var pequeña -> casi fijo
    } else {
      // rating faltante: imputado desde N(mu_x, sigma_x^2)
      x_star[i] ~ normal(mu_x, sigma_x);
    }
  }

  // --- Efecto espacial CAR propio ---
  // u ~ N(0, (tau_u * Q)^(-1))
  u ~ multi_normal_prec(rep_vector(0, N), tau_u * Q);

  // --- Likelihood ---
  for (i in 1:N) {
    for (t in 1:T) {
      y[i, t] ~ normal(
        beta0 + beta1 * x_star[i] + u[i] + gamma[t],
        sigma
      );
    }
  }
}
