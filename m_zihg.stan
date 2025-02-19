functions {
  real custom_lpmf(int pos, real p, int S, int N) {
    // n positives (data)
    // in S samples (data) (trials)
    // from a collection of N units (data)
    // with a prevalence of p (parameter)
    //real LL;
    int Nmax;
    array[N-S+1] real LL;
    Nmax = N-S + pos; // cannot be more infecteds than N-minus negative samples
    
    for (i in pos:Nmax){ // cycle over the number of possible infected
      LL[i-pos+1] =
        // prob of observing pos positives out of a sample of S from this population with 
        hypergeometric_lpmf(pos | S, i, N-i) + // i infecteds and N-i uninfecteds
        binomial_lpmf(i | N, p) ; // prob of getting i infecteds given a prevalence of p
       
    }
    return log_sum_exp(LL);
  }
  
}
data {
  int<lower=0> Nsamples;
  array[Nsamples] int<lower=0> pos;
  array[Nsamples] int<lower=1> S;
  array[Nsamples] int<lower=1> N; 
  real theta_a;
  real theta_b;
  real prev_a;
  real prev_b;
}
parameters {
  real<lower=0, upper=1> theta;
  real<lower=0, upper=1> prev;
}
model {
  theta ~ beta(theta_a, theta_b);
  prev ~ beta(prev_a, prev_b);
  
  for (n in 1:Nsamples) {
    if (pos[n] == 0) {
      target += log_sum_exp(
        log1m(theta),
        log(theta) + custom_lpmf(pos[n] | prev, S[n], N[n])
        );
    } else {
      target += log(theta) + custom_lpmf(pos[n] | prev, S[n], N[n]);
    }
  }
}
