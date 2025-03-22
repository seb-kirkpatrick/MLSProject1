
data {
  int<lower=0> N;
  int<lower=0> P;
  int<lower=0> T;
  int<lower=0> O;
  int<lower=0> S;
  int<lower=1, upper=P> player_id[N];
  int<lower=1, upper=T> team_id[N];
  int<lower=1, upper=O> opponent[N];
  int<lower=1, upper=S> season_id[N];
  real min_per_app[N];
  real last_g_min[N];
  real player_xG[N];
  real player_xA[N];
  int<lower=1, upper=4> pos[N];
  real xG[N];
  real xA[N];
  real last_g_xG[N];
  real last_g_xA[N];
}


parameters {
  real mu_xG;
  real mu_xA;
  real<lower=0> sigma_xG;
  real<lower=0> sigma_xA;
  real<lower=0> sigma_pos;
  real<lower=0> sigma_season;
  real lambda_xG[P];
  real lambda_xA[P];
  real season_team_effect[T, S];
  real season_opponent_effect[O, S];
  real pos_effect[4];
}


model {
  mu_xG ~ normal(0,1);
  mu_xA ~ normal(0,1);
  sigma_xG ~ normal(0, 1);
  sigma_xA ~ normal(0, 1);
  sigma_season ~ normal(0, 1);
  sigma_pos ~ normal(0, 1);
  lambda_xG ~ normal(mu_xG, sigma_xG);
  lambda_xA ~ normal(mu_xA, sigma_xA);
  
  for (t in 1:T) {
    for (s in 1:S) {
      season_team_effect[t, s] ~ normal(0, sigma_season);
    }
  }

  for (o in 1:O) {
    for (s in 1:S) {
      season_opponent_effect[o, s] ~ normal(0, sigma_season);
    }
  }
  
  pos_effect ~ normal(0, sigma_pos);
  
  for (n in 1:N) {
    xG[n] ~ normal(lambda_xG[player_id[n]] + 
                   season_team_effect[team_id[n], season_id[n]] + 
                   season_opponent_effect[opponent[n], season_id[n]] + 
                   pos_effect[pos[n]] +
                   player_xG[n] +
                   last_g_min[n] +
                   last_g_xG[n] +
                   min_per_app[n],
                   sigma_xG);
                   
    xA[n] ~ normal(lambda_xA[player_id[n]] +
                   season_team_effect[team_id[n], season_id[n]] +
                   season_opponent_effect[opponent[n], season_id[n]] + 
                   pos_effect[pos[n]] +
                   player_xA[n] +
                   last_g_min[n] +
                   last_g_xA[n] +
                   min_per_app[n],
                   sigma_xA);
  }
                   
                   
}

