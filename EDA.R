library(tidyverse)
library(factoextra)
library(rstan)

sched <- read_csv("atlutd_datascientist_project1_schedule.csv")
matches <- read_csv("atlutd_datascientist_project1_matchlog.csv")

table(sched$season_id)
table(matches$season_id)

# Working towards per 90, start with season totals
szn_stats <- matches |>
  mutate(
    player_match_pressures = ifelse(is.na(player_match_pressures), 0, player_match_pressures)
  ) |>
  group_by(player_name, player_id, season_id) |>
  summarize(
    minutes = sum(player_match_minutes),
    matches = n_distinct(match_id),
    xG = sum(player_match_np_xg),
    xA = sum(player_match_xa),
    shots = sum(player_match_np_shots),
    tib = sum(player_match_touches_inside_box),
    dribbles = sum(player_match_dribbles),
    passes = sum(player_match_passes),
    pressures = sum(player_match_pressures),
    da = sum(player_match_defensive_actions),
  ) |>
  ungroup()

# Now per 90
per_90 <- szn_stats |>
  reframe(
    player_name,
    player_id,
    season_id,
    minutes,
    m_per_m = minutes / matches,
    xG_per_90 = (xG * 90) / minutes,
    xA_per_90 = (xA * 90) / minutes,
    s_per_90 = (shots * 90) / minutes,
    dt_per_90 = (tib * 90) / minutes,
    d_per_90 = (dribbles * 90) / minutes,
    pa_per_90 = (passes * 90) / minutes,
    pr_per_90 = (pressures * 90) / minutes,
    da_per_90 = (da * 90) / minutes,
  )

# Since I do not have position, my thought is to cluster off some stats
# This could be tested more, but shots, passes, and defensive actions seem good

per_90_scaled <- scale(per_90[, 6:13])
fviz_nbclust(per_90_scaled[, c(3, 6, 8)], kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Number of Clusters") # 4 or 5 clusters

set.seed(740)
pos_c <- kmeans(per_90_scaled[, c(3, 6, 8)], centers = 4)  
per_90$pos <- as.factor(pos_c$cluster)

# Visualizing the clusters

ggplot(data = per_90,
  aes(x = s_per_90, y = pa_per_90, color = pos)
) + geom_point()

ggplot(data = per_90,
       aes(x = s_per_90, y = da_per_90, color = pos)
) + geom_point()

ggplot(data = per_90,
       aes(x = da_per_90, y = pa_per_90, color = pos)
) + geom_point()

# Cluster stats

per_90 |> 
  group_by(pos) |> 
  summarize(
    avg_xG = mean(xG_per_90, na.rm = TRUE),
    avg_xA = mean(xA_per_90, na.rm = TRUE),
    avg_shots = mean(s_per_90, na.rm = TRUE),
    avg_touches_in_box = mean(dt_per_90, na.rm = TRUE),
    avg_dribbles = mean(d_per_90, na.rm = TRUE),
    avg_passes = mean(pa_per_90, na.rm = TRUE),
    avg_pressures = mean(pr_per_90, na.rm = TRUE),
    avg_def_actions = mean(da_per_90, na.rm = TRUE),
    count = n()
  )

positions <- per_90 |>
  select(player_id, season_id, pos)

# Cluster 2 is central defenders, some-6's
# Cluster 4 is GKs,
# Cluster 1 is fullbacks, some 6's, and 8's
# Cluster 3 is attacking players

# Now that we have position, we need to get the data ready for the model
# The goal will be to build each players season within the data so their per 90 will be going into that game
# For the sake of this, I may remove the first week of the season, but that may be mute

#Schedule only goes back to 2021, so 2020 may get the axe from the model (probably for the better)

matchups <- sched |>
  select(match_id, match_date, match_week, home_team_id, home_score, away_team_id, away_score)

model_data <- matches |>
  filter(season_name >= 2021) |>
  left_join(matchups, by = "match_id") |>
  left_join(positions, by = c("player_id", "season_id")) |>
  arrange(player_id, season_id, match_week) |>
  group_by(player_id, season_id) |>
  mutate(
    xG = player_match_np_xg,
    xA = player_match_xa,
    szn_xG = if_else(row_number() > 1, lag(cumsum(player_match_np_xg), default = 0), 0),
    szn_xA = if_else(row_number() > 1, lag(cumsum(player_match_xa), default = 0), 0),
    szn_min = if_else(row_number() > 1, lag(cumsum(player_match_minutes), default = 0), 0),
    home = team_id == home_team_id,
    opponent = if_else(team_id == home_team_id, away_team_id, home_team_id),
    xG_per_90 = if_else(szn_min > 0, szn_xG * 90 / szn_min, 0),
    xA_per_90 = if_else(szn_min > 0, szn_xA * 90 / szn_min, 0),
    last_g_min = lag(player_match_minutes, default = 0),
    min_per_app = if_else(row_number() > 1, szn_min / (row_number() - 1), 0),
    last_game_xG = lag(player_match_np_xg, default = 0),
    last_game_xA = lag(player_match_xa, default = 0),
  ) |>
  ungroup()

stan_data <- model_data |>
  select(
    player_id, 
    team_id, 
    opponent,
    season_id,
    szn_xG, 
    szn_xA, 
    szn_min, 
    xG_per_90, 
    xA_per_90, 
    min_per_app, 
    last_g_min,
    home, 
    pos,
    xG,
    xA,
    last_game_xG,
    last_game_xA,
  )

# Having difficulty with the range of the data objects in stan, so I will reindex these variable to 1 to however many of each element there is
unique_player_ids <- sort(unique(stan_data$player_id))
player_id_map <- setNames(seq_along(unique_player_ids), unique_player_ids)
stan_data$player_id_new <- player_id_map[as.character(stan_data$player_id)]

unique_teams <- sort(unique(stan_data$team_id))
team_mapping <- setNames(seq_along(unique_teams), unique_teams)
stan_data$team_id_new <- team_mapping[as.character(stan_data$team_id)]

unique_opponents <- sort(unique(stan_data$opponent))
opponent_mapping <- setNames(seq_along(unique_opponents), unique_opponents)
stan_data$opponent_new <- opponent_mapping[as.character(stan_data$opponent)]

unique_seasons <- sort(unique(stan_data$season_id))
season_mapping <- setNames(seq_along(unique_seasons), unique_seasons)
stan_data$season_id_new <- season_mapping[as.character(stan_data$season_id)]

stan_data_list <- list(
  N = nrow(stan_data),
  P = length(unique(stan_data$player_id_new)),
  T = length(unique(stan_data$team_id_new)), 
  O = length(unique(stan_data$opponent_new)),
  S = length(unique(stan_data$season_id_new)),
  player_id = as.integer(stan_data$player_id_new),
  team_id = as.integer(stan_data$team_id_new),
  opponent = as.integer(stan_data$opponent_new),
  season_id = as.integer(stan_data$season_id_new),
  min_per_app = stan_data$min_per_app,
  last_g_min = stan_data$last_g_min, 
  player_xG = stan_data$xG_per_90,
  player_xA = stan_data$xA_per_90,
  pos = as.integer(stan_data$pos),
  xG = stan_data$xG,
  xA = stan_data$xA,
  last_g_xG = stan_data$last_game_xG,
  last_g_xA = stan_data$last_game_xA
  )

stan_model <- stan_model("test.stan")

fit <- stan(
  file = "test.stan",
  data = stan_data_list,
  iter = 1000,
  chains = 4,
  warmup = 500,
  refresh = 1
)
