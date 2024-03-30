library(tidyverse)
library(gt)
library(gtExtras)
library(nflfastR)

pbp2 <- load_pbp(season=2023)

eagles2 <-  pbp2 |> 
  filter(!is.na(epa)) |> 
  summarise(
    week = week, 
    pos_team = posteam,
    pos_team_score = posteam_score,
    def_pos_team_score = defteam_score,
    time = time,
    half = game_half,
    quarter = qtr,
    play_type = play_type,
    down = down,
    distance = ydstogo,
    yards_gained = yards_gained,
    epa = epa,
    success = success,
    touchdown = touchdown,
    pass = pass,
    rush = rush,
    wpa = wpa,
    
    score_diff_start = score_differential,
    score_diff = score_differential_post,
    adj_TimeSecsRem = 3600-game_seconds_remaining,
    td_multiplier = ifelse(touchdown == 1, 2, 1),
    go_ahead_multiplier = ifelse(score_diff_start <= 0 & score_diff >= 0, 2, 1),
    fourth_down_multiplier = ifelse(down == 4, 2, 1),
    desc = desc,
    
    passer_player_name = passer_player_name,
    receiver_player_name = receiver_player_name,
    rusher_player_name = rusher_player_name
  ) |> 
  mutate(score_diff_multiplier = case_when(
    score_diff_start > 0 ~ 1, 
    score_diff_start == 0 ~ 2, 
    score_diff_start > -9  ~ 4,
    .default = 1
  )) |> 
  mutate(clutch = .1* wpa * yards_gained * score_diff_multiplier * 
           (1+(adj_TimeSecsRem * .001)^3)* td_multiplier * go_ahead_multiplier * fourth_down_multiplier)
