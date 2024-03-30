library(cfbfastR)
library(tidyverse)
library(ggimage)
library(ggthemes)
library(gt)
library(ggrepel)
library(gtExtras)

pbp <- load_cfb_pbp(seasons = 2023:2023)

cornell <- pbp |> 
  filter(pos_team == "Cornell" | def_pos_team == "Cornell")

ivy <- pbp |> 
  filter(offense_conference == "Ivy")

cornellO <- cornell |> 
  filter(pos_team == "Cornell") |> 
  filter(play_type != "Penalty") |> 
  mutate(color = case_when(touchdown == 1 ~ "green", fg_made == 1 ~ "yellow", .default = "black"))

summaryCornellO <-  cornellO |> 
  filter(play_type != "Kickoff") |> 
  filter(play_type != "Kickoff Return (Offense)") |> 
  filter(fg_inds != 1) |> 
  filter(play_type != "Punt") |> 
  summarise(
    week = week, 
    pos_team = pos_team,
    def_pos_team,
    pos_team_score = pos_team_score,
    def_pos_team_score = def_pos_team_score,
    half = half,
    period = period,
    minutes = clock.minutes,
    seconds = clock.seconds,
    play_type = play_type,
    down = down,
    distance = distance,
    yards_to_goal = yards_to_goal,
    yards_gained = yards_gained,
    epa = EPA,
    success = success,
    yard_line = yard_line,
    touchdown = touchdown,
    pass = pass,
    rush = rush,
    wpa = wpa,
    
    adj_TimeSecsRem = adj_TimeSecsRem,
    drive_number = drive_number,
    lag_new_drive_pts = lag_new_drive_pts,
    
    passer_player_name = passer_player_name,
    receiver_player_name = receiver_player_name,
    rusher_player_name = rusher_player_name)


third_down <- summaryCornellO |> 
  filter(down == 3) |> 
  #group_by(period) |> 
  mutate(firstdown = ifelse(yards_gained >= distance, 1, 0)) |> 
  summarize(
    firstdowns = sum(firstdown),
    rate = firstdowns/n(),
    n()
  )

third_downIVY <- ivy |> 
  filter(down == 3) |> 
  filter(period == 4) |> 
  group_by(pos_team) |> 
  mutate(firstdown = ifelse(yards_gained >= distance, 1, 0)) |> 
  summarize(
    firstdowns = sum(firstdown),
    rate = firstdowns/n(),
    n()
  )
  
drives <- summaryCornellO |>
  group_by(drive_number, week) |> 
  filter(period == 4) |> 
  summarize(
    driveStart = 100-max(yards_to_goal), 
    driveResult = first(lag_new_drive_pts)
  )

targets <- summaryCornellO |>
  filter(down >= 3) |> 
  mutate(firstdown = ifelse(yards_gained >= distance, 1, 0)) |> 
  group_by(receiver_player_name) |> 
  summarize(
    firstdown = sum(firstdown),
    n(),
    rate = firstdown/n()
  )
  

  
  
  