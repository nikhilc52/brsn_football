library(cfbfastR)
library(tidyverse)
library(ggimage)
library(ggthemes)
library(gt)
library(ggrepel)
library(gtExtras)

pbp <- load_cfb_pbp(seasons = 2023:2023)

ivy <- pbp |> 
  filter(offense_conference == "Ivy" | defense_conference == "Ivy")

ivy <- ivy |> 
  mutate(team_alpha = ifelse(pos_team == "Cornell" | def_pos_team == "Cornell", 
                             0.9, 0.2))

cornell <- pbp |> 
  filter(pos_team == "Cornell" | def_pos_team == "Cornell")

cornellO <- cornell |> 
  filter(pos_team == "Cornell") |> 
  filter(play_type != "Penalty") |> 
  mutate(color = case_when(touchdown == 1 ~ "green", fg_made == 1 ~ "yellow", .default = "black"))

summaryCornellO <-  cornellO |> 
  filter(play_type != "Kickoff") |> 
  filter(play_type != "Kickoff Return (Offense)") |> 
  filter(fg_inds != 1) |> 
  filter(play_type != "Punt") |> 
  filter(!is.na(EPA)) |> 
  filter(EPA >= 0.001) |> 
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
    
    score_diff_start = score_diff_start,
    score_diff = score_diff,
    adj_TimeSecsRem = 3600-adj_TimeSecsRem,
    td_multiplier = ifelse(touchdown == 1, 2, 1),
    go_ahead_multiplier = ifelse(score_diff_start <= 0 & score_diff > 0, 2, 1),
    fourth_down_multiplier = ifelse(down == 4, 2, 1),
    play_text = play_text,
    
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
           (1+(adj_TimeSecsRem * .001)^3)* td_multiplier * go_ahead_multiplier * fourth_down_multiplier) |> 
  mutate(color = ifelse(def_pos_team == "Yale", "blue", "black"))

summaryCornellO |> 
  filter(yards_gained >= 25) |> 
  filter(pass == 1) |> 
  group_by(receiver_player_name) |> 
  summarize(
    count = n()
  ) |> 
  arrange(-count) |> 
  gt() |> 
  opt_align_table_header("center") |> 
  cols_align("center") |> 
  tab_source_note("Nikhil Chinchalkar for BRSN | Data: cfbfastR") |> 
  cols_label(
    receiver_player_name = "Player",
    count = "Catches"
  ) |> 
  opt_row_striping() |> 
  tab_header(title = "Catches Over 25 Yards") |> 
  gt_theme_nytimes() |> 
  tab_style(
    style = list(
      cell_fill(color = "lightyellow"),
      cell_text(weight = "bold", color = "black")
    ),
    locations = cells_body(
      columns = c(receiver_player_name, count),
      rows = receiver_player_name == "Davon Kiser"
    )
  ) |> 
  opt_align_table_header(align = "center")

summaryCornellO |> 
  filter(epa > 0) |> 
  ggplot(aes(x=3600-adj_TimeSecsRem, y=clutch)) +
  geom_point(aes(fill="black", color="black"),
             shape=20, show.legend = FALSE, size = 3) +
  scale_color_identity(aesthetics = c("fill","color"))+
  scale_x_continuous(breaks=scales::pretty_breaks(n=8))+
  scale_y_continuous(breaks=scales::pretty_breaks(n=8))+
  theme_fivethirtyeight()+  
  theme(axis.title = element_text(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank()) + 
  ylab('Clutch Index') +
  xlab("")+
  labs(title="Clutch Index Over the Course of a Game", 
       subtitle = "2023", 
       caption="Nikhil Chinchalkar for BRSN\nData: cfbfastR")+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(size = 16, hjust =0.5))+
  geom_hline(yintercept = 0, linetype="dashed")+
  geom_vline(xintercept = 3600, linetype="dashed")+
  geom_vline(xintercept = 2700, linetype="dashed")+
  geom_vline(xintercept = 1800, linetype="dashed")+
  geom_vline(xintercept = 900, linetype="dashed")+
  geom_vline(xintercept = 0, linetype="dashed")+
  annotate("text", x=3150, y=-5, label="First Quarter", color ="black", size = 3)+
  annotate("text", x=2250, y=-5, label="Second Quarter", color ="black", size = 3)+
  annotate("text", x=1350, y=-5, label="Third Quarter", color ="black", size = 3)+
  annotate("text", x=450, y=-5, label="Fourth Quarter", color ="black", size = 3)+
  scale_x_reverse()+
  geom_text_repel(aes(label = ifelse((clutch >= 50), 
    "Clutchest Play Of Season","")), nudge_x = -1, nudge_y = 3, min.segment.length = unit(0, 'lines'))+
  theme(panel.background = element_rect(fill="white", color="white"))+
  theme(plot.background = element_rect(fill="white", color="white"))+
  theme(legend.background = element_rect(fill="white", color="white"))+
  theme(legend.box.background = element_rect(fill="white", color="white"))



