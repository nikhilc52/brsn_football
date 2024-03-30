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
    
    passer_player_name = passer_player_name,
    receiver_player_name = receiver_player_name,
    rusher_player_name = rusher_player_name)

p <- summaryCornellO |> 
  filter(touchdown == 1) |> 
  ggplot(aes(x=adj_TimeSecsRem, y=yards_to_goal)) +
  geom_point(aes(fill="black", color="black"),
             shape=20, show.legend = FALSE, size = 5) +
  scale_color_identity(aesthetics = c("fill","color"))+
  scale_x_continuous(breaks=scales::pretty_breaks(n=8))+
  scale_y_continuous(breaks=scales::pretty_breaks(n=8))+
  theme(axis.title = element_text(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) + 
  ylab('') +
  xlab("")+
  labs(title="", 
       subtitle = "", 
       caption="")+
  theme(plot.title = element_text(size = 22, hjust =0.5, face = "bold"), 
        plot.subtitle = element_text(size = 16, hjust =0.5))+
  geom_hline(yintercept = 0, linetype="dashed")+
  geom_hline(yintercept = 50, linetype="dashed")
  scale_x_reverse()
  
  p +
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent') #transparent plot bg
    )

  
  ggsave(
    plot = p,
    filename = "tr_tst2.png",
    bg = "transparent"
  )
  