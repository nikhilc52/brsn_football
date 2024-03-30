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

cornellO |> 
  ggplot(aes(x=adj_TimeSecsRem, y=EPA, fill=color, color=color)) +
  geom_point(shape=20, size = 3) +  
  scale_x_continuous(breaks=scales::pretty_breaks(n=8))+
  scale_y_continuous(breaks=scales::pretty_breaks(n=8))+
  scale_color_identity(name = "",
                       breaks = c("black", "green", "yellow"),
                       labels = c("Non-Scoring", "Touchdown", "Field Goal"), guide="legend")+
  theme_fivethirtyeight()+  
  theme(axis.title = element_text(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank()) + 
  ylab('EPA') +
  xlab("")+
  labs(title="EPA/Play Over the Course of a Game", 
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
  annotate("text", x=3150, y=-7.5, label="First Quarter", color ="black", size = 3)+
  annotate("text", x=2250, y=-7.5, label="Second Quarter", color ="black", size = 3)+
  annotate("text", x=1350, y=-7.5, label="Third Quarter", color ="black", size = 3)+
  annotate("text", x=450, y=-7.5, label="Fourth Quarter", color ="black", size = 3)+
  scale_x_reverse()+
  geom_text_repel(color = "black",aes(label = ifelse((EPA >= 5.1), 
  "Best Play Of Season","")), nudge_x = -1, nudge_y = 1, min.segment.length = unit(0, 'lines'))+
  guides(fill = "none")+
  theme(panel.background = element_rect(fill="white", color="white"))+
  theme(plot.background = element_rect(fill="white", color="white"))+
  theme(legend.background = element_rect(fill="white", color="white"))+
  theme(legend.box.background = element_rect(fill="white", color="white"))


  

