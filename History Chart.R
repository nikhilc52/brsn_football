library(tidyverse)
library(ggimage)
library(ggthemes)
library(gt)
library(ggrepel)
library(gtExtras)

data <- read.csv("C:\\Users\\nikhi\\Downloads\\download.csv")

data |> 
  filter(Season >= 2013 & Season <= 2022) |> 
  select(Season, Week, HomeTeam, HomeScore, AwayTeam, AwayScore, Winner) |> 
  gt() |> 
  opt_align_table_header("center") |> 
  cols_align("center") |> 
  tab_source_note("Nikhil Chinchalkar for BRSN | Data: collegefootballdata.com") |> 
  opt_row_striping() |> 
  tab_header(title = "Cornell vs. Yale Recent History", ) |> 
  gt_theme_538() |> 
  cols_label(
    HomeTeam = "Home Team",
    AwayTeam = "Away Team",
    AwayScore = "Away Score",
    HomeScore = "Home Score",
  ) |> 
  opt_align_table_header(align = "center") |> 
  tab_style(
    style = list(
      cell_fill(color = "darkblue"),
      cell_text(weight = "bold", color = "white")
    ),
    locations = cells_body(
      columns = Winner,
      rows = Winner == "Yale"
    )
  ) |> 
  tab_style(
    style = list(
      cell_fill(color = "red"),
      cell_text(weight = "bold", color = "black")
    ),
    locations = cells_body(
      columns = Winner,
      rows = Winner == "Cornell"
    )
  )

