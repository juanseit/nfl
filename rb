library(tidyverse)
library(nflfastR)
library(gt)
options(scipen = 9999)

seasons <- 2010:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})
update_db(dbname = "pbp")

#RB

rb <- pbp %>%
  filter(rush == 1, season == 2020, down <= 4, week <= 17, !is.na(epa)) %>%
  group_by(name, posteam) %>%
  summarize(epa_per_rush = mean(epa), 
            yards_gained = mean(yards_gained), 
            success_rate = mean(success), 
            first_down = mean(first_down), plays = n()) %>%
  ungroup() %>%
  arrange(-epa_per_rush) %>%
  filter(plays > 120) %>%
  drop_na() %>%
  gt() %>%
  tab_header(title = md("**Desempenho dos running backs em 2020.**"),
             subtitle = "Mínimo 120 corridas. Semanas 1-17.") %>%
  tab_source_note("Table: @juanseit_ | Data by @nflfastR.") %>%
  data_color(columns = vars(epa_per_rush), 
             colors = scales::col_numeric(palette = c("blue","white","yellow","red"), domain = c(max(epa_per_rush), min(epa_per_rush)))) %>%
  cols_label(name = "Running back", 
             posteam = "Time", 
             epa_per_rush = "EPA/play", 
             yards_gained = "Jardas Ganhas", 
             success_rate = "SR%", 
             first_down = "First down%", 
             plays = "Jogadas") %>%
  fmt_number(
    columns = 3:6,
    decimals = 4,
    suffixing = TRUE
  ) %>%
  cols_align(align = c("center"), columns = TRUE)

rb #tabela
