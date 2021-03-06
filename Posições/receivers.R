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

#WR

recebedor <- pbp %>%
  filter(pass == 1, season == 2020, down <= 4, week <= 17, !is.na(epa)) %>%
  group_by(receiver_player_name, posteam) %>%
  summarize(total_epa = sum(epa), 
            success_rate = mean(success), 
            first_down = mean(first_down), 
            adot = mean(air_yards), plays = n()) %>%
  ungroup() %>%
  arrange(-total_epa) %>%
  filter(plays >= 100 & plays < 3174) %>%
  drop_na() %>%
  gt() %>%
  tab_header(title = md("**Desempenho dos recebedores em 2020.**"),
             subtitle = "Mínimo 100 tentativas. Semanas 1-17.") %>%
  tab_source_note("Table: @juanseit_ | Data by @nflfastR.") %>%
  tab_footnote(footnote = "ADOT = profundidade média dos passes tentados, em jardas.", 
               locations = cells_column_labels(columns = vars(adot))) %>%
  data_color(columns = vars(total_epa), 
             colors = scales::col_numeric(palette = c("blue","white","yellow","red"), domain = c(max(total_epa), min(total_epa)))) %>%
  cols_label(receiver_player_name = "Recebedor",
             posteam = "Time",
             total_epa = "EPA Total",
             success_rate = "SR%",
             first_down = "First down%",
             adot = "ADOT", plays = "Tentativas") %>%
  fmt_number(
      columns = 3:5,
      decimals = 4,
      suffixing = TRUE
  ) %>%
  fmt_number(
    columns = 6,
    decimals = 1,
    suffixing = TRUE
  ) %>%
  cols_align(align = c("center"), columns = TRUE) %>%
gt_theme_538(table.width = px(550))
  
recebedor #tabela
