library(tidyverse)
library(ggrepel)
library(ggimage)
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

#Ravens report
#EPA médio de ataque e defesa
epa_o <- pbp %>%
  filter(season == 2020, week <= 17, down <= 4, !is.na(epa)) %>%
  group_by(posteam) %>%
  summarize(epa = mean(epa)) %>%
  ungroup()

epa_d <- pbp %>%
  filter(season == 2020, week <= 17, down <= 4, !is.na(epa)) %>%
  group_by(defteam) %>%
  summarize(epa = mean(epa)) %>%
  ungroup()

evolution_o <- pbp %>% #ataque
  filter(posteam == "BAL", season == 2020, down <= 4, week <= 17, !is.na(epa)) %>%
  group_by(week) %>%
  summarize(epa_per_play = mean(epa), week, team = last(defteam)) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr')) %>%
  ggplot(mapping = aes(x = week, y = epa_per_play)) +
  geom_line(color = "purple", size = 2) +
  geom_image(aes(image = team_logo_espn)) +
  geom_hline(yintercept = mean(epa_o$epa), color = "white",linetype = "dashed") +
  labs(title = "Evolução do ataque dos Ravens em EPA/Play.",
       subtitle = "Temporada 2020, por semana.",
       y = "EPA por jogada.",
       x = "Semanas.",
       caption = "Figure: @juanseit_ | Data by @nflfastR.") +
  theme_theathletic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 17))

evolution_o #resultado

evolution_d <- pbp %>% #defesa
  filter(defteam == "BAL", season == 2020, down <= 4, week <= 17, !is.na(epa)) %>%
  group_by(week) %>%
  summarize(epa_per_play = mean(epa), week, team = last(posteam)) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr')) %>%
  ggplot(mapping = aes(x = week, y = epa_per_play)) +
  geom_line(color = "purple", size = 2) +
  geom_image(aes(image = team_logo_espn)) +
  geom_hline(yintercept = mean(epa_d$epa), color = "white",linetype = "dashed") +
  labs(title = "Evolução da defesa dos Ravens em EPA/Play.",
       subtitle = "Temporada 2020, por semana.",
       y = "EPA por jogada.",
       x = "Semanas.",
       caption = "Figure: @juanseit_ | Data by @nflfastR.") +
  theme_theathletic +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 17))

evolution_d #resultado

qb_report <- pbp %>% #qbs
  filter(posteam == "BAL", season == 2020, down <= 4, week <= 17, !is.na(epa)) %>%
  group_by(name) %>%
  summarize(epa_per_dropback = mean(qb_epa), 
            cpoe = mean(cpoe, na.rm = T), 
            success_rate = mean(success), 
            first_down = mean(first_down), plays = n()) %>%
  ungroup() %>%
  filter(plays >= 10 & plays < 592) %>%
  mutate(pff = c(81.5,52.9,57.4)) %>%
  drop_na() %>%
  gt() %>%
  tab_header(title = md("**Desempenho dos Quarterbacks em 2020.**"),
             subtitle = "Semanas 1 a 17. Mínimo 10 snaps.") %>%
  tab_source_note("Table: @juanseit_ | Data by @nflfastR and PFF.") %>%
  cols_label(name = "Quarterback", 
             epa_per_dropback = "EPA/play", 
             cpoe = "CPOE", success_rate = "SR%", 
             first_down = "First down%", plays = "Snaps", pff = "PFF Grade") %>%
  fmt_number(
    columns = 2:5,
    decimals = 4,
    suffixing = TRUE
  ) %>%
  fmt_number(
    columns = 3,
    decimals = 1,
    suffixing = TRUE
  ) %>%
  cols_align(align = c("center"), columns = TRUE) %>%
  cols_move(columns = vars(pff), after = vars(epa_per_dropback)) %>%
  gt_theme_538(table.width = px(550)) %>%
  data_color(
    columns = vars(pff),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    )
  )

qb_report #resultado

qb_epa_m <- pbp %>%
  filter(season == 2020, down <= 4, week <= 17, !is.na(epa)) %>%
  summarize(epa = mean(qb_epa), cpoe = mean(cpoe, na.rm = TRUE), plays = n()) %>%
  ungroup() %>%
  filter(plays >= 200)

lamar_epa <- pbp %>% #EPA
  filter(name == "L.Jackson", season == 2020, down <= 4, week <= 17, !is.na(epa)) %>%
  group_by(week) %>%
  summarize(epa = mean(qb_epa), week, team = last(defteam)) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr')) %>%
  ggplot(mapping = aes(x = week, y = epa)) +
  geom_line(color = "purple", size = 2) +
  geom_image(aes(image = team_logo_espn)) +
  geom_hline(yintercept = mean(qb_epa_m$epa), color = "white",linetype = "dashed") +
  labs(title = "Evolução de Lamar Jackson em EPA/Play.",
       subtitle = "Temporada 2020, por semana.",
       y = "EPA/Play.",
       x = "Semanas.",
       caption = "Figure: @juanseit_ | Data by @nflfastR.") +
  theme_theathletic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 17))

lamar_cpoe <- pbp %>% #CPOE
  filter(name == "L.Jackson", season == 2020, down <= 4, week <= 17, !is.na(cpoe)) %>%
  group_by(week) %>%
  summarize(cpoe = mean(cpoe, na.rm = TRUE), week, team = last(defteam)) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr')) %>%
  ggplot(mapping = aes(x = week, y = cpoe)) +
  geom_line(color = "purple", size = 2) +
  geom_image(aes(image = team_logo_espn)) +
  geom_hline(yintercept = mean(qb_epa_m$cpoe), color = "white", linetype = "dashed") +
  labs(title = "Evolução de Lamar Jackson em CPOE.",
       subtitle = "Temporada 2020, por semana.",
       y = "CPOE.",
       x = "Semanas.",
       caption = "Figure: @juanseit_ | Data by @nflfastR.") +
  theme_theathletic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 17))
