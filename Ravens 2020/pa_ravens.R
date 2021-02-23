library(tidyverse)
library(ggimage)
library(ggthemes)
library(nflfastR)
options(scipen = 9999)

#Baixando dados de múltiplas temporadas

seasons <- 2015:2020
pbp <- nflfastR::load_pbp(seasons)

pa_t <- pbp %>%
  filter(season >= 2015, shotgun == 0, down <= 4, week <= 17, !is.na(pass), !is.na(down), !is.na(rush)) %>%
  group_by(posteam, season) %>%
  summarize(pass_p = sum(if_else(pass == 1, 1, 0)),
            rush_p = sum(if_else(rush == 1, 1, 0)),
            plays = pass_p + rush_p,
            play_action_rate_t = pass_p/plays) %>%
  ungroup()
  
#play-action rate

pa_ravens <- pbp %>%
  filter(season >= 2015, posteam == "BAL", shotgun == 0, down <= 4, week <= 17, !is.na(pass), !is.na(down), !is.na(rush)) %>%
  group_by(season) %>%
  summarize(pass_p = sum(if_else(pass == 1, 1, 0)),
            rush_p = sum(if_else(rush == 1, 1, 0)),
            plays = pass_p + rush_p,
            play_action_rate = pass_p/plays,
            team = last(posteam)
            ) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr')) %>%
  ggplot(mapping = aes(x = season, y = play_action_rate)) +
  geom_line(color = "#330066", size = 2) +
  geom_point(color = "#330066", size = 5) +
  geom_image(aes(max(season), 0.1612903, image =  team_logo_espn),
             asp = 1.618, by = "height", size = 0.15, inherit.aes = F) +
  geom_hline(yintercept = mean(pa_t$play_action_rate_t), linetype = "solid", color = "white") +
  labs(x = "Temporada.",
       y = "Play-Action %.",
       title = "Uso de Play-Action pelo ataque dos Ravens.",
       subtitle = "Entre 2015 e 2020, Play-Action considerados como passes em formação pistol.",
       caption = "Gráfico: @juanseit_ | Data by @nflfastR."
  ) +
  theme_theathletic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

pa_ravens

ggsave("pa_ravens.png", width = 10, height = 8, dpi = 300)
