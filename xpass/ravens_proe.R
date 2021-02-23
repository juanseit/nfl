library(tidyverse)
library(ggimage)
library(ggthemes)
library(ggtext)
library(nflfastR)
options(scipen = 9999)

#Baixando dados de múltiplas temporadas

seasons <- 2015:2020
pbp <- nflfastR::load_pbp(seasons)

#proe

pa_ravens <- pbp %>%
  filter(season >= 2015, posteam == "BAL", down <= 4, week <= 17, !is.na(down), !is.na(pass_oe),
         vegas_wp > 0.05, vegas_wp < 0.95, half_seconds_remaining > 120) %>%
  group_by(season) %>%
  summarize(proe = mean(pass_oe),
            team = last(posteam),
            ) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr')) %>%
  ggplot(mapping = aes(x = season, y = proe)) +
  geom_line(color = "#330066", size = 2) +
  geom_point(color = "#330066", size = 5) +
  geom_image(aes(max(season), -11.80057365, image =  team_logo_espn),
             asp = 1.618, by = "height", size = 0.15, inherit.aes = F) +
  geom_hline(yintercept = 0, linetype = "solid", color = "white") +
  labs(x = "Temporada.",
       y = "Passes acima do esperado (%).",
       title = "O ataque dos Ravens foi abandonando o jogo aéreo.",
       subtitle = "Percentual de passes acima do esperado entre 2015 e 2020, em contexto neutro.",
       caption = "Gráfico: @juanseit_ | Data by @nflfastR."
  ) +
  theme_theathletic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

pa_ravens

ggsave("proe_ravens.png", width = 10, height = 8, dpi = 300)
