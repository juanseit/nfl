library(nflfastR)
library(tidyverse)
library(ggtext)
library(ggimage)
library(zoo)

rol_pbp <- load_pbp(2020)

media_epa <- rol_pbp %>%
  filter(week <= 17, down <= 4, !is.na(down), !is.na(epa)) %>%
  group_by(posteam) %>%
  summarize(epa = mean(epa)) %>%
  ungroup()

calc_media <- rol_pbp %>%
  filter(week <= 17, down <= 4, !is.na(down), !is.na(epa), posteam %in% c("BAL","CIN","PIT","CLE")) %>%
  group_by(posteam) %>%
  summarize(movel = rollmean(epa, 200, fill = NA, align = "right"),
            jogada = seq_along(play)) %>%
  ungroup()

calc_media <- calc_media %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

calc_media %>%
  ggplot(mapping = aes(x = jogada, y = movel)) +
  geom_line(size = 2, color = calc_media$team_color) +
  geom_image(aes(700,.25,image = team_logo_espn), asp = 16/9) +
  geom_hline(yintercept = mean(media_epa$epa), color = "black", linetype = "dashed") +
  scale_color_identity() +
  labs(title = "Médias moveis dos ataques da AFC North em 2020, em EPA/Play.",
       subtitle = "Média movel de 200 jogadas.",
       caption = "Gráfico: @juanseit_ | Data by @nflfastR.",
       x = "Jogadas.",
       y = "EPA/Play movel.") +
  theme_bw() +
  theme(
    line = element_line(lineend = 'round', color='darkblue'),
    text = element_text(color='darkblue'),
    plot.background = element_rect(fill = 'grey95', color = 'transparent'),
    panel.border = element_rect(color = 'darkblue', fill = NA),
    panel.background = element_rect(fill = 'white', color = 'transparent'),
    axis.ticks = element_line(color = 'darkblue', size = 0.5),
    axis.ticks.length = unit(2.75, 'pt'),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7, color = 'darkblue'),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 5),
    legend.background = element_rect(fill = 'grey90', color = 'darkblue'),
    legend.key = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color='grey85', size = 0.3),
    axis.title.y = element_text(vjust = 0.5),
    strip.background = element_rect(color = "darkblue")
  ) +
  facet_wrap(~ posteam, nrow = 2) +
  xlim(200,1200)

ggsave("medias_epa.png", dpi = 300)
  
  
