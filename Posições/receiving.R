library(tidyverse)
library(nflfastR)
library(ggimage)
library(ggrepel)
library(ggtext)

receiving <- read_csv("C:/Users/User/Documents/ngs_2020_receiving.csv")

team_ranks <- receiving %>%
  filter(!is.na(player_display_name), !is.na(team_abbr), player_position == "WR", targets > 40) %>%
  group_by(team_abbr) %>%
  summarize(sep = mean(avg_separation), cpl = mean(catch_percentage), yac_aba = mean(avg_yac_above_expectation)) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c("team_abbr"))

team_ranks %>%
  ggplot(mapping = aes(x = yac_aba, y = sep)) +
  geom_hline(yintercept = mean(team_ranks$sep), color = "white", linetype = "dashed") +
  geom_vline(xintercept = mean(team_ranks$yac_aba), color = "white", linetype = "dashed") +
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) +
  labs(x = "Jardas após recepção acima do esperado.",
       y = "Separação média do defensor mais próximo.",
       title = "Avaliando o corpo de recebedores dos times da NFL em 2020.",
       subtitle = "Capacidade de gerar separação e jardas após recepção. WRs com pelo menos 40 targets.",
       caption = "Gráfico: @juanseit_ | Data by Next Gen Stats.") +
  theme_theathletic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("receiving_1.png", width = 12, height = 9, dpi = 300)

player_ranks <- receiving %>%
  filter(!is.na(player_display_name), !is.na(team_abbr), player_position == "WR", targets > 90) %>%
  group_by(player_display_name, team_abbr) %>%
  summarize(sep = mean(avg_separation), cpl = mean(catch_percentage), share = percent_share_of_intended_air_yards, media_share = mean(share)) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c("team_abbr"))

player_ranks %>%
  ggplot(mapping = aes(x = share, y = sep)) +
  geom_hline(yintercept = mean(player_ranks$sep), color = "white", linetype = "dashed") +
  geom_vline(xintercept = mean(player_ranks$media_share), color = "white", linetype = "dashed") +
  geom_point(fill = player_ranks$team_color, color = player_ranks$team_color2, cex=player_ranks$cpl/8, pch = 21) +
  geom_text_repel(aes(label = player_display_name), color = "white") +
  labs(x = "Percentual das jardas aéreas da equipe.",
       y = "Separação média do defensor mais próximo.",
       title = "Avaliando os recebedores dos times da NFL em 2020.",
       subtitle = "Capacidade de gerar separação e percentual de passes da equipe direcionados ao recebedor. WRs com pelo menos 90 targets.",
       caption = "Gráfico: @juanseit_ | Data by Next Gen Stats.") +
  theme_theathletic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("receiving_2.png", width = 12, height = 9, dpi = 300)