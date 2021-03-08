library(tidyverse)
library(ggimage)
library(ggtext)
library(ggrepel)
library(nflfastR)

ngs_passing <- read_csv("C:/Users/User/Desktop/ngs_2020_passing.csv")
view(ngs_passing)

medias <- ngs_passing %>%
  filter(!is.na(player_short_name), !is.na(avg_time_to_throw), !is.na(avg_intended_air_yards)) %>%
  group_by(season) %>%
  summarize(adot_m = mean(avg_intended_air_yards), time_m = mean(avg_time_to_throw)) %>%
  ungroup() %>%
  arrange(adot_m)

ngs_passing %>%
  filter(!is.na(player_short_name), !is.na(avg_time_to_throw), !is.na(avg_intended_air_yards)) %>%
  group_by(player_short_name, team_abbr) %>%
  summarize(adot = avg_intended_air_yards, time = avg_time_to_throw, passes = completions) %>%
  filter(passes > 200) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c("team_abbr")) %>%
  ggplot(mapping = aes(x = time, y = adot)) +
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) +
  geom_text_repel(aes(label = player_short_name), color = "white") +
  geom_hline(yintercept = medias$adot_m, linetype = "dashed", color = "white") +
  geom_vline(xintercept = medias$time_m, linetype = "dashed", color = "white") +
  labs(title = "Gunslingers usam maior tempo para encontrar big plays.",
       subtitle = "Profundidade média dos passes em relação ao tempo para lançar. Mínimo 200 passes completos.",
       x = "Tempo médio.",
       y = "Profundidade média do passe.",
       caption = "Gráfico: @juanseit_ | Data by Next Gen Stats.") +
  theme_theathletic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("gunslingers.png", height = 9, width = 12, dpi = 300)