library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(ggthemes)
options(scipen = 9999)

#Baixando dados de múltiplas temporadas

seasons_2 <- 2018:2020
pbp_2 <- purrr::map_df(seasons_2, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
}) %>% add_xpass()

passoe <- pbp_2 %>% #bar plot
  filter(season == 2020, week <= 17, down <= 2, !is.na(pass_oe), vegas_wp > 0.05, vegas_wp < 0.95, half_seconds_remaining > 120) %>%
  group_by(posteam) %>%
  summarize(pass_oe = mean(pass_oe), team = last(posteam)) %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'), size = 0.03, asp = 16/9) %>%
  ungroup() %>%
  arrange(-pass_oe)

passoe %>%
  ggplot(mapping = aes(x = reorder(team,pass_oe), y = pass_oe)) +
  geom_col(aes(fill = if_else(pass_oe < 0, "#013369", "#D50A0A")), colour = "black", width = 0.4) +
  geom_image(aes(image = team_logo_espn)) +
  labs(x = "Times.",
       y = "Percentual de passes acima do esperado.",
       title = "Passes acima do esperado em early-downs (2020).",
       caption = "Figure: @juanseit_ | Data: @nflfastR",
       subtitle = "Probabilidade de vitória entre 95% e 5%, antes do two-minute warning.") +
  theme_539()+
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
)

ggsave("xpass.png", height = 8, width = 10, dpi = 300)

media_passes <- pbp_2 %>%
  filter(season == 2020, down <= 2, week <= 17, !is.na(pass_oe),!is.na(epa), vegas_wp > 0.05, vegas_wp < 0.95, half_seconds_remaining > 120) %>%
  group_by(posteam) %>%
  summarize(epa = mean(epa), pass_heavy = mean(pass_oe)) %>%
  ungroup()

ef_passes <- pbp_2 %>% #eficiência do ataque
  filter(season == 2020, down <= 2, !is.na(epa), !is.na(pass_oe), season_type == "REG", vegas_wp > 0.05, vegas_wp < 0.95, half_seconds_remaining > 120) %>%
  group_by(posteam) %>%
  summarize(media_epa = sum(epa)/sum(pass), pass_heavy = mean(pass_oe), team = last(posteam)) %>%
  ungroup() %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr')) %>%
  ggplot(mapping = aes(x = pass_heavy, y = media_epa)) +
  geom_image(aes(image = team_logo_espn),size = 0.03, asp = 16/9) +
  geom_hline(yintercept = mean(media_passes$epa), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(media_passes$pass_heavy), color = "red", linetype = "dashed", alpha=0.5) +
  labs(x = "Frequência de passes acima do esperado em early-downs.",
       y = "EPA/passe em early-downs.",
       title = "Eficiência de passes em early-downs (2020).",
       caption = "Figure: @juanseit_ | Data: @nflfastR",
       subtitle = "Probabilidade de vitória entre 95% e 5%, antes do two-minute warning.") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ef_passes
