#rbwr
library(tidyverse)
library(readxl)
library(ggthemes)
library(nflfastR)
library(data.table)
library(furrr)

rbwr <- read_xlsx("C:/Users/User/Downloads/rbwr.xlsx")
rbwr

seasons_3 = 2020
pbp_3 <- purrr::map_df(seasons_3, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

pbp_3 <- as.data.table(pbp_3)
pbp_3 <- pbp_3[!is.na(rusher_id) & !is.na(epa)]
rus_pbp <- decode_player_ids(pbp_3)
rus_pbp <- rus_pbp[!is.na(rusher_id)]
rosters <- as.data.table(nflfastR::fast_scraper_roster(2020, pp = TRUE))
roster_subset <- rosters[!is.na(gsis_id), .N, by = "season,gsis_id,team,position"]
rus_pbp[roster_subset, ruspos := i.position, on = .(rusher_id = gsis_id, season = season)]


view(rus_pbp)

pbp_rbwr <- merge(rus_pbp, rbwr, by = c("posteam"))

rb_3 <- pbp_rbwr %>%
  filter(ruspos %in% c('RB'), rush == 1, down <= 4, week <= 17, !is.na(epa), !is.na(down)) %>%
  group_by(name) %>%
  summarize(epa_per_rush = mean(epa), 
            rbwr = mean(rbwr), plays = n()) %>%
  ungroup() %>%
  arrange(-epa_per_rush) %>%
  filter(plays >= 100)

lm_rb <- rb_3 %>%
  ggplot(mapping = aes(x = rbwr, y = epa_per_rush)) +
  geom_point(color = "white", fill = "red", size =  4, pch = 21) +
  stat_smooth(method = "lm", se = FALSE, color = "white") + 
  geom_text(aes(0.68,0.1,label = "R^2 =  0.2415")) +
  labs(x = "Run Block Win Rate (%)",
       y = "EPA/Play",
       title = "Como as linhas ofensivas impactam o desempenho dos Running Backs.",
       subtitle = "Mínimo 100 corridas por RB.",
       caption = "Gráfico: @juanseit_ | Data by @nflfastR and ESPN.") +
theme_theathletic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("rbwr_rb.png", width = 10, height = 7, dpi = 300)

rb_3 %>%
  lm(epa_per_rush ~ rbwr, data = .) %>%
  summary()
