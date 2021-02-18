library(tidyverse)
library(ggrepel)
library(ggtext)
library(nflfastR)
options(scipen = 9999)

#Building pbp with @nflfastR 4.0.0

seasons <- 2018:2020
pbp <- nflfastR::load_pbp(seasons)

pbp %>%
  filter(name %in% c("L.Jackson", "J.Allen", "B.Mayfield", "S.Darnold"), season >= 2018,
         week <= 17, down <= 4, !is.na(down), !is.na(qb_epa)) %>%
  group_by(name, season) %>%
  summarize(epa = mean(qb_epa)) %>%
  ungroup() %>%
  mutate(pff = case_when(season == 2018 & name == "L.Jackson" ~ 58.6,
                         season == 2019 & name == "L.Jackson" ~ 91.1,
                         season == 2020 & name == "L.Jackson" ~ 81.5,
                         season == 2018 & name == "J.Allen" ~ 65.3,
                         season == 2019 & name == "J.Allen" ~ 64.2,
                         season == 2020 & name == "J.Allen" ~ 90.9,
                         season == 2018 & name == "B.Mayfield" ~ 83.2,
                         season == 2019 & name == "B.Mayfield" ~ 74.4,
                         season == 2020 & name == "B.Mayfield" ~ 81.6,
                         season == 2018 & name == "S.Darnold" ~ 64.7,
                         season == 2019 & name == "S.Darnold" ~ 63.6,
                         season == 2020 & name == "S.Darnold" ~ 58.4),
         season_lbl = if_else(season == 2018, "2018",
                              if_else(season == 2019, "2019",
                                      if_else(season == 2020, "2020", ""))),
         label = glue("{name} {season_lbl}")) %>%
  ggplot(mapping = aes(x = pff, y = epa)) +
  geom_point(color = "white", pch = 21, size = 5, fill = "red") +
  scale_fill_identity() +
  stat_smooth(linetype = "dashed", color = "white", method = "lm", se = FALSE) +
  geom_text_repel(aes(label = label), color = "white") +
  labs(x = "PFF Grade.",
       y = "EPA/Play.",
       title = "Desempenho da classe de QBs de 2018.",
       subtitle = "Abaixo da linha representa produção com pouca ajuda do time, e vice e versa.",
       caption = "Gráfico: @juanseit_ | Data by @nflfastR.") +
  theme_theathletic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("classe_2018.png", width = 10, height = 8, dpi = 300)
