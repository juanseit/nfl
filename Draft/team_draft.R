library(ggimage)
library(tidyverse)
library(ggtext)
library(ggpubr)

draft_20 <- read_csv("C:/Users/User/Documents/draft_2020.csv")
draft_19 <- read_csv("C:/Users/User/Documents/draft_2019.csv")
draft_18 <- read_csv("C:/Users/User/Documents/draft_2018.csv")
draft_17 <- read_csv("C:/Users/User/Documents/draft_2017.csv")
draft_16 <- read_csv("C:/Users/User/Documents/draft_2016.csv")
draft_value <- read_csv("C:/Users/User/Documents/draft_value.csv")

times_2016 <- tabela_draft_16 %>%
  filter(!is.na(Tm), !is.na(CarAV), !is.na(Pick), Tm == "BAL") %>%
  group_by(Tm) %>%
  summarize(picks = Pick, valor = CarAV, capital = Value) %>%
  ungroup() %>%
  ggplot(aes(x = picks)) +
  geom_line(aes(y=valor, color = "#241773"), lwd = 2) +
  geom_point(aes(y=valor, color = "#241773"), size = 4) +
  geom_line(aes(y=capital), color = "white", alpha = .5, lwd =1.5) +
  scale_color_identity() +
  theme_theathletic() +
  labs(x = "Picks.",
       y = "AV.")
  
times_2017 <- tabela_draft_17 %>%
  filter(!is.na(Tm), !is.na(CarAV), !is.na(Pick), Tm == "BAL") %>%
  group_by(Tm) %>%
  summarize(picks = Pick, valor = CarAV, capital = Value) %>%
  ungroup() %>%
  ggplot(aes(x = picks)) +
  geom_line(aes(y=valor, color = "#241773"), lwd = 2) +
  geom_point(aes(y=valor, color = "#241773"), size = 4) +
  geom_line(aes(y=capital), color = "white", alpha =.5, lwd = 1.5) +
  scale_color_identity() +
  theme_theathletic() +
  labs(x = "Picks.",
       y = "AV.")

times_2018 <- tabela_draft_18 %>%
  filter(!is.na(Tm), !is.na(CarAV), !is.na(Pick), Tm == "BAL") %>%
  group_by(Tm) %>%
  summarize(picks = Pick, valor = CarAV, capital = Value) %>%
  ungroup() %>%
  ggplot(aes(x = picks)) +
  geom_line(aes(y=valor, color = "#241773"), lwd = 2) +
  geom_point(aes(y=valor, color = "#241773"), size = 4) +
  geom_line(aes(y=capital), color = "white", alpha =.5, lwd = 1.5) +
  scale_color_identity() +
  theme_theathletic()+
  labs(x = "Picks.",
       y = "AV.")

times_2019 <- tabela_draft_19 %>%
  filter(!is.na(Tm), !is.na(CarAV), !is.na(Pick), Tm == "BAL") %>%
  group_by(Tm) %>%
  summarize(picks = Pick, valor = CarAV, capital = Value) %>%
  ungroup() %>%
  ggplot(aes(x = picks)) +
  geom_line(aes(y=valor, color = "#241773"), lwd = 2) +
  geom_point(aes(y=valor, color = "#241773"), size = 4) +
  geom_line(aes(y=capital), color = "white", alpha =.5, lwd = 1.5) +
  scale_color_identity() +
  theme_theathletic() +
  labs(x = "Picks.",
       y = "AV.")

draft_bal <- ggarrange(times_2016, times_2017, times_2018, times_2019, 
          labels = c("2016", "2017", "2018","2019"),
          ncol = 2, nrow = 2,
          font.label = list(color="white"))

draft_bal

ggsave("draft_bal.png", width = 10, height = 10, dpi = 300)

card_2016 <- tabela_draft_16 %>%
  filter(!is.na(Tm), !is.na(CarAV), !is.na(Pick), Tm == "ARI") %>%
  group_by(Tm) %>%
  summarize(picks = Pick, valor = CarAV, capital = Value) %>%
  ungroup() %>%
  ggplot(aes(x = picks)) +
  geom_line(aes(y=valor, color = "red"), lwd = 2) +
  geom_point(aes(y=valor, color = "red"), size = 4) +
  geom_line(aes(y=capital), color = "white", alpha = .5, lwd =1.5) +
  scale_color_identity() +
  theme_theathletic() +
  labs(x = "Picks.",
       y = "AV.")

card_2017 <- tabela_draft_17 %>%
  filter(!is.na(Tm), !is.na(CarAV), !is.na(Pick), Tm == "ARI") %>%
  group_by(Tm) %>%
  summarize(picks = Pick, valor = CarAV, capital = Value) %>%
  ungroup() %>%
  ggplot(aes(x = picks)) +
  geom_line(aes(y=valor, color = "red"), lwd = 2) +
  geom_point(aes(y=valor, color = "red"), size = 4) +
  geom_line(aes(y=capital), color = "white", alpha =.5, lwd = 1.5) +
  scale_color_identity() +
  theme_theathletic() +
  labs(x = "Picks.",
       y = "AV.")

card_2018 <- tabela_draft_18 %>%
  filter(!is.na(Tm), !is.na(CarAV), !is.na(Pick), Tm == "ARI") %>%
  group_by(Tm) %>%
  summarize(picks = Pick, valor = CarAV, capital = Value) %>%
  ungroup() %>%
  ggplot(aes(x = picks)) +
  geom_line(aes(y=valor, color = "red"), lwd = 2) +
  geom_point(aes(y=valor, color = "red"), size = 4) +
  geom_line(aes(y=capital), color = "white", alpha =.5, lwd = 1.5) +
  scale_color_identity() +
  theme_theathletic()+
  labs(x = "Picks.",
       y = "AV.")

card_2019 <- tabela_draft_19 %>%
  filter(!is.na(Tm), !is.na(CarAV), !is.na(Pick), Tm == "ARI") %>%
  group_by(Tm) %>%
  summarize(picks = Pick, valor = CarAV, capital = Value) %>%
  ungroup() %>%
  ggplot(aes(x = picks)) +
  geom_line(aes(y=valor, color = "red"), lwd = 2) +
  geom_point(aes(y=valor, color = "red"), size = 4) +
  geom_line(aes(y=capital), color = "white", alpha =.5, lwd = 1.5) +
  scale_color_identity() +
  theme_theathletic() +
  labs(x = "Picks.",
       y = "AV.")

draft_ari <- ggarrange(card_2016, card_2017, card_2018, card_2019, 
                       labels = c("2016", "2017", "2018","2019"),
                       ncol = 2, nrow = 2,
                       font.label = list(color="white"))

draft_ari

ggsave("draft_ari.png", width = 10, height = 10, dpi = 300)
