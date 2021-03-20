library(ggimage)
library(tidyverse)
library(ggtext)

draft_20 <- read_csv("C:/Users/User/Documents/draft_2020.csv")
draft_19 <- read_csv("C:/Users/User/Documents/draft_2019.csv")
draft_18 <- read_csv("C:/Users/User/Documents/draft_2018.csv")
draft_17 <- read_csv("C:/Users/User/Documents/draft_2017.csv")
draft_16 <- read_csv("C:/Users/User/Documents/draft_2016.csv")
draft_value <- read_csv("C:/Users/User/Documents/draft_value.csv")

tabela_draft_20 <- draft_20 %>%
  left_join(draft_value, by ="Pick")
tabela_draft_19 <- draft_19 %>%
  left_join(draft_value, by ="Pick")
tabela_draft_18 <- draft_18 %>%
  left_join(draft_value, by ="Pick")
tabela_draft_17 <- draft_17 %>%
  left_join(draft_value, by ="Pick")
tabela_draft_16 <- draft_16 %>%
  left_join(draft_value, by ="Pick")

valor_2020 <- tabela_draft_20 %>%
  filter(!is.na(DrAV)) %>%
  group_by(Tm) %>%
  summarize(capital_2020 = sum(Value), av_2020 = sum(DrAV)) %>%
  ungroup()

valor_2020 <- valor_2020 %>%
  rename(team = Tm) %>%
  mutate(team = case_when(
    team == "GNB" ~ "GB",
    team == "KAN" ~ "KC",
    team == "NOR" ~ "NO",
    team == "NWE" ~ "NE",
    team == "SFO" ~ "SF",
    team == "TAM" ~ "TB",
    team == "LVR" ~ "LV",
    TRUE ~ team
  ))

valor_2019 <- tabela_draft_19 %>%
  filter(!is.na(DrAV)) %>%
  group_by(Tm) %>%
  summarize(capital_2019 = sum(Value), av_2019 = sum(DrAV)) %>%
  ungroup()

valor_2019 <- valor_2019 %>%
  rename(team = Tm) %>%
  mutate(team = case_when(
    team == "GNB" ~ "GB",
    team == "KAN" ~ "KC",
    team == "NOR" ~ "NO",
    team == "NWE" ~ "NE",
    team == "SFO" ~ "SF",
    team == "TAM" ~ "TB",
    team == "LVR" ~ "LV",
    TRUE ~ team
  ))

valor_2018 <- tabela_draft_18 %>%
  filter(!is.na(DrAV)) %>%
  group_by(Tm) %>%
  summarize(capital_2018 = sum(Value), av_2018 = sum(DrAV)) %>%
  ungroup()

valor_2018 <- valor_2018 %>%
  rename(team = Tm) %>%
  mutate(team = case_when(
    team == "GNB" ~ "GB",
    team == "KAN" ~ "KC",
    team == "NOR" ~ "NO",
    team == "NWE" ~ "NE",
    team == "SFO" ~ "SF",
    team == "TAM" ~ "TB",
    team == "LVR" ~ "LV",
    TRUE ~ team
  ))

valor_2017 <- tabela_draft_17 %>%
  filter(!is.na(DrAV)) %>%
  group_by(Tm) %>%
  summarize(capital_2017 = sum(Value), av_2017 = sum(DrAV)) %>%
  ungroup()

valor_2017 <- valor_2017 %>%
  rename(team = Tm) %>%
  mutate(team = case_when(
    team == "GNB" ~ "GB",
    team == "KAN" ~ "KC",
    team == "NOR" ~ "NO",
    team == "NWE" ~ "NE",
    team == "SFO" ~ "SF",
    team == "TAM" ~ "TB",
    team == "LVR" ~ "LV",
    TRUE ~ team
  ))

valor_2016 <- tabela_draft_16 %>%
  filter(!is.na(DrAV)) %>%
  group_by(Tm) %>%
  summarize(capital_2016 = sum(Value), av_2016 = sum(DrAV)) %>%
  ungroup()

valor_2016 <- valor_2016 %>%
  rename(team = Tm) %>%
  mutate(team = case_when(
    team == "GNB" ~ "GB",
    team == "KAN" ~ "KC",
    team == "NOR" ~ "NO",
    team == "NWE" ~ "NE",
    team == "SFO" ~ "SF",
    team == "TAM" ~ "TB",
    team == "LVR" ~ "LV",
    TRUE ~ team
  ))

tabela_draft <- valor_2020 %>%
  inner_join(valor_2019, by = "team") %>%
  inner_join(valor_2018, by = "team") %>%
  inner_join(valor_2017, by = "team") %>%
  inner_join(valor_2016, by = "team")

tabela_draft %>%
  group_by(team) %>%
  summarize(capt = capital_2020+capital_2019+capital_2018+capital_2017+capital_2016, 
            av_t = av_2020+av_2019+av_2018+av_2017+av_2016) %>%
  ungroup() %>%
  left_join(nflfastR::teams_colors_logos, by = c("team" = "team_abbr")) %>%
  ggplot(mapping = aes(x = capt, y = av_t)) +
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) +
  labs(x = "Draft Capital (Em AV médio por Pick).",
       y = "AV acumulado pelos jogadores escolhidos.",
       title = "Os times que melhor draftam na NFL.",
       subtitle = "Usando o AV do Pro Football Reference para qualicar os drafts de 2016 a 2020.",
       caption = "Gráfico: @juanseit_ | Data by Pro Football Reference.") +
  theme_theathletic() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("draft.png", width = 12, height = 9, dpi = 300)




