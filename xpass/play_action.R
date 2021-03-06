library(tidyverse)
library(readxl)
library(nflfastR)
library(ggimage)
library(ggtext)
options(scipen = 9999)

# play action data from Pro Football Reference

pfr <- read_xlsx("C:/Users/User/Documents/play_action.xlsx")

pfr_t <- pfr %>%
  select(Tm, PassAtt_PA, PassAtt_RPO, Att) %>%
  rename(team = Tm) %>%
  mutate(team = case_when(
    team == "Arizona Cardinals" ~ "ARI",
    team == "Atlanta Falcons" ~ "ATL",
    team == "Buffalo Bills" ~ "BUF",
    team == "Baltimore Ravens" ~ "BAL",
    team == "Carolina Panthers" ~ "CAR",
    team == "Chicago Bears" ~ "CHI",
    team == "Cincinnati Bengals" ~ "CIN",
    team == "Cleveland Browns" ~"CLE",
    team == "Dallas Cowboys" ~ "DAL",
    team == "Denver Broncos" ~ "DEN",
    team == "Detroit Lions" ~ "DET",
    team == "Green Bay Packers" ~ "GB",
    team == "Houston Texans" ~ "HOU",
    team == "Indianapolis Colts" ~ "IND",
    team == "Jacksonville Jaguars" ~ "JAX",
    team == "Kansas City Chiefs" ~ "KC",
    team == "Los Angeles Rams" ~ "LA",
    team == "Los Angeles Chargers" ~ "LAC",
    team == "Las Vegas Raiders" ~ "LV",
    team == "Miami Dolphins" ~ "MIA",
    team == "Minnesota Vikings" ~ "MIN",
    team == "New England Patriots" ~ "NE",
    team == "New Orleans Saints" ~ "NO",
    team == "New York Giants" ~ "NYG",
    team == "New York Jets" ~ "NYJ",
    team == "Philadelphia Eagles" ~ "PHI",
    team == "Pittsburgh Steelers" ~ "PIT",
    team == "Seattle Seahawks" ~ "SEA",
    team == "San Francisco 49ers" ~ "SF",
    team == "Tampa Bay Buccaneers" ~ "TB",
    team == "Tennessee Titans" ~ "TEN",
    team == "Washington Football Team" ~ "WAS"
  ),
  Passattempts_PA = as.numeric(PassAtt_PA),
  Passattempts_RPO = as.numeric(PassAtt_RPO),
  Passattempts = as.numeric(Att)) %>%
  left_join(nflfastR::teams_colors_logos, by = c("team" = "team_abbr"))

pfr_t %>%
  filter(!is.na(team), !is.na(Passattempts), !is.na(Passattempts_PA)) %>%
  group_by(team) %>%
  summarize(att = Passattempts, att_pa = PassAtt_PA, md_pa = att_pa/att) %>%
  ungroup() %>%
  ggplot(mapping = aes(y = md_pa, x = reorder(team,md_pa))) +
  geom_col(fill = pfr_t$team_color, color = pfr_t$team_color2, position = "dodge") +
  geom_hline(yintercept = 0, linetype = "solid", color = "white") +
  scale_fill_identity() +
  scale_color_identity() +
  geom_image(aes(image = pfr_t$team_logo_espn)) +
  labs(y = "Porcentagem de uso de Play-Action.",
       title = "Jogadas de Play-Action resultam em maior EPA/Play.",
       subtitle = "Uso de Play-Action em relação ao total de passes em 2020.",
       caption = "Gráfico: @juanseit_ | Data by Pro Football Reference.") +
  theme_theathletic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("play_action.png", height = 9, width = 12, dpi = 300)
  
  
  
