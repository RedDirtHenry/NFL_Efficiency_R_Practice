library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggtext)
library(ggrepel)

# get data
seasons <- 2016:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  ) %>%
    filter(posteam == "DAL")
})

df <- pbp %>%
  # get to normal plays
  filter(!is.na(down), rush == 1 | pass == 1) %>%
  group_by(game_id) %>%
  mutate(
    # does one team have less than 10% chance to win?
    # note that i'm not using vegas_wp here because there are games 
    # where the seahawks are huge favorites and there are basically no plays left
    under_wp = if_else(between(wp, .10, .90), 0, 1),
    
    # game is "over" when we've gone under 10% WP
    over = if_else(cumsum(under_wp) > 0, 1, 0),
    
    # for calculating dak's epa
    dak_epa = if_else(name == "D.Prescott", qb_epa, NA_real_),
    
    # for help making labels
    home = if_else(home_team == "DAL", 1, 0)
  ) %>%
  # want plays where game isn't "over" and early downs
  filter(over == 0, down <= 2) %>%
  summarise(
    pass = mean(pass),
    season = dplyr::first(season),
    week = dplyr::first(week),
    dak_epa = mean(dak_epa, na.rm = T),
    defteam = dplyr::first(defteam),
    home = dplyr::first(home)
  ) %>%
  ungroup() %>%
  mutate(
    home_lbl = if_else(home == 1, "", "@"),
    playoff_lbl = if_else(week > 17, "*", ""),
    label = glue::glue("{home_lbl}{defteam}{substr(game_id, 3, 4)}{playoff_lbl}"),
    # for point  colors
    era = case_when(
      # ATL game in 2020
      season < 2020 & defteam == "PHI" | defteam=="NYG" | defteam=="WAS" ~ 1,
      # first 8 games of 2020
      season == 2020 & week <= 5  ~ 2,
      # all the other games in 2020
      TRUE ~ 3
    )
  )

color_easy = c("#F8766D", "#2CAB89", "#000680")[df$era]

# labels on the plot
text_df <- tibble(
  label = c(
    "Other games in <span style='color:#000680'>**2016-2019**</span>",
    "NFCE Games in <span style='color:#F8766D'>**2016-2020**</span>",
    "Games <span style='color:#2CAB89'>**2020**</span>"
  ),
  x = c(0.65, .65, .675),
  y = c(1.95, 1.75, 1.55),
  angle = c(0, 0, 0),
  color = c("black", "black", "black")
)

# make the plot
df %>%
  ggplot(aes(pass, dak_epa, label = label)) + 
  geom_hline(aes(yintercept = mean(dak_epa))) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, color = "black") +
  geom_ribbon(stat='smooth', se=TRUE, alpha=0.1, 
              aes(color = NULL)) +
  geom_point(size = 3, color = factor(color_easy)) +
  geom_text_repel(data = filter(df, 
                                pass < .35 | pass > .65 | dak_epa > .8 | dak_epa < -.25 | era > 1
  )
  , size = 4) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(size = 16, hjust = 0.5),
    plot.subtitle = element_markdown(size = 12, hjust = 0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold")
  ) +
  labs(
    x = "Dropback %",
    y = "EPA per play",
    title = "D. Prescott EPA per play vs Early Down Dropback %",
    subtitle = "Early-down efficiency, game in contention (until one team has < 10% chance to win)",
    caption = "Credit to @benbbaldwin for source code & @nflfastR | * = playoff game"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0,.1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0, .01)) +
  geom_richtext(data = text_df,   
                aes(
                  x, y, label = label, angle = angle
                ), color = "black", fill = NA, label.color = NA, size = 4
  ) +
  annotate("point", x = .706, y = 1.95, colour = "#000680", size = 3) +
  annotate("point", x = .706, y = 1.75, colour = "#F8766D", size = 3) +
  annotate("point", x = .706, y = 1.55, colour = "#2CAB89", size = 3) 

ggsave("img/dak_pass_freq.jpg")
