#set up
install.packages("pacman")
pacman::p_load(dplyr, tidyverse, WDI, ggplot2, ggthemes, here)

#task 1: loading WDI data on refugees and save it as csv
WDI::WDIsearch('refugee*')
dat = WDI::WDI(indicator=c('SM.POP.REFG'), start=1960, end=2021)
dat %>% write_csv(here::here("data-raw","refugee_stats.csv"))

#task 2: get the number of refugees (all countries)
ref_world <- dat %>%
  filter(country=="World" & !is.na(SM.POP.REFG)) %>%
  mutate(SM.POP.REFG = SM.POP.REFG/1000000)
ref_world %>% write_csv(here::here("data-raw","refugee_stats_world.csv"))

#task 3: visualize trend
p <- ggplot(ref_world, aes(x = year, y = SM.POP.REFG)) +
  geom_line() +
  labs(title = "Trend in the number of refugees",
       x = "Year",
       y = "Number of refugees (in millions)") +
  theme(axis.title.x = element_text(size = 15),  # Adjust size of x-axis title
        axis.title.y = element_text(size = 15))
p

## make it economist style
p + theme_economist() +
  scale_colour_economist()

ggsave(here::here("outputs","refugee_trend.png"))

#task 4: plot top 10 countries hosting refugees
ref_world %>%
  arrange(-SM.POP.REFG) %>%
  mutate(rank = seq(1:dim(.)[1])) %>%
  filter(rank < 10)