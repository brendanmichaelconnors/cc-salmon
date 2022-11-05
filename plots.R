library(tidyverse)

theme_sleek <- function(base_size = 11, base_family = "") {
  half_line <- base_size/2
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "grey30"),
      strip.text.y = element_text(colour = "grey30"),
      axis.text = element_text(colour = "grey30"),
      axis.title = element_text(colour = "grey30"),
      legend.title = element_text(colour = "grey30", size = rel(0.9)),
      panel.border = element_rect(fill = NA, colour = "grey70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      legend.text = element_text(size = rel(0.7), colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = "grey30", size = rel(1)),
      plot.subtitle = element_text(colour = "grey30", size = rel(.85))
    )
}

# load data ----
er_data_pse <- read.csv("data/CU-ERs-Nov_2_2022_11_11_22_QueryResults.csv") # from PSE
resid_data_pse <- read.csv("data/CU-resids-Nov_4_2022_14_57_48_QueryResults.csv") # from PSE

# er plots ----
er_data <- er_data_pse %>%
  group_by(species, year) %>%
  summarise(avg_er = mean(datavalue)) %>%
  select(species, year,avg_er)

ggplot(er_data, aes( y = avg_er*100, x = year)) +
  geom_line(size=1.5, color="grey") +
  labs(x = "Year", y = "Harvest rate (%)") +
  facet_wrap(~species) +
  theme_sleek() 

ggsave("figs/all-er-avg.jpeg", width = 8, height = 5, units = "in")

# resid plots ----

data_resid <- resid_data_pse %>%
  pivot_wider(names_from = parameter, values_from = datavalue) %>%
  drop_na(Recruits) %>%
  mutate(prod = log(Recruits/Spawners)) %>%
  drop_na(prod) %>%
  group_by(species, year) %>%
  summarise(avg_prod = mean(prod)) %>%
  mutate(smooth_avg_prod = zoo::rollmean(avg_prod, k = 5, fill = NA, align = "right"))%>%
  mutate(smooth_10_avg_prod = zoo::rollmean(avg_prod, k = 10, fill = NA, align = "right"))%>%
  select(species, year,avg_prod,smooth_avg_prod,smooth_10_avg_prod)

ggplot(data_resid%>%filter(species != "River sockeye", year<2012), aes( y = smooth_avg_prod, x = year)) +
  geom_line(size=1.5, color="grey") +
  labs(x = "Brood year", y = "Productivity index") +
  facet_wrap(~species) +
  theme_sleek() 

ggsave("figs/all-prod-avg.jpeg", width = 6, height = 4, units = "in")

ggplot(data_resid%>%filter(species != "River sockeye", year<2012), aes( y = smooth_10_avg_prod, x = year)) +
  geom_line(size=1.5, color="grey") +
  labs(x = "Brood year", y = "Productivity index (10 yr. mean)") +
  facet_wrap(~species) +
  theme_sleek() 

ggsave("figs/all-prod-10_avg.jpeg", width = 7, height = 4, units = "in")
