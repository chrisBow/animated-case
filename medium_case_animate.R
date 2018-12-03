# ggplot2 theme to use later

theme_chris <- function (base_size = 12, base_family = "serif", ticks = TRUE) 
{
  ret <- theme_bw(base_family = base_family, base_size = base_size) + 
    theme(legend.background = element_blank(), legend.key = element_blank(), 
          panel.border = element_blank(), 
          strip.background = element_blank(), 
          panel.background = element_rect(fill = "#94B1C533", colour = NA),
          plot.background = element_rect(fill = "#ffffff"),
          axis.line = element_blank(), 
          panel.grid = element_blank(),
          axis.text.x = element_text(colour = "#2a3132"),
          axis.title.x = element_text(colour = "#2a3132"),
          axis.title.y = element_text(colour="#2a3132"),
          axis.text.y = element_text(colour="#2a3132"),
          axis.title = element_text(colour = "#2a3132"),
          plot.title = element_text(colour = "#2a3132", 
                                    margin = margin(0,0,10,0)),
          plot.subtitle = element_text(colour = "#2a3132"),
          plot.caption = element_text(colour = "#2a3132"),
          legend.title = element_text(colour = "#2a3132"),
          legend.text = element_text(colour = "#2a3132"))
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  ret
}


# import yearly data (total, summed values, not means or medians)
# dataset compiled from historical Ross-CASE reports

library(readr)

fund_df <- read_csv("year_sum.csv")

# quick look at data

library(dplyr)

glimpse(fund_df)

library(ggplot2)

ggplot(fund_df, aes(x = year,
                    y = new_funds_raised)) +
  geom_line()


# create contactable alumni x100 variable to place values on equivalent scale

fund_df <-
  fund_df %>%
  mutate(contact_alum_x100 = contactable_alumni * 100)


# create tidy dataframe

library(tidyr)

fund_tidy <-
  fund_df %>%
  gather(kpi, value, - year) %>%
  mutate(kpi = as.factor(kpi))

glimpse(fund_tidy)


# create animated plot

library(gganimate)
library(transformr)

first_animate <-
  fund_tidy %>%
  filter(kpi != "contactable_alumni") %>%
  ggplot(aes(x = year, 
             y = value,
             colour = kpi)) +
  geom_line() +
  transition_reveal(kpi, year) +
  labs(title = "Trends in University Fundraising KPIs Over Time",
       subtitle = "Data from Ross-CASE reports",
       x = "Year",
       y = 'Value',
       caption = "y axis labelling omitted due to differences in scale between KPIs",
       colour = "KPI") +
  scale_colour_discrete(labels = c("Cash received", 
                                   "Contactable alumni",
                                   "Fundraising staff",
                                   "New funds raised")) +
  scale_y_discrete(labels = NULL) +
  theme_chris()


# animate and save

first_animated <- animate(first_animate, height = 500, width = 800)

anim_save("first_animated.gif", animation = first_animated)



# create non-animated plot with trendlines

fund_tidy %>%
  filter(kpi != "contactable_alumni") %>%
  ggplot(aes(x = year, 
             y = value,
             colour = kpi)) +
  geom_line() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE) +
  labs(title = "Trends in University Fundraising KPIs Over Time",
       subtitle = "Data from Ross-CASE reports",
       x = "Year",
       y = 'Value',
       caption = "y axis labelling omitted due to differences in scale between KPIs",
       colour = "KPI") +
  scale_colour_discrete(labels = c("Cash received", 
                                   "Contactable alumni",
                                   "Fundraising staff",
                                   "New funds raised")) +
  scale_y_discrete(labels = NULL) +
  theme_chris()


#---- create linear model and augmented dataframe ----

# build pre-filtered dataframe

fund_tidy2 <-
  fund_tidy %>%
  filter(kpi != "contactable_alumni")


# build linear model

lin_mod <- lm(value ~ year + kpi, data = fund_tidy2)


# augment linear model to produce tidy dataframe with fitted values

library(broom)

aug_mod <- augment(lin_mod)


# create animated graph

aug_animate <-
  aug_mod %>%
  ggplot(aes(x = year, 
             y = value,
             colour = kpi)) +
  geom_line(aes(group = kpi, y = .fitted), size = 0.5, linetype = "dashed") +
  geom_point(size = 2) +
  geom_line(aes(group = kpi)) +
  transition_reveal(kpi, year) +
  labs(title = "Trends in University Fundraising KPIs Over Time",
       subtitle = "Data from Ross-CASE reports",
       x = "Year",
       y = 'Value',
       caption = "y axis labelling omitted due to differences in scale between KPIs",
       colour = "KPI") +
  scale_colour_discrete(labels = c("Cash received", 
                                   "Contactable alumni",
                                   "Fundraising staff",
                                   "New funds raised")) +
  theme_chris()

# animate and save

aug_animated <- animate(aug_animate, height = 500, width = 800)

anim_save("aug_animated.gif", animation = aug_animated)



#---- build multiple models for animated plot with trendlines ----

# build nested tibble

fund_nested <-
  fund_tidy2 %>%
  group_by(kpi) %>%
  nest()


# build separate regression models

fund_models <- 
  fund_nested %>%
  mutate(lm_mod = map(data, 
                     ~lm(formula = value ~ year, 
                         data = .x)))


# augment models and unnest tibble 

fund_models_aug <-
  fund_models %>%
  mutate(aug = map(lm_mod, ~augment(.x))) %>% 
  unnest(aug)


case_animate <-
fund_models_aug %>%
  ggplot(aes(x = year, 
             y = value,
             colour = kpi)) +
  geom_line(aes(group = kpi, y = .fitted), size = 0.5, linetype = "dashed") +
  geom_point(size = 2) +
  geom_line(aes(group = kpi)) +
  transition_reveal(kpi, year) +
  labs(title = "Trends in University Fundraising KPIs Over Time",
       subtitle = "Data from Ross-CASE reports",
       x = "Year",
       y = 'Value',
       caption = "y axis labelling omitted due to differences in scale between KPIs",
       colour = "KPI") +
  scale_colour_discrete(labels = c("Cash received", 
                                   "Contactable alumni",
                                   "Fundraising staff",
                                   "New funds raised")) +
  scale_fill_discrete() +
  theme_chris()


# animate and save

case_animation <- animate(case_animate, height = 500, width = 800)

anim_save("case_animation.gif", animation = case_animation)
