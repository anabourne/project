## set up ----
## Load Libraries
library(dplyr)
library(ggplot2)

##load data
individual <- readr::read_csv(
  here::here("data", "individual.csv")
) %>%
  select(stem_diameter, height, growth_form)

## substting data
analysis_df <- individual %>%
  filter(complete.cases(.), growth_form != "liana")

## order levels
gf_levels <- table(analysis_df$growth_form) %>%
  sort() %>% 
  names()

analysis_df <- analysis_df %>%
  mutate(growth_form = factor(growth_form,
                              levels = gf_levels
  ))


## Plots ----
# Figure 1: barplot
analysis_df %>%
  ggplot(aes(y = growth_form, colour = growth_form, fill = growth_form)) +
  geom_bar(alpha = 0.5, show.legend = FALSE)

# Figure 2: Violin plots of stem diameter and height across growth forms
analysis_df %>%
  tidyr::pivot_longer(
    cols = c(stem_diameter, height),
    names_to = "var",
    values_to = "value"
  ) %>%
  ggplot(aes(x = log(value), y = growth_form, colour = growth_form, 
             fill = growth_form)) +
  geom_violin(alpha = 0.5, trim = T) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  facet_grid(~var)

## Fit overall linear model ----
lm_overal <- lm(log(stem_diameter) ~ log(height), data = analysis_df)
lm_overal
# intercept: were my line cross the y axis
# log: slope of my line

lm_overal %>%
  broom::glance() #returns a tibble with important stats info

lm_overal %>%
  broom::tidy() # information about the coefficients of our model: estimate, 
                # std, error and p values

analysis_df %>%
  ggplot(aes(x = log(height), y = log(stem_diameter))) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") + # line
  xlab("Log of the height(m)") + 
  ylab("Log of stem diameter (m)") + 
  theme_linedraw()


## Fit linear model with a growth form interaction
lm_growth <- lm(log(stem_diameter) ~ log(height) * growth_form,
                data = analysis_df)
lm_growth %>%
  broom::glance() 
# r square increased, which it means we are explaining more of our data

lm_growth %>%
  broom::tidy()

analysis_df %>%
  ggplot(aes(x = log(height), 
             y = log(stem_diameter),
             colour = growth_form)) + 
  geom_point(alpha = 0.1) + 
  geom_smooth(method = lm) + 
  labs(
    x = "Log of the height(m)",
    y = "Log of stem diameter (m)",
    colour = "Growth forms"
  ) +
  theme_linedraw()


