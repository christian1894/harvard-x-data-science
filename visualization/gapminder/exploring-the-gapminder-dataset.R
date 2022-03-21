library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)

# Scatter plot of life expectancy 
# versus fertility for the African continent in 2012 by region.

gapminder %>% filter(year == 2012 & continent == "Africa") %>%
  ggplot(aes(x = fertility, y = life_expectancy, color = region)) +
  geom_point()

# African countries that in 2012 had fertility rates of 
# 3 or less and life expectancies of at least 70.

df = gapminder %>% 
  filter(year == 2012 & 
           continent == "Africa" & 
           life_expectancy >= 70 & 
           fertility <= 3) %>%
  select(country, region) %>%
  arrange(region)

# Vietnam war: Life expectancy from 1960 
# to 2010 in USA and Vietnam

vietnam_war_years = seq(1960, 2010)
vietnam_war_countries = c("Vietnam", "United States")
tab = gapminder %>%
  filter(year %in% vietnam_war_years &
           country %in% vietnam_war_countries)

p = tab %>%
  ggplot(aes(x = year, y = life_expectancy, color = country)) +
  geom_line()
p

# Communist Khmer Rouge ruling Cambodia: Life expectancy 
# from 1975 to 1979 in Cambodia

tab = gapminder %>%
  filter(year %in% seq(1960, 2010) &
           country == "Cambodia")

p = tab %>%
  ggplot(aes(x = year, y = life_expectancy)) +
  geom_line()
p

# plot dollars per day for African countries in 2010
# using GDP data

daydollars = gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & 
           year == 2010 & 
           !is.na(dollars_per_day))

# smooth density plot using a log (base 2) x axis

daydollars %>% 
  ggplot(aes(dollars_per_day)) +
  geom_density() +
  scale_x_continuous(trans = "log2")

# density plots for 1970 and 2010

years = c(1970, 2010)

daydollars = gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & 
           year %in% years & 
           !is.na(dollars_per_day))

daydollars %>% 
  ggplot(aes(dollars_per_day, fill = region)) +
  geom_density(bw = 0.5, position = "stack") +
  scale_x_continuous(trans = "log2") +
  facet_grid(rows = years)

# infant mortality rates versus dollars per day for
# African countries.

gapminder_Africa_2010 = gapminder %>%
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & 
           year == 2010 & 
           !is.na(dollars_per_day))

gapminder_Africa_2010 %>%
  ggplot(aes(x = dollars_per_day,
             y = infant_mortality,
             color = region,
             label = country)) +
  scale_x_continuous(trans = "log2") +
  geom_text() +
  geom_point()

# Infant mortality and dollars per day patterns 
# African countries between 1970 and 2010.
years = c(1970, 2010)
gapminder_Africa_1970_and_2010 = gapminder %>%
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(year %in% years &
           continent == "Africa" & 
           !is.na(dollars_per_day) &
           !is.na(infant_mortality))

plot = gapminder_Africa_1970_and_2010 %>%
  ggplot(aes(x = dollars_per_day,
             y = infant_mortality,
             color = region,
             label = country)) +
  scale_x_continuous(trans = "log2") +
  geom_text() +
  geom_point()

plot + facet_grid(year~.)
