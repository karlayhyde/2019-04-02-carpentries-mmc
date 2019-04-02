library(readr)
gapminder <-  readr::read_csv(here("data/gapminder/raw/gapminder_data.csv"))
mean(gapminder$gdpPercap [gapminder$continent== "Africa"])
mean(gapminder$gdpPercap [gapminder$continent== "Americas"])
max(gapminder$lifeExp)
min(gapminder$lifeExp)
gapminder

library(tidyverse)

year_country_gdp <- select(gapminder, year, country, gdpPercap)
# Pipe Function to do a string of edits on your file



LifeExp_btcountry  <- gapminder %>% 
  group_by(country) %>%
  summarize(
            mean_val = mean(lifeExp),
            sd_gdpPercap = sd(gdpPercap),
            min_lifeExp=min(lifeExp),
            max_lifeExp=max(lifeExp),
            ) 

  filter(continent =="Africa") %>%
  select(lifeExp, country, year) 
   
head(LifeExp_btcountry)


ggplot code:
  ggplot(data = gapminder, aes(x = year, y = lifeExp, color = continent)) +
  geom_line() + facet_wrap( ~ country)


   ggplot(data = gapminder, aes(x = year, y = lifeExp, color = continent)) +
                             geom_line() + facet_wrap( ~ country)


