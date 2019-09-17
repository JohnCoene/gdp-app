data(gapminder, package = "gapminder")

gapminder <- gapminder %>% 
  filter(year == max(year)) %>% 
  mutate(country = as.character(country))