data(gapminder, package = "gapminder")

dataset <- gapminder %>% 
  select(-continent, -lifeExp, -pop) %>% 
  mutate(country = as.character(country)) %>% 
  tidyr::pivot_wider(
    names_from = year,
    values_from = gdpPercap
  ) %>% 
  mutate_if(is.numeric, scales::rescale, to = c(.06, .1)) %>% 
  mutate(
    country = case_when(
      country == "United States" ~ "United States of America",
      TRUE ~ country
    )
  )

years <- names(dataset)[2:length(names(dataset))]

add_color <- function(dataset){
  scl <- scales::col_numeric(c("#2c7fb8", "#7fcdbb", "#edf8b1"), c(.06, .1))
  nms <- names(dataset)
  nms <- nms[2:length(nms)]
  nms <- paste0("color_", nms)

  colors <- dataset %>% 
    mutate_if(is.numeric, scl) %>% 
    purrr::set_names(c("country", nms))

  left_join(dataset, colors, by = "country")
}

dataset <- add_color(dataset)

all_vars <- names(dataset)