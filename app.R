library(dplyr)
library(shiny)
library(globe4r)
library(echarts4r)

#globe4r::shared_background()
source("./data/preprocess.R")

func <- htmlwidgets::JS(
  "function(params){
    return(params.name)
  }"
)

ui <- fluidPage(
  windowTitle = "GDP - APP",
  theme = shinythemes::shinytheme("cyborg"),
  fluidPage(
    column(11, echarts4rOutput("countries", height = 80)),
    column(1, selectInput("indicator", "", choices = c("GDP/CAP" = "gdpPercap", "POP" = "pop")))
  ),
  globeOutput("globe", height = "90vh")
)

server <- function(input, output, session) {

  output$globe <- renderGlobe({
    gapminder %>% 
      select(
        country,
        gdpPercap = input$indicator
      ) %>% 
      create_globe() %>% 
      globe_choropleth(
        coords(
          country = country,
          cap_color = gdpPercap,
          side_color = gdpPercap,
          altitude = gdpPercap
        )
      ) %>% 
      scale_choropleth_cap_color() %>% 
      scale_choropleth_side_color() %>% 
      scale_choropleth_altitude(min = .1, max = .6) %>% 
      globe_rotate(.25)
  })

  output$countries <- renderEcharts4r({
    gapminder %>% 
      select(
        country,
        gdpPercap = input$indicator
      ) %>% 
      mutate(
        gdpPercap = gdpPercap / 1000,
        label = paste0(country, "<br/>GDP/capita: ", prettyNum(round(gdpPercap, 2), big.mark = ","), "K")
      ) %>% 
      arrange(-gdpPercap) %>% 
      e_charts(country) %>% 
      e_bar(gdpPercap, bind = label, color = "#2c7fb8") %>% 
      e_tooltip(formatter = func, confine = TRUE) %>% 
      e_y_axis(gdpPercap, show = FALSE) %>% 
      e_x_axis(show = FALSE) %>% 
      e_legend(FALSE)
  })
}

shinyApp(ui, server)