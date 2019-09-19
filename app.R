library(shiny)
library(dplyr)
library(globe4r)
library(echarts4r)

source("./data/preprocess.R")

ui <- fluidPage(
  theme = shinythemes::shinytheme("cyborg"),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css?family=Raleway+Dots&display=swap"
    )
  ),
  sliderInput(
    "year",
    "Year",
    step = 5,
    value = 1982,
    min = as.integer(years[1]),
    max = as.integer(years[length(years)]),
    width = "100%",
    animate = animationOptions(interval = 2500, loop = FALSE)
  ),
  uiOutput("selected"),
  globeOutput("globe", height = "70vh"),
  echarts4rOutput("trend", height = "180")
)

server <- function(input, output, session) {

  output$globe <- render_globe({
    dataset %>% 
      create_globe() %>% 
      globe_img_url(image_url("dark")) %>% 
      globe_background("#060606") %>% 
      globe_choropleth(
        coords(
          country = country,
          `1952`, `1957`, `1962`, `1967`, `1972`, `1977`,
          `1982`, `1987`, `1992`, `1997`, `2002`, `2007`,
          color_1952, color_1957, color_1962, color_1967, 
          color_1972, color_1977, color_1982, color_1987, 
          color_1992, color_1997, color_2002, color_2007,
          transition = 2000
        ),
        on_click = TRUE
      ) %>%  
      globe_pov(0, -70, 2L) %>% 
      polygons_altitude("1982") %>% 
      polygons_cap_color("color_1982") %>% 
      polygons_side_color(constant("rgba(212, 212, 212, .2)"))
  })

  observeEvent(input$year, {
    color <- paste0("color_", input$year) 

    globe_proxy("globe") %>% 
      polygons_altitude(as.character(input$year)) %>% 
      polygons_cap_color(color)
  })

  output$selected <- renderUI({
    req(input$globe_click_polygon)
    h1(
      input$globe_click_polygon$country,
      style = "position:absolute;top:10%;left:2%;z-index:999;font-family: 'Raleway Dots', cursive;"
    )
  })

  output$trend <- renderEcharts4r({
    input$globe_click_polygon
    req(input$globe_click_polygon)

    gapminder %>% 
      filter(country == input$globe_click_polygon$country) %>% 
      select(year, gdpPercap) %>% 
      arrange(year) %>% 
      e_charts(year) %>% 
      e_area(
        gdpPercap,
        smooth = TRUE,
        showSymbol = FALSE
      ) %>% 
      e_visual_map(
        gdpPercap, 
        show = FALSE,
        inRange = list(
          color = c("#2c7fb8", "#7fcdbb", "#edf8b1")
        )
      ) %>% 
      e_grid(
        show = FALSE,
        top = 5,
        left = 10,
        right = 10,
        bottom = 5
      ) %>% 
      e_x_axis(
        year, show = TRUE, 
        splitLine = FALSE,
        axisLine = FALSE,
        axisLabel = list(
          inside = TRUE,
          formatter = htmlwidgets::JS("function(x){return(x.toString())}"),
          textStyle = list(
            color = "#fff"
          )
        )
      ) %>% 
      e_y_axis(show = FALSE, axis_line = FALSE) %>% 
      e_legend(FALSE) 
  })

}

shinyApp(ui, server)
