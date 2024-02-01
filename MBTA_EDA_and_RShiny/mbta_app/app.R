##Final Project for MA615. Made by Jiun Lee.

library(shiny)
library(googleway)
library(randomcoloR)
library(dplyr)
library(mapsapi)

ui <- fluidPage(titlePanel("Finding Directions"), 
                sidebarLayout(
                  sidebarPanel(
                    textInput(inputId = "depart", label = "Type your departing location"),
                    textInput(inputId = "arrive", label = "Type your destination"),
                    helpText("Note:This app will show the multiple paths by bus and subway.",
                             "When you click the each path, it will show you the time it takes.",
                             "ex) Try typing 'Boston University' and 'Boston College'. "),
                    actionButton(inputId = "route", label = "Find Route",
                                 style = "background-color:#A1B3A9;
                                                       color:#F5FAF8; 
                                             border-color:#BEBEBE;
                                             border-style:double;
                                             border-width:4px;
                                             border-radius:7%;
                                             font-size:15px;")),
                  
                  mainPanel("Google Map",
                            google_mapOutput(outputId = "gmap"))
                )
)


server <- function(input, output, session) {
  
  api_key <- ""
  
  output$gmap <- renderGoogle_map({
    google_map(key = api_key,
               location = c(42.361145, -71.057083),
               zoom = 10,
               search_box = TRUE,
               scale_control = TRUE,
               height = 1000)%>%
      add_traffic()
  })
  
  observeEvent(input$route,{
    
    r <- mp_directions(origin = input$depart,
                       destination = input$arrive,
                       alternatives = TRUE,
                       key = api_key,
                       quiet = TRUE,
                       transit_mode = c("bus", "subway"))
    rd<- mp_get_routes(r)
    i <- nrow(rd)
    rd$color <- c(randomColor(count = i, luminosity = "bright"))
    
    google_map_update(map_id = "gmap") %>%
      clear_traffic() %>%
      clear_polylines() %>%
      clear_markers() %>%
      add_traffic() %>%
      add_polylines(data = rd,
                    polyline = "geometry",
                    stroke_colour = "color",
                    stroke_weight = 6,
                    stroke_opacity = 0.8,
                    info_window = "duration_text",                    
                    load_interval = 100)
    
  })
  
  
}

shinyApp(ui, server)



