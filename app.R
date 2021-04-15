
#The libraries needed for the project
library(shiny) #for the shiny app
library(leaflet) #for  maps visualization
library(plotly) #for interactive graphs
library(colourpicker) #for picking a color if user's wish
library(DT) #for interactive tables
library(tidyr)

#upload the data frame
places <- read.csv2("https://raw.githubusercontent.com/VladislavsLuksha/US-Accomodation-in-Boston-Hawaii-Chicago/main/AB_US_2021.csv")

###############################################################################
###############################################################################
#introduce user interface part of the app
ui <- fluidPage(
  h1("US Housing: Boston, Hawaii, Chicago"),
  h2("This is my R Project"),
  h3("Uladzislau Luksha, Marticulation number: 964000"),
  #creating a container for tab panels
  tabsetPanel(
  #Creating an 'Inputs and Map' tab
    tabPanel(
      title = "Inputs and Map",
      sidebarLayout(
        sidebarPanel(
          #Adding Radio Buttons for a city selection
          radioButtons("city","Select a city",choices =
                         c("Boston","Hawaii","Chicago")),
          #Addind a Slider for a price selection
          sliderInput("price","Select your price per night", min(places$price),
                      max = 1450,
                      value = c(100, 150)),
          ),
        mainPanel(
          #Creating a space for the map in UI
          leafletOutput("mymap"),
                         )
        )),
    #Creating a 'Plot' tab
    tabPanel(
      title = "Plot",
      #creating color input for the user's choice
      colourInput("color", "Select point color", value = "blue"),
      #creating a checkbox for the possibility of adding the line of best fit
      checkboxInput("fit", "Add line of best fit", FALSE),
      #Creating a space for the plot in UI
      plotlyOutput("plot"),
      ),
    #Creating a "Table" tab
    tabPanel(
      title = "Table",
      #Creating an interface for download button
      downloadButton(outputId = "download_data", label = "Download"),
      #Creating a space for the table in UI
      DT::dataTableOutput("table")

    )))
###############################################################################
###############################################################################

# Defining the logic of the server
server <- function(input, output) {
  # Creating a reactive function in order to simplify the code
  # and unify the filtering data conditions
  filtered_data <- reactive({
    data <- places
    data <- subset(
      data,
      #the fileter is dependent on the input price and the city
      price >= input$price[1] & price <= input$price[2] & city == input$city)
    data})

#rendering a plot
  output$plot <- renderPlotly({
#I decided to use plotly library in order to make the plot more interactive
    ggplotly({
      data <- subset(places,
                     city %in% input$city &
                       price >= input$price[1] & price <= input$price[2])
#the graph is a scattered plot showing the connection between number of
#the reviews and the price per night, there is a possibility to add a linear fit
      p <- ggplot(data, aes(number_of_reviews, price)) +
        geom_point(size = 2, col = input$color) +
        scale_x_log10() +
        ggtitle(input$title)
#addind a linear trend if the user's input for the checkbox is TRUE
      if (input$fit) {
        p <- p + geom_smooth(method = "lm") #smoothing for linear models
      }
      p
    })
  })

#defining a palette for the map pins
  pal <- colorFactor(palette = c("darkolivegreen1", "darkolivegreen2","darkolivegreen3","darkolivegreen4"),
                     levels = c("Private room","Entire home/apt","Shared room","Hotel room"))
  output$mymap <- renderLeaflet({
    #creating a data for the map, using the filtered data reactive function
    places_data <- filtered_data()
    places_data %>%
    leaflet()  %>%
      addProviderTiles("CartoDB")  %>%
      addCircleMarkers(
        data = places_data,
        ~ longitude,
        ~ latitude,
        popup = places$price,
        color = pal(places$room_type),
        radius = 1) %>%
      #adding a legend to the map
      addLegend(pal = pal,
                values = c("Private room","Entire home/apt","Shared room","Hotel room"),
                opacity = 1,
                title = "room_type",
                position = "bottomright")
  })
  #rendering an interactive data table
  output$table <- DT::renderDataTable({
    # The table output is also interactive and uses reactive function as data
    data <- filtered_data()
    data
  })

  # Creating a download handler
  output$download_data <- downloadHandler(
    # The downloaded file is named "uladzislau_luksha_data.csv"
    filename = "uladzislau_luksha_data.csv",
    content = function(file) {
      # The code for filtering the data is copied from the
      data <- filtered_data()
      data
      write.csv(data, file, row.names = FALSE)
    })

}
###############################################################################
###############################################################################
#Running the application
shinyApp(ui = ui, server = server)
