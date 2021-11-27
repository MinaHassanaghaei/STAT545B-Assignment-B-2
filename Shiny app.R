
library(shiny)
library(tidyverse)
data <- read.csv("bcl-data.csv")


ui <- fluidPage(
  titlePanel("BC Liquor Application"),
  h3("Let's choose a drink!"),
  fluidRow(column(10,
                  sidebarLayout(
                    sidebarPanel(
                      wellPanel(selectInput(inputId = "id_var",
                                  label = h5(em(strong("What kind of drink are you looking for?"))),
                                  choices = c("BEER", "REFRESHMENT",
                                              "SPIRITS", "WINE"),
                                  selected = "REFRESHMENT")),
                      wellPanel(sliderInput(inputId = "id_slider",
                                  label=h5(em(strong("Select your price range:"))),
                                  min = 0, max = 100,
                                  value = c(5,25),
                                  pre="$")),
                    ),
                    mainPanel(img(
                      src="Banner.jpeg",
                      height = 300, width = 450), plotOutput("id_histogram"),
                              tableOutput("id_table"))
                  )),
           )
  )



server <- function(input, output){
  observe(print(input$id_slider))
  observe(print(input$id_var))


  data_filtered <- reactive({
    switch (input$id_var,
            "BEER" = data[data$Type=="BEER",],
            "REFRESHMENT" = data[data$Type=="REFRESHMENT",],
            "SPIRITS" = data[data$Type=="SPIRITS",],
            "WINE" = data[data$Type=="WINE",]
    ) %>%
      filter(Price < input$id_slider[2],
             Price > input$id_slider[1])})
  output$id_histogram <- renderPlot({
    data_filtered() %>%
      ggplot(aes(Alcohol_Content))+
      geom_histogram()})
  output$id_table <- renderTable({
    data_filtered()
  })
}

library(shiny)

shinyApp(ui=ui, server=server)


