library(shiny)
library(tidyverse)


ui <- fluidPage(sliderInput(inputId = "cant",
                            label = "tamano muestral:",
                            min = 1, max = 500,
                            value = 30),
                selectInput("distri","Distribucion", c("Gamma","Normal")),
                plotOutput("hist")
                )

server <- function(input, output) {
  output$hist <- renderPlot({
    if (input$distri == "Normal") {
      dist <- data.frame(x = rnorm(input$cant))
      gg <- dist %>%
        ggplot(aes(x)) + geom_histogram(binwidth = 0.25)
      print(gg)
    } else{
      dist <- data.frame(x = rgamma(n = input$cant, shape = 1))
      gg <- dist %>%
        ggplot(aes(x)) + geom_histogram(binwidth = 0.25)
      print(gg)
    }
  })
}

shinyApp(ui, server)
