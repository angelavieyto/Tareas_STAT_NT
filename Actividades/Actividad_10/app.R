library(shiny)
library(tidyverse)


ui <- fluidPage(
          sidebarLayout(
            sidebarPanel(
              
              sliderInput(inputId = "cant",
                          label = "Tamaño muestral:",
                          min = 1, max = 500,
                          value = 30),
              
              selectInput(inputId = "distri",
                          label = "Distribucion:",
                          choices = c("Normal", "Gamma", "Ambas")),
              
              textInput(inputId = "cant_bins",
                        label = "Cantidad de bins:",
                        value = 20),
              
              img(src = "meme.jpg", height = 230, width = 230)
            ),
            
            mainPanel(
              tabsetPanel(
                tabPanel("Histograma", plotOutput("hist")),
                tabPanel("Plot",
                         fluidRow(
                           column(6, plotOutput("plot1")),
                           column(6, plotOutput("plot2"))
                         ),
                         fluidRow(
                           column(6, plotOutput("plot3")),
                           column(6, plotOutput("plot4"))
                         )),
                tabPanel("Resumen"),
                tabPanel("Tabla")
              )
            )
          )
      )


server <- function(input, output){
            
            datos <- reactive(
              
                      if (input$distri == "Normal") {
                        rnorm(input$cant)
                        
                      } else if (input$distri == "Gamma") {
                        rgamma(input$cant, shape = 1)
                        
                      } else {
                        normal <- rnorm(input$cant)
                        gamma <- rgamma(input$cant, shape = 1)
                        df_ambas <- data.frame(
                          valores = c(normal, gamma),
                          tipo = c(rep("Normal", times = input$cant),
                                   rep("Gamma", times = input$cant))
                        )
                      }
                    )
            
            grafico <- reactive(
              
                        if(input$distri == "Normal" | input$distri == "Gamma") {
                          gg <- data.frame(x = datos()) %>% 
                                  ggplot(aes(x)) +
                                  geom_histogram(bins = input$cant_bins)
                          print(gg)
                          
                        } else {
                          gg <- datos() %>% 
                                  ggplot(aes(valores)) +
                                  geom_histogram(bins = input$cant_bins) +
                                  facet_wrap(vars(tipo))
                          print(gg)
                        }
            )
            
            output$hist <- renderPlot({grafico()}, height = 700, res = 96)
            output$plot1 <- renderPlot({plot(grafico())}, height = 370, res = 96)
            output$plot2 <- renderPlot({plot(grafico())}, height = 370, res = 96)
            output$plot3 <- renderPlot({plot(grafico())}, height = 370, res = 96)
            output$plot4 <- renderPlot({plot(grafico())}, height = 370, res = 96)
  
} 

shinyApp(ui = ui, server = server) 

# Puntaje 10/10

