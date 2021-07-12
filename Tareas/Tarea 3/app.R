library(shiny)
library(tidyverse)
library(DT)

propinas <- read_csv("propina.csv")

ui <- fluidPage(
        
        titlePanel("Datos de Propina"),
        
        sidebarLayout(
          
          sidebarPanel(
            
            selectInput(
              "varcolor",
              "Variable en color",
              c("sexo", "fuma", "dia", "momento")
            ),
            
            selectInput(
              "digitos", "Cantidad de decimales",
              c(0, 1, 2)),
            
            actionButton("runif", "Mostrar"),
            
            actionButton("reset", "Ocultar")
            
          ),
          
          mainPanel(
            
            tabsetPanel(
              
              tabPanel(
                "Bivariado",
                h2("Diagrama de dispersión", align = "center"),
                plotOutput("scat"),
                dataTableOutput("tabla")
              ),
              
              tabPanel(
                "Univariado",
                h2("Gráfico de Barras", align = "center"),
                plotOutput("bar")
              )
            )
          )
        )
      )
              

server <- function(input, output) {
  
  react <- reactiveValues(data = NULL)
  
  observeEvent(input$runif, {
    
    react$scat <- ggplot(data = propinas,
                         aes(x = total,
                             y = propina,
                             colour = .data[[input$varcolor]])) +
                  geom_point() +
                  theme(aspect.ratio = 1) +
                  scale_x_continuous(name ="Total de la cuenta") +
                  scale_y_continuous(name = "Propina")
    
    
    react$tabla <- reactive({
                  
                   propinas %>%
                    group_by(.data[[input$varcolor]]) %>%
                    summarise(
                      mean_prop = mean(propina),
                      sd_propina = sd(propina),
                      mean_total = mean(total),
                      sd_total = sd(total)
                    ) %>% 
                    mutate(
                      across(
                        where(is.double),
                        function (x) {round(x,
                                            digits = 2 # .data[[input$digitos]]
                        )}
                      )) %>% datatable()
                })
    
    react$bar <- ggplot(data = propinas, 
                        aes(x = .data[[input$varcolor]])) +
                 geom_bar() +
                 labs(y = "Cantidad")
    })
    
    
    observeEvent(input$reset, {
      react$scat <- NULL
      react$tabla <- NULL
      react$bar <- NULL
    })    
  
    output$scat <- renderPlot({
      
      if (is.null(react$scat)) 
        return()
      plot(react$scat)
    
    })  
    
    
    output$tabla <- renderDT({
      
      if (is.null(react$tabla)) 
        return()
      print(react$tabla())
      
    })
    
    
    output$bar <- renderPlot({
      
      if (is.null(react$bar)) 
        return()
      plot(react$bar)
      
    })
}


shinyApp(ui = ui, server = server)

# Puntaje 9/10
# Esta descoordinada la tabla y el gráfico.
# No es necesario poner los gráficos y objetos reactivo en el observeEvent