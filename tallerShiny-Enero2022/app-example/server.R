library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
load(file = "input/datos/movies.rdata")
# Define las funciones de server
server <- function(input, output) {
    # Crea un scatter plot
    output$scatterplot <- renderPlot({
        ggplot(data = movies_subset(), # usamos el data reactivo!
               aes_string(x = input$x, y = input$y)) +  
            geom_point() +
            geom_smooth(method = "lm")
    })
    # crea una tabla resumen
    movies_subset <- reactive({
        # viene del UI
        req(input$selected_type)
        # filtra base de datos
        filter(movies, mpaa %in% input$selected_type)
    }) #termina data reactivo
    output$n <- renderUI({
        HTML(paste0("El gráfico muestra la relación entre la audiencia y la crítica de ",
                    nrow(movies_subset()), 
                    " películas ", 
                    "<b>", input$selected_type, "</b>"))
    }) #termina output de N de muestra
}   # Termina server