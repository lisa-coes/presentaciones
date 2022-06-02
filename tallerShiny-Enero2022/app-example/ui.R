library(shiny)
library(shinydashboard)
load(file = "input/datos/movies.rdata")
ui <- dashboardPage(
    dashboardHeader(
        title = "Mi dashboard",
        titleWidth = 200,
        disable = F
    ),
    # Sidebar  con las definiciones de input y output
    dashboardSidebar(
        # Inputs: selecciona variables de un plot
        # Selecciona variable para el eje Y
        selectInput(
            inputId = "y",
            label = "Y-axis:",
            choices = c("rating", "votes", "budget", "length", "year"),
            selected = "rating"
        ),
        # Selecciona variable para el eje X
        selectInput(
            inputId = "x",
            label = "X-axis:",
            choices = c("rating", "votes", "budget", "length", "year"),
            selected = "budget"
        ),
        selectInput(
            inputId = "selected_type",
            label = "Selecciona clasificación:",
            choices = levels(movies$mpaa),
            selected = "NC-17"
        )
    ),
    # Output: Show scatterplot
    dashboardBody(
        fluidPage(
            box(
                # mostrar scatterplot
                plotOutput(outputId = "scatterplot"),
                # mostrar el N de peliculas
                uiOutput(outputId = "n")
            )
        ))
)