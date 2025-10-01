




library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
# library(ABACUS)

ui <- page_fluid(
  theme = bs_theme(version = 5),
  h2("Selector de comida"),
  selectInput("opcion", "¿Qué tipo?",
              choices = c("Pedir"="pedir","Hacer en casa"="hacer"),
              selected = "pedir"),
  actionButton("nuevo", "Nuevo sorteo"),
  plotOutput("graf", height = 320)
)

server <- function(input, output, session){
  
  # (Opcional) inicio aleatorio:
  # observe({ updateSelectInput(session, "opcion", selected = sample(c("pedir","hacer"), 1)) })
  
  simula <- function(op){
    opciones <- if (op == "hacer")
      c("Carne","Pollo","Pescado")
    else
      c("Pizza","Hamburguesa","Sushi","China","Mexicana","Alitas")
    
    tibble(platillo = sample(opciones, 1000, TRUE)) |>
      count(platillo, name = "freq") |>
      mutate(es_max = freq == max(freq))
  }
  
  # Re-muestrea al cambiar opción o al hacer clic en el botón
  datos <- reactive({
    req(input$opcion %in% c("pedir","hacer"))
    input$nuevo
    simula(input$opcion)
  })
  
  output$graf <- renderPlot({
    datos() |>
    ggplot(aes(reorder(platillo, -freq), freq, fill = es_max)) +
      geom_col() +
      guides(fill = "none") +
      labs(x = NULL, y = "Frecuencia") +
      scale_fill_manual(values=c("grey90","lightblue2")) +
      theme_minimal(base_size = 14)
    
    # barplot(datos()$es_max,datos()$platillo)
    
  })
}

shinyApp(ui, server)


