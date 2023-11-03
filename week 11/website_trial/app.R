library(shiny)
library(ggplot2)
library(DT)

ui <- fluidPage(
  titlePanel("About Me"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    mainPanel(
      plotOutput("distPlot"),
      dataTableOutput("dataTable")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    # generate bins using input$bins from the slider
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$dataTable <- renderDataTable({
    # create a table with 10 rows
    DT::datatable(data.frame(A = rnorm(10), B = rnorm(10), C = rnorm(10)))
  })
}

shinyApp(ui = ui, server = server)
