library(shiny)

ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      img(src = "codealong8.png", height = 140, width = 400),
      p("p creates a paragraph of text."),
      p("A new p() command starts a new paragraph. Supply a style attribute to change the format o"),
      strong("strong() makes bold text."),
      em("em() creates italicized (i.e, emphasized) text."),
      br(),
      code("code displays your text similar to computer code"),
      div("div creates segments of text with a similar style. This division of text is all blue be"),
        br(),
        p("span does the same thing as div, but it works with",
          span("groups of words", style = "color:blue"),
          "that appear inside a paragraph.")
      )
    )
  )
shinyApp(ui = ui, server = server)


