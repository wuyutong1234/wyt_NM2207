library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)

insomnia_uncleaned <- read.csv("insomnia.csv")
# Data cleaning
insomnia <- insomnia_uncleaned %>%
  select(
    Sex, Age, ISI_total, BDI_total, ASHS_total, ASHS_physiological, ASHS_cognitive,
    ASHS_emotional, ASHS_SleepEnvirnmont, ASHS_DaytimeSleep, ASHS_substances,
    ASHS_bedtimeRoutine, ASHS_sleepStability, ASHS_BedroomSharing, DBAS_total, FIRST_total,
    GCTI_total, ACE_tot, asq_home, asq_school, asq_attendance, asq_romantic, asq_peer,
    asq_teacher, asq_future, asq_leisure, asq_finance, asq_responsibility, cope_disengage_su,
    cope_growth, cope_disengage_mental, cope_emotions, cope_socialsupp_instr, cope_active,
    cope_denial, cope_religion, cope_humor, cope_disengage_emo, cope_restraint,
    cope_socialsupp_emo, cope_acccept, cope_suppression, cope_planning
  )

cleaned_insomnia <- na.omit(insomnia)

cleaned_insomnia$Sex <- factor(cleaned_insomnia$Sex, levels = c(0, 1), labels = c("Female", "Male"))

df <- cleaned_insomnia
# Find subset of columns that are suitable for scatter plot
df_num <- df |> select(where(is.numeric), -Year)

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "minty"),
  sidebar = sidebar(
    varSelectInput("xvar", "X variable", df_num, selected = "Bill Length (mm)"),
    varSelectInput("yvar", "Y variable", df_num, selected = "Bill Depth (mm)"),
    checkboxGroupInput(
      "species", "Filter by species",
      choices = unique(df$Species), 
      selected = unique(df$Species)
    ),
    hr(), # Add a horizontal rule
    checkboxInput("by_species", "Show species", TRUE),
    checkboxInput("show_margins", "Show marginal plots", TRUE),
    checkboxInput("smooth", "Add smoother"),
  ),
  plotOutput("scatter")
)

server <- function(input, output, session) {
  subsetted <- reactive({
    req(input$species)
    df |> filter(Species %in% input$species)
  })
  
  output$scatter <- renderPlot({
    p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + list(
      theme(legend.position = "bottom"),
      if (input$by_species) aes(color = Species),
      geom_point(),
      if (input$smooth) geom_smooth()
    )
    
    if (input$show_margins) {
      margin_type <- if (input$by_species) "density" else "histogram"
      p <- ggExtra::ggMarginal(p, type = margin_type, margins = "both",
                               size = 8, groupColour = input$by_species, groupFill = input$by_species)
    }
    
    p
  }, res = 100)
}

shinyApp(ui, server)