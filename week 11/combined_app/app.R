library(tidyverse)
library(dplyr)
library(ggplot2)
library(bslib)  # Load the bslib package

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

library(shiny)

ui <- fluidPage(
  titlePanel("Demographic of Insomnia"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", "Select Plot", choices = c("Age", "Gender"))
    ),
    mainPanel(
      plotOutput("selectedPlot")
    )
  ),
  # Apply a basic Bootstrap theme to the entire app
  bs_theme(bootswatch = "flatly")
)

server <- function(input, output) {
  output$selectedPlot <- renderPlot({
    if (input$plotType == "Age") {
      # Age plot code
      ggplot(cleaned_insomnia, aes(x = Age, y = ISI_total, color = ISI_total)) +
        geom_line(size = 1, linetype = "solid") +
        geom_point(data = subset(cleaned_insomnia, ISI_total == max(ISI_total)), size = 3) +
        labs(x = "Age", y = "Insomnia Severity", title = "Insomnia Severity by Age") +
        theme_minimal() +
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 16, face = "bold"),
          legend.position = "none"
        )
    } else if (input$plotType == "Gender") {
      # Gender plot code
      insomniacutoff <- 14
      filtered_data <- cleaned_insomnia %>% filter(ISI_total >= insomniacutoff)
      ggplot(filtered_data, aes(x = "", fill = Sex)) +
        geom_bar(width = 1) +
        coord_polar(theta = "y") +
        labs(title = "Gender Distribution for Insomnia") +
        scale_fill_manual(values = c("Male" = "#0077B6", "Female" = "#FF6F61"))
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)
