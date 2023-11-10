library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(bslib)

insomnia_uncleaned <- read.csv("insomnia.csv")

# Data cleaning
insomnia <- insomnia_uncleaned %>%
  select(
    Sex, Age, ISI_total, BDI_total, ASHS_total, ASHS_physiological,
    ASHS_cognitive, ASHS_emotional, ASHS_SleepEnvirnmont, ASHS_DaytimeSleep,
    ASHS_substances, ASHS_bedtimeRoutine, ASHS_sleepStability,
    ASHS_BedroomSharing, DBAS_total, FIRST_total, GCTI_total, ACE_tot,
    asq_home, asq_school, asq_attendance, asq_romantic, asq_peer,
    asq_teacher, asq_future, asq_leisure, asq_finance, asq_responsibility,
    cope_disengage_su, cope_growth, cope_disengage_mental, cope_emotions,
    cope_socialsupp_instr, cope_active, cope_denial, cope_religion,
    cope_humor, cope_disengage_emo, cope_restraint, cope_socialsupp_emo,
    cope_acccept, cope_suppression, cope_planning
  )

cleaned_insomnia <- na.omit(insomnia)

cleaned_insomnia$Sex <- factor(cleaned_insomnia$Sex, levels = c(0, 1), labels = c("Female", "Male"))

#customise the theme
ui <- fluidPage(
  theme = bs_theme(bg = "#fff",
                   fg = "#245465",
                   primary = "#E69F00",
                   secondary = "#0072B2",
                   success = "#009E73",
                   base_font = font_google("Inter"),
                   code_font = font_google("JetBrains Mono")),
  titlePanel("Demographics of Insomnia"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", "Select Plot", choices = c("Age", "Gender"))
    ),
    mainPanel(
      plotOutput("selectedPlot")
    )
  )
)

server <- function(input, output) {
  output$selectedPlot <- renderPlot({
    if (input$plotType == "Age") {
      # Age plot code
      ggplot(cleaned_insomnia, aes(x = Age, y = ISI_total, color = ISI_total)) +
        geom_line(size = 1, linetype = "solid", color = "#43819d") +
        geom_point(data = subset(cleaned_insomnia, ISI_total == max(ISI_total)), color = "#daa671", size = 5) +  
        labs(x = "Age", y = "Insomnia Severity", title = "Insomnia Severity by Age") +
        theme_minimal() +
        theme(
          axis.text = element_text(color = "#245465", size = 12),
          axis.title = element_text(color = "#245465", size = 14),
          plot.title = element_text(color = "#245465", size = 20, hjust = 0.5),  # Center the title
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
        scale_fill_manual(values = c("Male" = "#43819d", "Female" = "#e4ccca"), name = "Sex") +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "#245465", size = 20, hjust = 0.5),  # Center the title and increase title size
          plot.margin = margin(5, 5, 5, 5, "mm")  # Adjust plot margin to make the pie chart bigger
        )
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)
