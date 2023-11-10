library(shiny)
library(ggplot2)
library(bslib)
library(tidyverse)
library(dplyr)

insomnia_uncleaned <- read.csv("insomnia.csv")

# Data cleaning
insomnia <- insomnia_uncleaned %>%
  select(Sex, Age, ISI_total, BDI_total, ASHS_total, ASHS_physiological, ASHS_cognitive,ASHS_emotional, ASHS_SleepEnvirnmont, ASHS_DaytimeSleep, ASHS_substances, ASHS_bedtimeRoutine, ASHS_sleepStability, ASHS_BedroomSharing, DBAS_total, FIRST_total, GCTI_total, ACE_tot, asq_home, asq_school, asq_attendance, asq_romantic, asq_peer, asq_teacher, asq_future, asq_leisure, asq_finance, asq_responsibility, cope_disengage_su, cope_growth, cope_disengage_mental, cope_emotions, cope_socialsupp_instr, cope_active, cope_denial, cope_religion, cope_humor, cope_disengage_emo, cope_restraint, cope_socialsupp_emo, cope_acccept, cope_suppression, cope_planning)

# Assuming you have a data frame named 'my_data' and you want to rename the variable 'old_name' to 'new_name'
rename_insomnia <- insomnia %>%
  rename(Insomnia_severity = ISI_total) %>%
  rename(Depression = BDI_total) %>%
  rename(Sleep_hygiene = ASHS_total) %>%
  rename(Predisposition_to_insomnia = FIRST_total) %>%
  rename(Dysfunctional_beliefs = DBAS_total) %>%
  rename(Cognitive_intrusiveness = GCTI_total)

cleaned_insomnia <- na.omit(rename_insomnia)

cleaned_insomnia$Sex <- factor(cleaned_insomnia$Sex, levels = c(0, 1), labels = c("Female", "Male"))


# Function to create a plot with improved aesthetics
# Function to create a plot with improved aesthetics
create_plot <- function(data, x_var, y_var, title, custom_labels = NULL) {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(color = "#0077B6", size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "#FF6F61", linetype = "dashed", size = 1.2) +
    labs(
      x = ifelse(!is.null(custom_labels) && length(custom_labels) == ncol(data[x_var]), custom_labels, gsub("_", " ", tolower(x_var))),  # Custom or default x-axis label
      y = "insomnia severity",
      title = tools::toTitleCase(gsub("_", " ", title))  # Improve title case
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
      plot.title = element_text(hjust = 0.5, size = 16),  # Center and increase title size
      axis.title = element_text(size = 14),  # Increase axis label size
      legend.position = "none"  # Remove legend if not needed
    )
}
custom_labels <- c("Depression", "Sleep hygiene", "Predisposition to insomnia", "Dysfunctional beliefs", "Cognitive intrusiveness")
p <- create_plot(cleaned_insomnia, "Depression", "Insomnia_severity", "Insomnia severity against Depression", custom_labels)



# Define UI
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel("Choose a causal factor to see how it correlates with insomnia"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("plotSelection", "Select Plot:",
                   choices = c("Depression", "Sleep hygiene", "Predisposition to insomnia", "Dysfunctional beliefs", "Cognitive intrusiveness"),
                   selected = "Depression")
    ),
    mainPanel(
      plotOutput("selectedPlot")
    )
  )
)

# Define server
server <- function(input, output) {
  output$selectedPlot <- renderPlot({
    selected_plot <- input$plotSelection
    
    if (selected_plot == "Depression") {
      p <- create_plot(cleaned_insomnia, "Depression", "Insomnia_severity", "Insomnia severity against Depression")
    } else if (selected_plot == "Sleep hygiene") {
      p <- create_plot(cleaned_insomnia, "Sleep_hygiene", "Insomnia_severity", "Insomnia severity against Sleep hygiene")
    } else if (selected_plot == "Predisposition to insomnia") {
      p <- create_plot(cleaned_insomnia, "Predisposition_to_insomnia", "Insomnia_severity", "Insomnia severity against Predisposition to insomnia")
    } else if (selected_plot == "Dysfunctional beliefs") {
      p <- create_plot(cleaned_insomnia, "Dysfunctional_beliefs", "Insomnia_severity", "Insomnia severity against Dysfunctional beliefs")
    } else if (selected_plot == "Cognitive intrusiveness") {
      p <- create_plot(cleaned_insomnia, "Cognitive_intrusiveness", "Insomnia_severity", "Insomnia severity against Cognitive intrusiveness")
    }
    
    print(p)
  })
}

# Run the app
shinyApp(ui, server)
