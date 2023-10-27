library(tidyverse)

insomnia_uncleaned <-read.csv("insomnia.csv")
insomnia <- insomnia_uncleaned %>% # <-- pipe operator
  select(Group, SubGroup, Sex, Age, ISI_total, PSQI_total, BDI_total, ASHS_total, ASHS_physiological, ASHS_cognitive,ASHS_emotional, ASHS_SleepEnvirnmont, ASHS_DaytimeSleep, ASHS_substances, ASHS_bedtimeRoutine, ASHS_sleepStability, ASHS_BedroomSharing, DBAS_total, FIRST_total, GCTI_total, NEO_neuroticism, NEO_extraversion, NEO_openness, NEO_agreeableness, NEO_Conscientiousness, ACE_tot, asq_home, asq_school, asq_attendance, asq_romantic, asq_peer, asq_teacher, asq_future, asq_leisure, asq_finance, asq_responsibility, cope_disengage_su, cope_growth, cope_disengage_mental, cope_emotions, cope_socialsupp_instr, cope_active, cope_denial, cope_religion, cope_humor, cope_disengage_emo, cope_restraint, cope_socialsupp_emo, cope_acccept, cope_suppression, cope_planning)
# View the columns stacked one below another

cleaned_insomnia <- na.omit(insomnia)
cleaned_insomnia$Sex <- factor(cleaned_insomnia$Sex, levels = c(0, 1), labels = c("Female", "Male"))

participants <- cleaned_insomnia %>%
  select(Group, SubGroup, Sex, Age)


sleep_quality <- cleaned_insomnia %>%
  select(ISI_total, PSQI_total)


causes <- cleaned_insomnia %>%
  select(BDI_total, ASHS_total, ASHS_physiological, ASHS_cognitive,ASHS_emotional, ASHS_SleepEnvirnmont, ASHS_DaytimeSleep, ASHS_substances, ASHS_bedtimeRoutine, ASHS_sleepStability, ASHS_BedroomSharing, DBAS_total, FIRST_total, GCTI_total, NEO_neuroticism, NEO_extraversion, NEO_openness, NEO_agreeableness, NEO_Conscientiousness, ACE_tot, asq_home, asq_school, asq_attendance, asq_romantic, asq_peer, asq_teacher, asq_future, asq_leisure, asq_finance, asq_responsibility)


coping <- cleaned_insomnia %>%
  select(cope_disengage_su, cope_growth, cope_disengage_mental, cope_emotions, cope_socialsupp_instr, cope_active, cope_denial, cope_religion, cope_humor, cope_disengage_emo, cope_restraint, cope_socialsupp_emo, cope_acccept, cope_suppression, cope_planning)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Insomnia Severity by Age"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 0,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({
    ggplot(cleaned_insomnia, aes(x = Age, y = ISI_total)) +
      geom_point() +
      labs(x = "Age", y = "Insomnia Severity") +
      ggtitle("Insomnia Severity by Age")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
