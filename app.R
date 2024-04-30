#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
library(tibble)
library(workflows)
library(glmnet)
library(tidymodels)
library(readr)
library(dplyr)
library(textrecipes)

# Define UI for application that draws a histogram
model <- readRDS("best_lasso_model.rds")
ui <- fluidPage(
  titlePanel("Student Score Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("weekly_study_time", "Weekly Study Time", 2),
      selectInput("extra_educational_support", "Extra Educational Support", choices = c("school support", "no school support"), selected = "no school support"),
      selectInput("family_educational_support", "Family Educational Support", choices = c("family support", "no family support"), selected = "no family support"),
      selectInput("paid_extra_classes", "Paid Extra Classes", choices = c("yes", "no"), selected = "no"),
      selectInput("extra_curricular_activities", "Extra Curricular Activities", choices = c("yes", "no"), selected = "no"),
      selectInput("romantic_relationship", "Romantic Relationship", choices = c("yes", "no"), selected = "no"),
      numericInput("free_time_after_school", "Free Time After School", 3),
      numericInput("weekend_alcohol_consumption", "Weekend Alcohol Consumption", 1),
      actionButton("predict", "Predict")
    ),
    mainPanel(
      h5("Student Name: Hitmontop"),
      h5("Basic Information"),
      p("Sex: Female", HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"), "        Age: 16", HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"), "Family size: less or equal to 3"),
      p("School Name: GPL", HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"), "School Choice Reason: other", HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"), "Guardian: Mother"),
      p("Home Address Type: urban", HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"), "Parent Cohabitation Status: Living together"),
      p("Mother Education: secondary education", HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"), "Mother Job: Teacher"),
      p("Father Education: secondary education", HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"), "Father Job: Other"),
      p("Past Class Failures: 0"),
      verbatimTextOutput("prediction"),
      plotOutput("distribution")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  prediction <- eventReactive(input$predict, {
    input_data <- tibble(
      student_school = "GPl",
      student_sex = "M",
      student_age = 16,
      home_address_type = "U",
      family_size = "LE3",
      parent_cohabitation_status = "T",
      mother_education = 3,
      father_education = 3,
      mother_job = 'teacher',
      father_job = 'other',
      school_choice_reason = "other",
      student_guardian = "mother",
      travel_time = 1,
      weekly_study_time = input$weekly_study_time,
      past_class_failures = 0,
      extra_educational_support = input$extra_educational_support,
      family_educational_support = input$family_educational_support,
      paid_extra_classes = input$paid_extra_classes,
      extra_curricular_activities = input$extra_curricular_activities,
      attended_nursery_school = "yes",
      higher_education_aspiration = "yes",
      internet_access_at_home = "yes",
      romantic_relationship = input$romantic_relationship,
      family_relationship_quality = 1,
      free_time_after_school = input$free_time_after_school,
      going_out_with_friends = 3,
      workday_alcohol_consumption = 3,
      weekend_alcohol_consumption = input$weekend_alcohol_consumption,
      current_health_status = 3,
      school_absences = 0,
      major = "Math",
      id = 400,
      period = as.character(1),
      guardian_education = as.character(3),
      guardian_job = "teacher"
    )
    
    # Preprocess the input data as required by your model
    # (e.g., scaling, encoding categorical variables, etc.)
    
    # Predict the score using the Lasso model
    predicted_score <- predict(model, new_data = input_data)
    
    # Return the predicted score
    return(predicted_score)
  })
  
  
  score_distribution <- eventReactive(input$predict, {
    student_data <- read.csv("data/student-cleaned.csv")
    
    predicted_score_value <- as.numeric(prediction())
    
    print(predicted_score_value)
    
    ggplot(student_data, aes(x = score)) +
      geom_bar() +
      geom_point(aes(x = predicted_score_value, y = 0), shape = 17, size = 8, color = "red") +
      labs(title = "Here is the expected position of your Child:", y = "count")
  })
  
  output$prediction <- renderText({ 
    paste("Your score prediction is:", prediction())
  })
  
  output$distribution <- renderPlot({
    score_distribution()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
