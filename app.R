# ==================================
# Personalized Nutrition Planner
# With 7-Day Plan + Full Nutrition Trend
# ==================================

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

# -------------------------------
# Generate Indian Food Dataset
# -------------------------------

set.seed(123)

food_data <- data.frame(
  Food = c("Idli", "Dosa", "Paneer Butter Masala", "Chicken Curry",
           "Rajma", "Chole", "Egg Curry", "Fish Fry",
           "Upma", "Poha", "Dal Tadka", "Mutton Biryani",
           "Vegetable Biryani", "Curd Rice", "Samosa",
           "Paratha", "Omelette", "Grilled Chicken",
           "Palak Paneer", "Pav Bhaji"),
  
  Calories = sample(150:500, 20),
  Protein = sample(5:35, 20),
  Carbs = sample(20:80, 20),
  Fats = sample(5:30, 20),
  
  Diet = c("Veg", "Veg", "Veg", "Non-Veg",
           "Veg", "Veg", "Non-Veg", "Non-Veg",
           "Veg", "Veg", "Veg", "Non-Veg",
           "Veg", "Veg", "Veg",
           "Veg", "Non-Veg", "Non-Veg",
           "Veg", "Veg")
)

# -------------------------------
# UI
# -------------------------------

ui <- fluidPage(
  
  titlePanel("🥗 Personalized Nutrition Planner (Indian Food)"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput("weight", "Enter Weight (kg):", 60),
      numericInput("height", "Enter Height (cm):", 170),
      numericInput("age", "Enter Age:", 25),
      
      selectInput("gender", "Select Gender:",
                  choices = c("Male", "Female")),
      
      selectInput("goal", "Select Health Goal:",
                  choices = c("Weight Loss",
                              "Weight Gain",
                              "Maintain Weight")),
      
      selectInput("diet", "Diet Preference:",
                  choices = c("Veg", "Non-Veg", "Both")),
      
      actionButton("generate", "Generate 7-Day Plan")
    ),
    
    mainPanel(
      
      h3("Estimated Daily Calorie Requirement"),
      textOutput("calories"),
      
      h4("Weight Category"),
      textOutput("weight_status"),
      
      br(),
      
      h3("7-Day Auto Meal Plan"),
      tableOutput("weekly_plan"),
      
      br(),
      
      h3("Daily Target vs Weekly Average Intake"),
      textOutput("intake_status"),
      plotlyOutput("comparison_plot"),
      
      br(),
      
      h3("Macronutrient Distribution (Weekly Avg)"),
      plotlyOutput("macro_plot"),
      
      br(),
      
      h3("Weekly Nutritional Trend (Calories + Macros)"),
      plotlyOutput("nutrition_trend")
    )
  )
)

# -------------------------------
# SERVER

server <- function(input, output) {
  
  observeEvent(input$generate, {
    
    # BMI
    height_m <- input$height / 100
    bmi <- input$weight / (height_m^2)
    
    weight_category <- ifelse(bmi < 18.5, "Underweight",
                              ifelse(bmi < 24.9, "Perfect Weight",
                                     ifelse(bmi < 29.9, "Overweight",
                                            "Obese")))
    
    # BMR Calculation
    if(input$gender == "Male"){
      bmr <- 10*input$weight + 6.25*input$height - 5*input$age + 5
    } else {
      bmr <- 10*input$weight + 6.25*input$height - 5*input$age - 161
    }
    
    # Goal adjustment
    if(input$goal == "Weight Loss"){
      total_calories <- bmr - 500
    } else if(input$goal == "Weight Gain"){
      total_calories <- bmr + 500
    } else {
      total_calories <- bmr
    }
    
    output$calories <- renderText({
      paste(round(total_calories), "kcal per day")
    })
    
    output$weight_status <- renderText({
      paste("Your BMI:", round(bmi,1), "-", weight_category)
    })
    
    # Filter Diet
    if(input$diet == "Both"){
      filtered_food <- food_data
    } else {
      filtered_food <- food_data %>% filter(Diet == input$diet)
    }
    
    # 7-Day Plan
    weekly_plan <- data.frame()
    
    for(i in 1:7){
      temp_day <- filtered_food %>%
        slice_sample(n = 3)
      temp_day$Day <- paste("Day", i)
      weekly_plan <- rbind(weekly_plan, temp_day)
    }
    
    output$weekly_plan <- renderTable({
      weekly_plan %>% select(Day, Food, Calories, Protein, Carbs, Fats)
    })
    
    # Weekly Summary by Day
    weekly_summary <- weekly_plan %>%
      group_by(Day) %>%
      summarise(
        Calories = sum(Calories),
        Protein = sum(Protein),
        Carbs = sum(Carbs),
        Fats = sum(Fats)
      )
    
    avg_weekly_calories <- mean(weekly_summary$Calories)
    
    # Intake Status
    intake_message <- ifelse(avg_weekly_calories < total_calories,
                             "Your weekly average intake is BELOW your target.",
                             ifelse(avg_weekly_calories > total_calories,
                                    "Your weekly average intake is ABOVE your target.",
                                    "Your intake matches your target perfectly."))
    
    output$intake_status <- renderText({
      paste("Average Daily Intake:",
            round(avg_weekly_calories),
            "kcal —", intake_message)
    })
    
    # Comparison Bar Chart
    comparison_data <- data.frame(
      Type = c("Target Calories", "Average Intake"),
      Calories = c(total_calories, avg_weekly_calories)
    )
    
    output$comparison_plot <- renderPlotly({
      p <- ggplot(comparison_data,
                  aes(x = Type, y = Calories)) +
        geom_bar(stat = "identity") +
        theme_minimal()
      
      ggplotly(p)
    })
    
    # Macro Pie Chart (Interactive with Values)
    macro_data <- data.frame(
      Nutrient = c("Protein", "Carbs", "Fats"),
      Value = c(mean(weekly_plan$Protein),
                mean(weekly_plan$Carbs),
                mean(weekly_plan$Fats))
    )
    
    output$macro_plot <- renderPlotly({
      plot_ly(macro_data,
              labels = ~Nutrient,
              values = ~Value,
              type = 'pie',
              textinfo = 'label+percent+value',
              hoverinfo = 'label+value+percent')
    })
    
    # -------------------------------
    # NEW: Multi-Line Nutritional Trend Chart
    # -------------------------------
    
    weekly_long <- weekly_summary %>%
      pivot_longer(cols = -Day,
                   names_to = "Nutrient",
                   values_to = "Value")
    
    output$nutrition_trend <- renderPlotly({
      p <- ggplot(weekly_long,
                  aes(x = Day,
                      y = Value,
                      color = Nutrient,
                      group = Nutrient)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        theme_minimal()
      
      ggplotly(p)
    })
    
  })
}

# -------------------------------
# Run App
# -------------------------------

shinyApp(ui = ui, server = server)
