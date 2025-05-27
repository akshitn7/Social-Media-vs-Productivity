library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

# Load your dataset
data_all <- read.csv("cleaned_data.csv")

ui <- dashboardPage(
  dashboardHeader(title = "ProdByHabits"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plot", tabName = "plot", icon = icon("chart-bar")),
      menuItem("T-Test", tabName = "ttest", icon = icon("flask")),
      menuItem("ANOVA", tabName = "anova", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # ---- PLOT TAB ----
      tabItem(tabName = "plot",
              h2("Data Visualization"),
              selectInput("plot_var", "Select Column", choices = names(data_all)),
              plotOutput("plot_output")
      ),
      
      # ---- T-TEST TAB ----
      tabItem(tabName = "ttest",
              h2("T-Test"),
              radioButtons("sample_count", "Number of Samples:",
                           choices = c("One Sample" = "one", "Two Sample" = "two")),
              uiOutput("ttest_var_ui"),
              actionButton("run_ttest", "Run T-Test"),
              verbatimTextOutput("ttest_result")
      ),
      
      # ---- ANOVA TAB ----
      tabItem(tabName = "anova",
              h2("ANOVA Test"),
              uiOutput("anova_ui"),
              actionButton("run_anova", "Run ANOVA"),
              verbatimTextOutput("anova_result")
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  # ---- T-TEST ----
  output$ttest_var_ui <- renderUI({
    numeric_vars <- names(data_all)[sapply(data_all, is.numeric)]
    
    if (input$sample_count == "one") {
      tagList(
        selectInput("var1", "Select Variable", numeric_vars),
        numericInput("mu", "Hypothesized Mean", value = 0)
      )
    } else {
      tagList(
        selectInput("group_var", "Select Variable to Split into samples", numeric_vars),
        numericInput("cutoff", "Split Cutoff Value", value = 0),
        selectInput("target_var", "Select Target Variable for T-Test", numeric_vars)
      )
    }
  })
  
  observeEvent(input$run_ttest, {
    df <- data_all
    result <- NULL
    
    if (input$sample_count == "one") {
      req(input$var1, input$mu)
      result <- t.test(df[[input$var1]], mu = input$mu)
      
    } else if (input$sample_count == "two") {
      req(input$group_var, input$cutoff, input$target_var)
      
      # Split into two groups
      group_low <- df[[input$group_var]] < input$cutoff
      group_high <- df[[input$group_var]] >= input$cutoff
      
      data_low <- df[group_low, input$target_var, drop = TRUE]
      data_high <- df[group_high, input$target_var, drop = TRUE]
      
      # Ensure both groups have data
      if (length(data_low) < 2 || length(data_high) < 2) {
        result <- "Not enough data in one or both groups to perform t-test."
      } else {
        result <- t.test(data_low, data_high)
      }
    }
    
    output$ttest_result <- renderPrint({ result })
  })
  
  
  # ---- ANOVA ----
  output$anova_ui <- renderUI({
    tagList(
      selectInput("anova_dep", "Dependent Numeric Variable", c("online_hours", "work_hours", "self_prod_score", "actual_prod_score", "stress", "sleep_hours","job_score")),
      selectInput("anova_group", "Grouping Variable", c("age_group", "focus_app", "digital_wellbeing", "app", "job", "gender"))
    )
  })
  
  observeEvent(input$run_anova, {
    req(input$anova_dep, input$anova_group)
    result <- aov(as.formula(paste(input$anova_dep, "~", input$anova_group)), data = data_all)
    output$anova_result <- renderPrint({ summary(result) })
  })
  
  # ---- PLOT ----
  output$plot_output <- renderPlot({
    req(input$plot_var)
    var_data <- data_all[[input$plot_var]]
    
    if (is.numeric(var_data)) {
      ggplot(data_all, aes(x = .data[[input$plot_var]])) +
        geom_histogram(binwidth = 1,fill = "purple", color = "black", alpha = 0.5) +
        labs(title = paste("Histogram of", input$plot_var), x = input$plot_var)
    } else {
      ggplot(data_all, aes(x = .data[[input$plot_var]])) +
        geom_bar(fill = "coral", color = "black") +
        labs(title = paste("Bar Plot of", input$plot_var), x = input$plot_var) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

shinyApp(ui, server)