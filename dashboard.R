library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

# Load your dataset
data_all <- read.csv("cleaned_data.csv")

ui <- dashboardPage(
  dashboardHeader(title = "SocialMedia"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("T-Test", tabName = "ttest", icon = icon("flask")),
      menuItem("ANOVA", tabName = "anova", icon = icon("chart-line")),
      menuItem("Plot", tabName = "plot", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # ---- T-TEST TAB ----
      tabItem(tabName = "ttest",
              h2("T-Test"),
              radioButtons("sample_count", "Number of Samples:",
                           choices = c("One Sample" = "one", "Two Sample" = "two")),
              uiOutput("ttest_var_ui"),
              numericInput("mu", "Hypothesized Mean", value = 0),
              actionButton("run_ttest", "Run T-Test"),
              verbatimTextOutput("ttest_result")
      ),
      
      # ---- ANOVA TAB ----
      tabItem(tabName = "anova",
              h2("ANOVA Test"),
              uiOutput("anova_ui"),
              actionButton("run_anova", "Run ANOVA"),
              verbatimTextOutput("anova_result")
      ),
      
      # ---- PLOT TAB ----
      tabItem(tabName = "plot",
              h2("Data Visualization"),
              selectInput("plot_var", "Select Column", choices = names(data_all)),
              plotOutput("plot_output")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ---- T-TEST ----
  output$ttest_var_ui <- renderUI({
    numeric_vars <- names(data_all)[sapply(data_all, is.numeric)]
    if (input$sample_count == "one") {
      selectInput("var1", "Select Variable", numeric_vars)
    } else {
      tagList(
        selectInput("var1", "Select Variable 1", numeric_vars),
        selectInput("var2", "Select Variable 2", numeric_vars)
      )
    }
  })
  
  observeEvent(input$run_ttest, {
    df <- data_all
    result <- NULL
    
    if (input$sample_count == "one") {
      req(input$var1)
      result <- t.test(df[[input$var1]], mu = input$mu)
    } else if (input$sample_count == "two") {
      req(input$var1, input$var2)
      result <- t.test(df[[input$var1]], df[[input$var2]])
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
        geom_histogram(fill = "purple", color = "black", bins = 30) +
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