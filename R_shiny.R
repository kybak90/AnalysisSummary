library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(modelsummary)

ui = fluidPage(
  titlePanel("Data Analysis"),
  
  tabsetPanel(
    
    # tabPanel 1
    tabPanel("Data Summary",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Upload the excel file", accept = c(".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
                 numericInput("num_head", "Number of rows to display", value = 10, min = 1),
                 checkboxGroupInput("dist_vars", "Check response distribution", choices = NULL),
                 checkboxGroupInput("preprocess", "Select preprocessing method",
                                    choices = c("Remove empty rows", "Set 9999 as NA", "Remove all rows with NA"))
               ),
               mainPanel(
                 tableOutput("data_preview"),
                 verbatimTextOutput("data_dim"),
                 tableOutput("var_summary"),
                 uiOutput("var_distribution"))
             )
    ),
    
    # tabPanel 2
    tabPanel("Variable Selection",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("var1_select"),
                 uiOutput("var2_select"),
                 sliderInput("ylim_var1", "Y-axis limit for Variable 1", min = 0, max = 1000, value = 500),
                 sliderInput("ylim_var2", "Y-axis limit for Variable 2", min = 0, max = 1000, value = 500)
               ),
               mainPanel(plotOutput("var_plots"))
             )
    ),
    
    # tabPanel 3
    tabPanel("Regression Analysis",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("response_var1_select"),
                 uiOutput("response_var2_select"),
                 selectInput("weight_var", "Select weight variable", choices = NULL),
                 checkboxGroupInput("predictor_vars", "Select predictor variables", choices = NULL, selected = NULL),
                 checkboxGroupInput("factor_vars", "Select factor variables", choices = NULL, selected = NULL),
                 actionButton("run_model", "Run Regression")
               ),
               mainPanel(
                 verbatimTextOutput("model_summary"),
                 plotOutput("regression_plots"))
             )
    ),
  )
)


server = function(input, output, session) {
  
  # tabPanel 1
  raw_data = reactive({
    req(input$file)
    read_excel(input$file$datapath)
  })
  
  processed_data = reactive({
    data = raw_data()
    if ("Set 9999 as NA" %in% input$preprocess) {
      data = data %>% mutate(across(everything(), ~ replace(., . == 9999, NA)))
    }
    if ("Remove empty rows" %in% input$preprocess) {
      data = data %>% filter(complete.cases(.))
    }
    if ("Remove all rows with NA" %in% input$preprocess) {
      data = na.omit(data)
    }
    if (!is.null(input$factor_vars)) {
      data = data %>% mutate(across(all_of(input$factor_vars), as.factor))
    }
    data
  })
  
  observe({
    req(raw_data())
    updateCheckboxGroupInput(session, "dist_vars", choices = names(raw_data()))
  })
  
  output$data_dim = renderPrint({
    req(processed_data())
    paste("Data dimension:", dim(processed_data())[1], "rows", dim(processed_data())[2], "columns")
  })
  
  output$data_preview = renderTable({
    req(processed_data())
    head(processed_data(), input$num_head)
  })
  
  output$var_distribution = renderUI({
    req(processed_data(), input$dist_vars)
    
    fluidRow(
      lapply(input$dist_vars, function(var) {
        column(4, tableOutput(paste0("dist_table_", var)))
      })
    )
  })
  
  # tabPanel 2
  observe({
    req(processed_data(), input$dist_vars)
    
    lapply(input$dist_vars, function(var) {
      output[[paste0("dist_table_", var)]] = renderTable({
        data = processed_data()
        size = nrow(data)
        data %>% group_by(.data[[var]]) %>% summarise(n = n(), prop = n() / size, .groups = "drop")
      })
    })
  })
  
  output$var1_select = renderUI({
    req(processed_data())  
    selectInput("var1", "Variable 1 select", 
                choices = names(processed_data()), 
                selected = names(processed_data())[1])
  })
  
  output$var2_select = renderUI({
    req(processed_data())  
    selectInput("var2", "Variable 2 select", 
                choices = names(processed_data()), 
                selected = names(processed_data())[2])
  })
  
  output$var_plots = renderPlot({
    req(processed_data(), input$var1, input$var2, input$ylim_var1, input$ylim_var2)
    
    data = processed_data()
    
    plot_var1 = ggplot(data, aes(x = .data[[input$var1]], fill = factor(.data[[input$var1]]))) +
      geom_bar() +
      ylim(0, input$ylim_var1) +
      theme(legend.position = "none") +
      xlab("") + ylab("") +
      ggtitle("Distribution of Variable 1") +
      theme(plot.title = element_text(size=20)) +
      theme_set(theme_bw())
    
    plot_var2 = ggplot(data, aes(x = .data[[input$var2]], fill = factor(.data[[input$var2]]))) +
      geom_bar() +
      ylim(0, input$ylim_var2) +
      theme(legend.position = "none") +
      xlab("") + ylab("") +
      ggtitle("Distribution of Variable 2") +
      theme(plot.title = element_text(size=20)) +
      theme_set(theme_bw())
    
    grid.arrange(plot_var1, plot_var2, ncol = 2)
  })
  
  # tabPanel 3
  observe({
    req(raw_data())
    updateCheckboxGroupInput(session, "dist_vars", choices = names(raw_data()))
    updateSelectInput(session, "weight_var", choices = names(raw_data()))
    updateCheckboxGroupInput(session, "predictor_vars", choices = names(raw_data()))
    updateCheckboxGroupInput(session, "factor_vars", choices = names(raw_data()), selected = names(raw_data()))
  })
  
  output$response_var1_select = renderUI({
    req(processed_data())  
    selectInput("response_var1", "Select Response Variable 1", choices = names(processed_data()))
  })
  
  output$response_var2_select = renderUI({
    req(processed_data())  
    selectInput("response_var2", "Select Response Variable 2", choices = names(processed_data()))
  })
  
  output$model_summary = renderPrint({
    req(model_results())
    cat("Model 1 Summary:\n")
    print(summary(model_results()$fit1))
    cat("\nModel 2 Summary:\n")
    print(summary(model_results()$fit2))
  })
  
  model_results = eventReactive(input$run_model, {
    req(processed_data(), input$weight_var, input$predictor_vars, input$response_var1, input$response_var2)
    dat = processed_data()
    predictors = paste(input$predictor_vars, collapse = " + ")
    
    formula1 = as.formula(paste(input$response_var1, "~", predictors))
    formula2 = as.formula(paste(input$response_var2, "~", predictors))
    
    fit1 = lm(formula1, data = dat, weights = dat[[input$weight_var]])
    fit2 = lm(formula2, data = dat, weights = dat[[input$weight_var]])
    
    list(fit1 = fit1, fit2 = fit2)
  })
  
  output$regression_plots = renderPlot({
    req(model_results())
    
    p1 = modelplot(model_results()$fit1, conf_level = .95, coef_omit = "Intercept") +
      aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
      scale_color_manual(values = c("gray", "blue")) +
      geom_vline(xintercept = 0, color = 'orange') +
      theme(legend.position = "none") +
      xlim(c(-0.5, 0.7)) +
      ggtitle("Model 1")
    
    p2 = modelplot(model_results()$fit2, conf_level = .95, coef_omit = "Intercept") +
      aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
      scale_color_manual(values = c("gray", "blue")) +
      geom_vline(xintercept = 0, color = 'orange') +
      theme(legend.position = "none") +
      xlim(c(-0.5, 0.7)) +
      ggtitle("Model 2")
    
    grid.arrange(p1, p2, ncol = 2)
  })

}

shinyApp(ui, server)
