library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)

data(USArrests)
states_data <- USArrests
states_data$State <- rownames(USArrests)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Segoe UI', sans-serif; margin: 0; background: #f5f7fa; }
      .navbar { background: #4a90e2; color: white; padding: 15px 25px; 
                display: flex; justify-content: space-between; align-items: center; }
      .navbar-title { font-size: 20px; font-weight: 600; }
      .navbar-nav { display: flex; gap: 10px; }
      .nav-btn { background: transparent; border: 1px solid rgba(255,255,255,0.5); 
                 color: white; padding: 8px 16px; font-size: 13px; transition: all 0.3s; }
      .nav-btn:hover { background: rgba(255,255,255,0.2); }
      .nav-btn.active { background: white !important; color: #4a90e2 !important; font-weight: 500; }
      .content-area { padding: 25px; max-width: 1200px; margin: 0 auto; }
      .widget-box { background: white; padding: 20px; margin: 15px 0; border-left: 4px solid #4a90e2; }
      .widget-box h3 { color: #2c3e50; margin-bottom: 15px; font-size: 16px; font-weight: 600; }
      .chart-area { background: white; padding: 20px; margin: 20px 0; border: 1px solid #e1e8ed; }
      .sidebar-area { background: #ecf0f1; padding: 20px; margin: 15px 0; }
      .btn-custom { background: #4a90e2; color: white; border: none; padding: 10px 20px; 
                    margin: 5px; font-weight: 500; transition: background 0.3s; }
      .btn-custom:hover { background: #357abd; color: white; }
      .form-control { border: 1px solid #bdc3c7; padding: 8px 12px; }
      .form-control:focus { border-color: #4a90e2; outline: none; }
      h2 { color: #2c3e50; margin-bottom: 20px; font-weight: 600; }
      .results-box { background: #f8f9fa; padding: 15px; margin: 10px 0; border: 1px solid #dee2e6; }
    "))
  ),
  
  div(class = "navbar",
    div(class = "navbar-title", "US CRIME ANALYSIS TOOL"),
    div(class = "navbar-nav",
      actionButton("home_btn", "HOME", class = "nav-btn active"),
      actionButton("data_btn", "DATA OVERVIEW", class = "nav-btn"), 
      actionButton("analysis_btn", "ANALYSIS", class = "nav-btn"),
      actionButton("viz_btn", "VISUALIZATION", class = "nav-btn"),
      actionButton("reports_btn", "REPORTS", class = "nav-btn")
    )
  ),
  
  tags$script(HTML("
    $(document).on('click', '.nav-btn', function() {
      $('.nav-btn').removeClass('active');
      $(this).addClass('active');
    });
  ")),
  
  div(class = "content-area",
    conditionalPanel(
      condition = "output.current_page == 'home'",
      fluidRow(
        column(8,
          h2("WELCOME TO US CRIME ANALYSIS TOOL"),
          div(class = "widget-box",
            h3("PROJECT OVERVIEW"),
            p("Analyze violent crime statistics across US states including Murder, Assault, and Rape arrest rates per 100,000 residents, plus Urban Population percentages."),
            p("Target Users: Law enforcement, policy makers, researchers, analysts")
          ),
          div(class = "widget-box",
            h3("DATASET VARIABLES"),
            p(strong("Murder:"), "Arrests per 100,000 residents"),
            p(strong("Assault:"), "Arrests per 100,000 residents"),
            p(strong("UrbanPop:"), "Urban population percentage"),
            p(strong("Rape:"), "Arrests per 100,000 residents")
          ),
          div(class = "chart-area",
            plotlyOutput("overview_chart", height = "300px")
          )
        ),
        column(4,
          div(class = "sidebar-area",
            h3("QUICK ACTIONS"),
            actionButton("load_data", "LOAD DATA", class = "btn-custom"),
            br(), br(),
            downloadButton("export_csv", "EXPORT CSV", class = "btn-custom"),
          ),
          div(class = "sidebar-area",
            h3("DATA SUMMARY"),
            p("50 US States"),
            p("4 Crime Variables"),
            p("Year: 1973")
          )
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.current_page == 'data'",
      h2("DATA OVERVIEW"),
      div(class = "widget-box",
        h3("IMPORT/EXPORT CONTROLS"),
        fileInput("import_csv", "IMPORT CSV", accept = ".csv"),
        downloadButton("export_data", "EXPORT DATA", class = "btn-custom"),
        actionButton("reset_to_original", "RESET TO US CRIME DATA", class = "btn-custom")
      ),
      div(class = "widget-box",
        h3("DATA TYPE CONVERSION"),
        fluidRow(
          column(4, selectInput("convert_var", "Select Variable:", 
                               choices = c("Murder", "Assault", "UrbanPop", "Rape"))),
          column(4, selectInput("convert_type", "Convert To:",
                               choices = c("Numeric", "Categorical", "Factor"))),
          column(4, br(), actionButton("convert_btn", "CONVERT", class = "btn-custom"))
        )
      ),
      div(class = "widget-box",
        h3("DATA TABLE"),
        DT::dataTableOutput("data_table")
      )
    ),
    
    conditionalPanel(
      condition = "output.current_page == 'analysis'",
      h2("STATISTICAL ANALYSIS"),
      div(class = "widget-box",
        h3("ANALYSIS TYPE"),
        fluidRow(
          column(6, selectInput("analysis_type", "Select Analysis:",
                               choices = c("Descriptive Statistics", "Correlation Analysis", "State Ranking"))),
          column(6, br(), actionButton("run_analysis", "RUN ANALYSIS", class = "btn-custom"))
        )
      ),
      div(class = "widget-box",
        h3("LOOP PROCESSING"),
        p("FOR LOOP: Calculate crime totals for each state"),
        p("WHILE LOOP: Find states above threshold"),
        actionButton("start_loop", "START PROCESSING", class = "btn-custom"),
        div(class = "results-box",
          h4("Loop Results:"),
          verbatimTextOutput("loop_results")
        )
      ),
      div(class = "widget-box",
        h3("CONDITIONAL ANALYSIS"),
        fluidRow(
          column(4, selectInput("condition_action", "IF Crime Rate > Threshold:",
                               choices = c("Mark as High Risk", "Highlight in Red", "Add to Alert List"))),
          column(4, numericInput("threshold_val", "Threshold Value:", value = 10, min = 0)),
          column(4, br(), actionButton("apply_condition", "APPLY", class = "btn-custom"))
        ),
        div(class = "results-box",
          h4("Condition Results:"),
          verbatimTextOutput("condition_results")
        )
      ),
      div(class = "chart-area",
        h4("Analysis Results:"),
        verbatimTextOutput("analysis_results"),
        DT::dataTableOutput("analysis_table")
      )
    ),
    
    conditionalPanel(
      condition = "output.current_page == 'visualization'",
      h2("DATA VISUALIZATION"),
      div(class = "widget-box",
        h3("PLOT CONTROLS"),
        fluidRow(
          column(3, selectInput("x_var", "X-Axis Variable:",
                               choices = c("Murder", "Assault", "UrbanPop", "Rape"))),
          column(3, selectInput("y_var", "Y-Axis Variable:",
                               choices = c("Murder", "Assault", "UrbanPop", "Rape"), selected = "Assault")),
          column(3, selectInput("plot_color", "Plot Color:",
                               choices = c("Blue", "Purple", "Teal"))),
          column(3, br(), actionButton("update_plot", "UPDATE PLOT", class = "btn-custom"))
        )
      ),
      div(class = "widget-box",
        h3("PLOT TYPE SELECTION"),
        actionButton("scatter_plot", "SCATTER PLOT", class = "btn-custom"),
        actionButton("bar_chart", "BAR CHART", class = "btn-custom"),
        actionButton("histogram", "HISTOGRAM", class = "btn-custom"),
        actionButton("box_plot", "BOX PLOT", class = "btn-custom")
      ),
      div(class = "chart-area",
        plotlyOutput("main_plot", height = "400px")
      ),
      div(class = "widget-box",
        h3("PLOT OPTIONS"),
        fluidRow(
          column(6, textInput("plot_title", "Plot Title:", value = "US Crime Analysis")),
          column(3, checkboxInput("show_legend", "Show Legend:", value = TRUE)),
          column(3, br(), actionButton("update_options", "UPDATE PLOT", class = "btn-custom"))
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.current_page == 'reports'",
      h2("CRIME ANALYSIS REPORTS"),
      div(class = "widget-box",
        h3("REPORT GENERATION"),
        fluidRow(
          column(4, selectInput("report_type", "Report Type:",
                               choices = c("State Rankings Report", "Regional Comparison", "Crime Correlation Report"))),
          column(4, selectInput("include_vars", "Include Variables:",
                               choices = c("All Variables", "Murder Only", "Violent Crimes Only"))),
          column(4, br(), actionButton("generate_report", "GENERATE REPORT", class = "btn-custom"))
        )
      ),
      div(class = "widget-box",
        h3("EXPORT OPTIONS"),
        downloadButton("download_final", "DOWNLOAD CSV", class = "btn-custom")
      ),
      div(class = "chart-area",
        plotOutput("report_preview", height = "300px")
      ),
      div(class = "widget-box",
        h3("REPORT SUMMARY"),
        DT::dataTableOutput("summary_table")
      )
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(
    current_data = NULL,
    original_data = states_data,
    plot_type = "scatter",
    current_page = "home",
    loop_results = "",
    condition_results = ""
  )
  
  observeEvent(input$home_btn, { values$current_page <- "home" })
  observeEvent(input$data_btn, { values$current_page <- "data" })
  observeEvent(input$analysis_btn, { values$current_page <- "analysis" })
  observeEvent(input$viz_btn, { values$current_page <- "visualization" })
  observeEvent(input$reports_btn, { values$current_page <- "reports" })
  
  output$current_page <- reactive({ values$current_page })
  outputOptions(output, "current_page", suspendWhenHidden = FALSE)
  
  observeEvent(input$load_data, {
    values$current_data <- states_data 
    showNotification("Data loaded successfully", type = "message")
  })

  observeEvent(input$import_csv, {
    req(input$import_csv)
    tryCatch({
      imported_data <- read.csv(input$import_csv$datapath)
      imported_data$State <- rownames(imported_data)  
      values$current_data <- imported_data
      showNotification("CSV file imported successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error importing CSV:", e$message), type = "error")
    })
  })


  
  output$overview_chart <- renderPlotly({
    req(values$current_data)
    p <- ggplot(values$current_data, aes(x = Murder, y = Assault, text = State)) +
      geom_point(color = "#4a90e2", size = 3, alpha = 0.7) +
      theme_minimal() +
      labs(title = "Murder vs Assault Rates by State", x = "Murder Rate", y = "Assault Rate")
    ggplotly(p, tooltip = "text")
  })
  
  output$data_table <- DT::renderDataTable({
    req(values$current_data) 
    DT::datatable(values$current_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  observeEvent(input$convert_btn, {
    var_name <- input$convert_var
    if(input$convert_type == "Categorical") {
      values$current_data[[var_name]] <- cut(values$current_data[[var_name]], breaks = 3, 
                                           labels = c("Low", "Medium", "High"))
    } else if(input$convert_type == "Factor") {
      values$current_data[[var_name]] <- as.factor(values$current_data[[var_name]])
    } else {
      values$current_data[[var_name]] <- as.numeric(as.character(values$current_data[[var_name]]))
    }
    showNotification(paste("Converted", var_name, "to", input$convert_type), type = "message")
  })
  
  observeEvent(input$start_loop, {
    results <- c()
    orig_data <- values$original_data
    
    for(i in 1:min(10, nrow(orig_data))) {
      state_name <- orig_data$State[i]
      total_crime <- orig_data$Murder[i] + orig_data$Assault[i] + orig_data$Rape[i]
      results <- c(results, paste(state_name, "- Total Crime:", round(total_crime, 1)))
    }
    
    values$loop_results <- paste(results, collapse = "\n")
    showNotification("FOR Loop processing completed", type = "message")
  })
  
  output$loop_results <- renderText({
    values$loop_results
  })
  
  observeEvent(input$apply_condition, {
    threshold <- input$threshold_val
    orig_data <- values$original_data
    high_crime_states <- orig_data[orig_data$Murder > threshold, ]
    
    if(nrow(high_crime_states) > 0) {
      if(input$condition_action == "Mark as High Risk") {
        values$current_data$RiskLevel <- ifelse(orig_data$Murder > threshold, "High Risk", "Normal")
        values$condition_results <- paste("High Risk States (Murder >", threshold, "):\n",
                                        paste(high_crime_states$State, collapse = ", "))
      } else if(input$condition_action == "Add to Alert List") {
        values$condition_results <- paste("ALERT LIST - High Murder Rate States:\n",
                                        paste(high_crime_states$State, collapse = "\n"))
      }
    } else {
      values$condition_results <- paste("No states found with Murder rate above", threshold)
    }
    showNotification("Conditional analysis applied", type = "message")
  })
  
  output$condition_results <- renderText({
    values$condition_results
  })
  
  output$analysis_results <- renderText({
    if(input$run_analysis > 0) {
      orig_data <- values$original_data
      if(input$analysis_type == "Descriptive Statistics") {
        paste("DESCRIPTIVE STATISTICS:\n",
              "Mean Murder Rate:", round(mean(orig_data$Murder, na.rm = TRUE), 2), "\n",
              "Mean Assault Rate:", round(mean(orig_data$Assault, na.rm = TRUE), 2), "\n",
              "Mean Urban Pop:", round(mean(orig_data$UrbanPop, na.rm = TRUE), 2), "%\n",
              "SD Murder:", round(sd(orig_data$Murder, na.rm = TRUE), 2))
      } else if(input$analysis_type == "Correlation Analysis") {
        cor_murder_assault <- cor(orig_data$Murder, orig_data$Assault, use = "complete.obs")
        cor_murder_urban <- cor(orig_data$Murder, orig_data$UrbanPop, use = "complete.obs")
        paste("CORRELATION ANALYSIS:\n",
              "Murder vs Assault:", round(cor_murder_assault, 3), "\n",
              "Murder vs Urban Pop:", round(cor_murder_urban, 3))
      } else if(input$analysis_type == "State Ranking") {
        top_murder <- orig_data[order(-orig_data$Murder)[1:5], ]
        paste("TOP 5 HIGHEST MURDER RATES:\n",
              paste(1:5, ". ", top_murder$State, " (", round(top_murder$Murder, 1), ")", sep = "", collapse = "\n"))
      }
    }
  })
  
  observeEvent(input$scatter_plot, { values$plot_type <- "scatter" })
  observeEvent(input$bar_chart, { values$plot_type <- "bar" })
  observeEvent(input$histogram, { values$plot_type <- "histogram" })
  observeEvent(input$box_plot, { values$plot_type <- "box" })
  
  output$main_plot <- renderPlotly({
    colors <- c("Blue" = "#4a90e2", "Purple" = "#9b59b6", "Teal" = "#1abc9c")
    plot_color <- colors[input$plot_color]
    
    if(values$plot_type == "scatter") {
      if(is.numeric(values$current_data[[input$x_var]]) && is.numeric(values$current_data[[input$y_var]])) {
        p <- ggplot(values$current_data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], text = State)) +
          geom_point(color = plot_color, size = 3, alpha = 0.7) +
          theme_minimal() +
          labs(title = input$plot_title, x = input$x_var, y = input$y_var)
      } else {
        p <- ggplot(values$original_data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], text = State)) +
          geom_point(color = plot_color, size = 3, alpha = 0.7) +
          theme_minimal() +
          labs(title = paste(input$plot_title, "(Original Data)"), x = input$x_var, y = input$y_var)
      }
    } else if(values$plot_type == "bar") {
      if(is.numeric(values$current_data[[input$x_var]])) {
        top_10 <- head(values$current_data[order(-values$current_data[[input$x_var]]), ], 10)
        p <- ggplot(top_10, aes(x = reorder(State, .data[[input$x_var]]), y = .data[[input$x_var]])) +
          geom_col(fill = plot_color, alpha = 0.7) +
          coord_flip() +
          theme_minimal() +
          labs(title = paste(input$plot_title, "- Top 10 States"), x = "State", y = input$x_var)
      } else {
        top_10 <- head(values$original_data[order(-values$original_data[[input$x_var]]), ], 10)
        p <- ggplot(top_10, aes(x = reorder(State, .data[[input$x_var]]), y = .data[[input$x_var]])) +
          geom_col(fill = plot_color, alpha = 0.7) +
          coord_flip() +
          theme_minimal() +
          labs(title = paste(input$plot_title, "- Top 10 States (Original Data)"), x = "State", y = input$x_var)
      }
    } else if(values$plot_type == "histogram") {
      if(is.numeric(values$current_data[[input$x_var]])) {
        p <- ggplot(values$current_data, aes(x = .data[[input$x_var]])) +
          geom_histogram(bins = 10, fill = plot_color, alpha = 0.7, color = "white") +
          theme_minimal() +
          labs(title = input$plot_title, x = input$x_var, y = "Frequency")
      } else {
        p <- ggplot(values$current_data, aes(x = .data[[input$x_var]])) +
          geom_bar(fill = plot_color, alpha = 0.7) +
          theme_minimal() +
          labs(title = paste(input$plot_title, "(Categorical)"), x = input$x_var, y = "Count")
      }
    } else if(values$plot_type == "box") {
      if(is.numeric(values$current_data[[input$x_var]])) {
        p <- ggplot(values$current_data, aes(x = "", y = .data[[input$x_var]])) +
          geom_boxplot(fill = plot_color, alpha = 0.7) +
          theme_minimal() +
          labs(title = input$plot_title, x = "", y = input$x_var)
      } else {
        p <- ggplot(values$original_data, aes(x = "", y = .data[[input$x_var]])) +
          geom_boxplot(fill = plot_color, alpha = 0.7) +
          theme_minimal() +
          labs(title = paste(input$plot_title, "(Original Data)"), x = "", y = input$x_var)
      }
    }
    
    if(input$show_legend && values$plot_type == "scatter") {
      p <- p + geom_point(aes(color = State), show.legend = FALSE)
    }
    
    ggplotly(p, tooltip = if(values$plot_type == "scatter") "text" else NULL)
  })
  
  output$report_preview <- renderPlot({
    if(input$generate_report > 0) {
      orig_data <- values$original_data
      if(input$report_type == "State Rankings Report") {
        if(input$include_vars == "Murder Only") {
          top_states <- head(orig_data[order(-orig_data$Murder), ], 10)
          ggplot(top_states, aes(x = reorder(State, Murder), y = Murder)) +
            geom_col(fill = "#4a90e2", alpha = 0.7) +
            coord_flip() +
            theme_minimal() +
            labs(title = "Top 10 States - Murder Rate Ranking", x = "State", y = "Murder Rate")
        } else if(input$include_vars == "Violent Crimes Only") {
          orig_data$TotalViolent <- orig_data$Murder + orig_data$Assault + orig_data$Rape
          crime_data <- head(orig_data[order(-orig_data$TotalViolent), ], 10)
          ggplot(crime_data, aes(x = reorder(State, TotalViolent), y = TotalViolent)) +
            geom_col(fill = "#4a90e2", alpha = 0.7) +
            coord_flip() +
            theme_minimal() +
            labs(title = "Top 10 States - Total Violent Crime Ranking", x = "State", y = "Total Violent Crimes")
        } else {
          ggplot(orig_data, aes(x = Murder, y = Assault, size = UrbanPop, color = Rape)) +
            geom_point(alpha = 0.7) +
            scale_color_gradient(low = "#4a90e2", high = "#e74c3c") +
            theme_minimal() +
            labs(title = "State Crime Analysis - All Variables", 
                 subtitle = "Size = Urban Population, Color = Rape Rate")
        }
      } else if(input$report_type == "Regional Comparison") {
        ggplot(orig_data, aes(x = UrbanPop, y = Murder)) +
          geom_point(color = "#4a90e2", size = 3, alpha = 0.7) +
          geom_smooth(method = "lm", color = "#e74c3c") +
          theme_minimal() +
          labs(title = "Regional Analysis: Urban Population vs Murder Rate")
      } else {
        ggplot(orig_data, aes(x = Murder, y = Assault)) +
          geom_point(color = "#4a90e2", size = 3, alpha = 0.7) +
          geom_smooth(method = "lm", color = "#e74c3c") +
          theme_minimal() +
          labs(title = "Crime Correlation Report: Murder vs Assault")
      }
    }
  })
  
  output$summary_table <- DT::renderDataTable({
    orig_data <- values$original_data
    summary_data <- data.frame(
      Metric = c("Murder Rate", "Assault Rate", "Urban Pop", "Rape Rate"),
      Highest_State = c(
        orig_data$State[which.max(orig_data$Murder)],
        orig_data$State[which.max(orig_data$Assault)],
        orig_data$State[which.max(orig_data$UrbanPop)],
        orig_data$State[which.max(orig_data$Rape)]
      ),
      Lowest_State = c(
        orig_data$State[which.min(orig_data$Murder)],
        orig_data$State[which.min(orig_data$Assault)],
        orig_data$State[which.min(orig_data$UrbanPop)],
        orig_data$State[which.min(orig_data$Rape)]
      )
    )
    DT::datatable(summary_data, options = list(dom = 't'))
  })
  
  output$export_csv <- downloadHandler(
    filename = function() { paste("crime_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(values$current_data, file, row.names = FALSE) }
  )
  
  output$download_final <- downloadHandler(
    filename = function() { paste("crime_summary_report_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { 
      orig_data <- values$original_data
      summary_data <- data.frame(
        Metric = c("Murder Rate", "Assault Rate", "Urban Pop", "Rape Rate"),
        Highest_State = c(
          orig_data$State[which.max(orig_data$Murder)],
          orig_data$State[which.max(orig_data$Assault)],
          orig_data$State[which.max(orig_data$UrbanPop)],
          orig_data$State[which.max(orig_data$Rape)]
        ),
        Highest_Value = c(
          max(orig_data$Murder),
          max(orig_data$Assault),
          max(orig_data$UrbanPop),
          max(orig_data$Rape)
        ),
        Lowest_State = c(
          orig_data$State[which.min(orig_data$Murder)],
          orig_data$State[which.min(orig_data$Assault)],
          orig_data$State[which.min(orig_data$UrbanPop)],
          orig_data$State[which.min(orig_data$Rape)]
        ),
        Lowest_Value = c(
          min(orig_data$Murder),
          min(orig_data$Assault),
          min(orig_data$UrbanPop),
          min(orig_data$Rape)
        )
      )
      write.csv(summary_data, file, row.names = FALSE)
    }
  )

  output$export_data <- downloadHandler(
    filename = function() { paste("crime_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(values$current_data, file, row.names = FALSE) }
  )

  observeEvent(input$reset_to_original, {
  values$current_data <- states_data  
  showNotification("Data reset to original US Crime dataset", type = "message")
})

}

shinyApp(ui = ui, server = server)