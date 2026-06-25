library(shiny)
library(bslib)
library(shinyjs)
library(shinycssloaders)
library(ggplot2)
library(glmnet)

if (file.exists("tax_model3.rds")) {
  model <- readRDS("tax_model3.rds")
} else {
  model <- list()
}

gender_map <- c("Female" = 0, "Male" = 1)
city_map <- c(
  "Austin" = 1, "Boston" = 2, "Chicago" = 3, "Columbus" = 4, "Dallas" = 5,
  "Denver" = 6, "Fort Worth" = 7, "Houston" = 8, "Indianapolis" = 9,
  "Jacksonville" = 10, "Los Angeles" = 11, "Nashville" = 12, "New York" = 13,
  "Philadelphia" = 14, "Phoenix" = 15, "Portland" = 16, "San Antonio" = 17,
  "San Diego" = 18, "San Jose" = 19, "Seattle" = 20
)
profession_map <- c("Businessman" = 1, "Designer" = 2, "Doctor" = 3, "Engineer" = 4, "Freelancer" = 5, "Teacher" = 6)
marital_map <- c("Single" = 1, "Married" = 2, "Divorced" = 3, "Widowed" = 4)

AGE_MIN <- 18
AGE_MAX <- 63

log1p_ <- function(x) log(x + 1)
scale_age <- function(age) (age - AGE_MIN) / (AGE_MAX - AGE_MIN)

money_input <- function(id, label, value) {
  numericInput(id, label, value = value, min = 0, step = 500)
}

ui <- page_navbar(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty", 
    primary = "#2c7fb8",
    base_font = font_google("Inter")
  ),
  title = "TAX INTELLIGENCE — FUTURE TAX PREDICTOR",
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .card { 
        border-radius: 12px; 
        box-shadow: 0 4px 15px rgba(0,0,0,0.05); 
        transition: transform 0.2s ease-in-out; 
      }
      .card:hover { transform: translateY(-2px); }
      .btn-primary { 
        background: linear-gradient(135deg, #2c7fb8 0%, #1d5a84 100%); 
        border: none;
        transition: all 0.3s ease;
      }
      .btn-primary:hover { 
        transform: scale(1.02);
        box-shadow: 0 4px 12px rgba(44, 127, 184, 0.4);
      }
      .table-container, .plot-container { animation: fadeIn 0.8s ease-in-out; }
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
    "))
  ),
  
  nav_panel(
    title = "Future Tax Predictor",
    p("Now we can forecast tax with the help of AI for next 3 years"),
    layout_sidebar(
      sidebar = sidebar(
        width = 380,
        title = "Parameters & Financials",
        open = TRUE,
        
        accordion(
          accordion_panel(
            "Demographics", icon = icon("user"),
            selectInput("gender", "Gender", choices = names(gender_map)),
            numericInput("age", "Age (years)", value = 45, min = AGE_MIN, max = AGE_MAX, step = 1),
            selectInput("profession", "Profession", choices = names(profession_map)),
            selectInput("marital_status", "Marital Status", choices = names(marital_map)),
            selectInput("city", "City", choices = names(city_map))
          ),
          accordion_panel(
            "Yearly Income ($)", icon = icon("wallet"),
            layout_column_wrap(
              width = "50%",
              money_input("yearly_income_2019", "Income 2019", 60000), money_input("yearly_income_2020", "Income 2020", 62000),
              money_input("yearly_income_2021", "Income 2021", 65000), money_input("yearly_income_2022", "Income 2022", 68000),
              money_input("yearly_income_2023", "Income 2023", 71000), money_input("yearly_income_2024", "Income 2024", 74000),
              money_input("yearly_income_2025", "Income 2025", 77000)
            )
          ),
          accordion_panel(
            "Yearly Tax Paid ($)", icon = icon("receipt"),
            layout_column_wrap(
              width = "50%",
              money_input("yearly_tax_2019", "Tax 2019", 9000), money_input("yearly_tax_2020", "Tax 2020", 9300),
              money_input("yearly_tax_2021", "Tax 2021", 9800), money_input("yearly_tax_2022", "Tax 2022", 10200),
              money_input("yearly_tax_2023", "Tax 2023", 10700), money_input("yearly_tax_2024", "Tax 2024", 11100),
              money_input("yearly_tax_2025", "Tax 2025", 11600)
            )
          ),
          accordion_panel(
            "Other Financials ($)", icon = icon("chart-line"),
            money_input("investments", "Investments", 15000),
            money_input("deductions", "Deductions", 9000),
            money_input("annual_bonus", "Annual Bonus", 5000),
            money_input("business_income", "Business Income", 2000),
            hr(),
            h4("Forecast Assumption"),
            numericInput("income_growth", "Assumed annual income growth (%)", value = 3, min = -20, max = 50, step = 0.5),
            helpText("Used to project 2026-2028 income, since the model needs a full 7-year income window to roll forward each year's forecast.")
          )
        ),
        br(),
        actionButton("predict_btn", "Predict Future Tax (2026-2028)", icon = icon("wand-magic-sparkles"), class = "btn-primary w-100 py-2")
      ),
      
      layout_column_wrap(
        width = 1,
        card(
          card_header(class = "bg-primary text-white", tags$span(icon("chart-bar"), " 10 Year Tax Record (2019-2028)")),
          card_body(
            div(class = "plot-container",
                withSpinner(plotOutput("tax_distribution_plot", height = "320px"), type = 7, color = "#2c7fb8")
            )
          )
        ),
        card(
          card_header(tags$span(icon("table"), " Predicted Tax 2026-28")),
          card_body(
            div(class = "table-container",
                tableOutput("prediction_table")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  full_timeline_data <- eventReactive(input$predict_btn, {
    Sys.sleep(0.7) 
    
    g <- input$income_growth / 100
    incomes <- c(input$yearly_income_2019, input$yearly_income_2020, input$yearly_income_2021,
                 input$yearly_income_2022, input$yearly_income_2023, input$yearly_income_2024, input$yearly_income_2025)
    taxes <- c(input$yearly_tax_2019, input$yearly_tax_2020, input$yearly_tax_2021,
               input$yearly_tax_2022, input$yearly_tax_2023, input$yearly_tax_2024, input$yearly_tax_2025)
    
    historical_df <- data.frame(
      Year = 2019:2025,
      Tax = taxes,
      Type = "Historical Baseline"
    )
    
    age_years <- input$age
    forecasted_results <- data.frame(Year = integer(0), Tax = numeric(0), Type = character(0))
    
    for (step in 1:3) {
      target_year <- 2025 + step
      next_income <- incomes[length(incomes)] * (1 + g)
      
      if (inherits(model, "cv.glmnet")) {
        row <- data.frame(
          gender = gender_map[[input$gender]], 
          age = scale_age(age_years),
          profession = profession_map[[input$profession]], 
          marital_status = marital_map[[input$marital_status]],
          city = city_map[[input$city]], 
          yearly_income_2019 = log1p_(incomes[1]), 
          yearly_income_2020 = log1p_(incomes[2]),
          yearly_income_2021 = log1p_(incomes[3]), 
          yearly_income_2022 = log1p_(incomes[4]), 
          yearly_income_2023 = log1p_(incomes[5]),
          yearly_income_2024 = log1p_(incomes[6]), 
          yearly_income_2025 = log1p_(incomes[7]), 
          yearly_tax_2019 = log1p_(taxes[1]),
          yearly_tax_2020 = log1p_(taxes[2]), 
          yearly_tax_2021 = log1p_(taxes[3]), 
          yearly_tax_2022 = log1p_(taxes[4]),
          yearly_tax_2023 = log1p_(taxes[5]), 
          yearly_tax_2024 = log1p_(taxes[6]), 
          yearly_tax_2025 = log1p_(taxes[7]),
          investments = log1p_(input$investments), 
          deductions = log1p_(input$deductions), 
          annual_bonus = log1p_(input$annual_bonus),
          business_income = log1p_(input$business_income)
        )
        
        expected_order <- c(
          "gender", "age", "profession", "marital_status", "city",
          "yearly_income_2019", "yearly_income_2020", "yearly_income_2021", 
          "yearly_income_2022", "yearly_income_2023", "yearly_income_2024", "yearly_income_2025",
          "yearly_tax_2019", "yearly_tax_2020", "yearly_tax_2021", 
          "yearly_tax_2022", "yearly_tax_2023", "yearly_tax_2024", "yearly_tax_2025",
          "investments", "deductions", "annual_bonus", "business_income"
        )
        row <- row[, expected_order, drop = FALSE]
        
        row_matrix <- as.matrix(row)
        pred_log <- predict(model, newx = row_matrix, s = "lambda.min")[1, 1]
        pred_dollars <- exp(pred_log) - 1
      } else {
        pred_dollars <- (next_income * 0.15) + (age_years * 12)
      }
      
      forecasted_results <- rbind(forecasted_results, data.frame(
        Year = target_year, 
        Tax = round(pred_dollars, 2),
        Type = "AI Forecast"
      ))
      
      incomes <- c(incomes[-1], next_income)
      taxes <- c(taxes[-1], pred_dollars)
      age_years <- age_years + 1
    }
    
    rbind(historical_df, forecasted_results)
  })
  
  output$prediction_table <- renderTable({
    if (input$predict_btn == 0) {
      return(data.frame(Note = "Set the inputs on the left and click 'Predict Future Tax (2026-2028)'."))
    }
    df <- subset(full_timeline_data(), Type == "AI Forecast")
    df$Tax <- paste0("$", formatC(df$Tax, format = "f", big.mark = ",", digits = 2))
    names(df) <- c("Year", "Predicted Tax", "Source Status")
    df[,-3] 
  }, striped = TRUE, hover = TRUE, bordered = FALSE, align = "c")
  
  output$tax_distribution_plot <- renderPlot({
    if (input$predict_btn == 0) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "Click 'Predict Future Tax' to populate chronological chart visualization.", size = 5, color = "grey50") + theme_void())
    }
    
    plot_data <- full_timeline_data()
    plot_data$Type <- factor(plot_data$Type, levels = c("Historical Baseline", "AI Forecast"), labels = c("Historical Tax Record", "Predicted Tax"))
    
    ggplot(plot_data, aes(x = factor(Year), y = Tax, fill = Type)) +
      geom_bar(stat = "identity", width = 0.72, alpha = 0.9, position = "identity", color = NA) +
      scale_fill_manual(values = c("Historical Tax Record" = "#2c7fb8", "Predicted Tax" = "#2ca25f")) +
      scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
      labs(x = NULL, y = "Total Annual Tax", fill = NULL) +
      theme_minimal(base_family = "sans") +
      theme(
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, color = "#4A4A4A"),
        axis.title.y = element_text(size = 13, margin = margin(r = 10)),
        legend.text = element_text(size = 11),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      )
  }, bg = "transparent")
}

shinyApp(ui = ui, server = server)