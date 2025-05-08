library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)
library(bs4Dash)
library(bslib)
library(plotly)
library(readxl)
library(lubridate)
library(treemapify)
library(tidyr)
library(colorspace)
library(RColorBrewer)
library(forcats)
library(shinycssloaders)
library(shinyjs)
library(extrafont)
suppressMessages(loadfonts())

# Load New Business Data
sales_data <- read_excel("./data/Sales Data New Business.xlsx", 
                         sheet = "2024 Data", col_types = c("date","text", "text", "text", "text", "numeric", "text", "text", "text", "text", "numeric", "text", "text"))

# Load Renewals Data
sales_data_renewals <- read_excel("./data/Sales Data Renewal Business.xlsx", 
                                  sheet = "2024 Data", col_types = c("text", "text", "text", "text", "text", "date", "text", "numeric", "numeric", "text", "text"))

# Load Health Business Data 
sales_data_health <- read_excel("./data/Sales Data Health.xlsx", 
                                sheet = "2024 Data", col_types = c("date",  "text", "text", "text", "numeric", "text", "text", "text", "text", "text", "numeric", "text"))

# Load Premiums Data 
premium_data <- read_excel("./data/Selected_Premium_Data.xlsx", col_types = c("text", "text", "text", "date", "date", "text", "numeric", "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric",  "numeric", "text", "text", "text", "text", "text", "text", "text")) %>%
  mutate(
    Year = format(POLICY_FROM_DATE, "%Y"),
    Month = format(POLICY_FROM_DATE, "%B"),
    Quarter = paste0("Q", lubridate::quarter(POLICY_FROM_DATE))
  )

claims_data <- read_excel("C:/Users/Robin Ochieng/OneDrive - Kenbright/Attachments/projects/2025/May/Jubilee Claims Dashboard/data/Selected_Claims_Data.xlsx", col_types = c("text", "text", "date", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "date"))

# Source custom modules
source("modules/customValueBox.R")

# Source New Business module scripts
source("modules/NewDataTableModule.R", local = TRUE)
source("modules/PremiumsDashboardModule.R", local = TRUE)

# Source Renewal Business module scripts
source("modules/RenewalDataTableModule.R", local = TRUE)
source("modules/RenewalSalesDashboardModule.R", local = TRUE)

# Source Renewal Business module scripts
source("modules/MedicalDataTableModule.R", local = TRUE)
source("modules/MedicalSalesDashboardModule.R", local = TRUE)

# Define a custom theme using bslib
my_theme <- bs_theme(
  bg = "#202123", 
  fg = "#E1E1E1", 
  primary = "#EA80FC", 
  secondary = "#00BFA5",
  base_font = font_google("Nunito"),
  heading_font = font_google("Nunito"),
  code_font = font_google("Nunito"),
  navbar_bg = "#333333", 
  navbar_fg = "#ffffff"  
)

ui <- dashboardPage(
  title = "Claims Dashboard",
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  freshTheme = my_theme,
  dashboardHeader(
    title = dashboardBrand(
      title = HTML("<div class='header-left'><strong style='font-weight: bold;'>Underwriting & Claims Dashboard</strong></div>"),
      color = "white",
      href = "https://vehicle.co.ke/"
    ),
    controlbarIcon = NULL,
    status = "white",
    sidebarIcon = NULL,
    fixed = TRUE
  ),
  sidebar = dashboardSidebar(
    skin = "light",
    tags$div(
      class = "menu-container",
    sidebarMenu(
      menuItem("Premiums", tabName = "dashboard_premiums", icon = icon("sack-dollar")),
      menuItem("Claims", tabName = "dashboard_claims", icon = icon("file-shield"))
    )),
    div(class = "sidebar-footer",
        img(src = "images/jubilee.png", class = "jubilee-logo"),
        img(src = "images/kenbright.png")
    )
  ),
  controlbar = bs4DashControlbar(
    skin = "light",
    title = "Filter Settings", 
    id = "dashboardControlbar",
    width = 200,
    bs4Card(
      width = 12,
      title = HTML(" New Business"),
      #title = HTML(paste(icon("plus-square"), " New Business")),
      solidHeader = TRUE,
      status = "secondary",
      background = "white",
      class = "bs4-card-custom",
      selectInput(inputId = "new_business_month", label = "Select Month", choices = NULL),
      selectInput(inputId = "new_business_quarter", label = "Select Quarter", choices = NULL),
      selectInput(inputId = "new_business_year", label = "Select Year", choices = NULL)
    ),
    bs4Card(
      title = HTML(paste(icon("sync"), " Renewals")),
      width = 12,
      solidHeader = TRUE,
      status = "secondary",
      background = "white",
      class = "bs4-card-custom",
      selectInput(inputId = "renewal_business_month", label = "Select Month", choices = NULL),
      selectInput(inputId = "renewal_business_quarter", label = "Select Quarter", choices = NULL),
      selectInput(inputId = "renewal_business_year", label = "Select Year", choices = NULL)
    ),
    bs4Card(
      title = HTML(paste(icon("hospital-symbol"), " Medical")),
      solidHeader = TRUE,
      status = "secondary",
      width = 12,
      background = "white",
      class = "bs4-card-custom",
      selectInput(inputId = "health_business_month", label = "Select Month", choices = NULL),
      selectInput(inputId = "health_business_quarter", label = "Select Quarter", choices = NULL),
      selectInput(inputId = "health_business_year", label = "Select Year", choices = NULL)
    )
  ),
  dashboardBody(
    tags$head(
      includeCSS("www/css/custom_styles.css"),
      #tags$link(href = "https://fonts.googleapis.com/css 2?family=Mulish:wght@400;700&display=swap", rel = "stylesheet"),
      tags$link(rel = "shortcut icon", href = "favicon/kenbright.ico", type = "image/x-icon")
    ),
    tabItems(
      tabItem(tabName = "dashboard_premiums", premiumsDashboardUI("premiums_dashboard")),
      tabItem(tabName = "dashboard_renewals", RenewalsalesDashboardUI("sales_dashboard_renewal")),
      tabItem(tabName = "dashboard_medical", MedicalsalesDashboardUI("sales_dashboard_medical")),
      tabItem(tabName = "data_table_new_business", dataTableUI("data_table")),
      tabItem(tabName = "data_table_renewal_business", RenewaldataTableUI("data_table_renewal")),
      tabItem(tabName = "data_table_medical_business", MedicaldataTableUI("data_table_medical"))  
  )
  ),
  footer = bs4DashFooter(
    div(style = "background-color: #ffffff; color: #000000; text-align: center; padding: 6px; font-size: 10px", 
        "© 2025 Claims Dashboard. All rights reserved.")
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$toggleControlbar, {
    updateBoxSidebar("dashboardControlbar")
  })


filtered_premium_data <- reactive({
  req(premium_data)
  data <- premium_data 
  data
})



  #Premium Dashboard Server Modules
  premiumsDashboardServer("premiums_dashboard", data = filtered_premium_data)#, filtered_data__new_business)
  # dataTableServer("data_table", filtered_data__new_business)

#2. Renewal Business -----------------------------------------------------------------------------------
observe({
    sales_data_renewals <- sales_data_renewals %>%
      mutate(Month = as.character(Month),  
             Month = trimws(Month),
             Year = as.numeric(as.character(Year)),
             Quarter = as.character(Quarter))
             
    # Set choices for month, quarter, and year with "All" as default option
    month_choices <- c("All" = "All", unique(sales_data_renewals$Month[!is.na(sales_data_renewals$Month)]))
    quarter_choices <- c("All" = "All", unique(sales_data_renewals$Quarter[!is.na(sales_data_renewals$Quarter)]))
    year_choices <- c("All" = "All", unique(sales_data_renewals$Year[!is.na(sales_data_renewals$Year)]))
    
    # Update select input options in the UI
    updateSelectInput(session, "renewal_business_month", choices = month_choices, selected = "All")
    updateSelectInput(session, "renewal_business_quarter", choices = quarter_choices, selected = "All")
    updateSelectInput(session, "renewal_business_year", choices = year_choices, selected = "All")
  })
  
  # Reactive expression to filter the data based on selected month, quarter, and year for renewals
  filtered_data__renewal_business <- reactive({
    req(sales_data_renewals)
    # Start with the full data and apply filters conditionally
    data <- sales_data_renewals
    if (input$renewal_business_year != "All") {
      data <- data %>% filter(Year == as.numeric(input$renewal_business_year))
    }
    if (input$renewal_business_month != "All") {
      data <- data %>% filter(Month == input$renewal_business_month)
    }
    if (input$renewal_business_quarter != "All") {
      data <- data %>% filter(Quarter == as.character(input$renewal_business_quarter))
    }
    data
  })

  #Renewal Business Server Modules
  RenewalsalesDashboardServer("sales_dashboard_renewal", filtered_data__renewal_business)
  RenewaldataTableServer("data_table_renewal", filtered_data__renewal_business)



#3. Medical Business -----------------------------------------------------------------------------------
  observe({
    sales_data_health <- sales_data_health %>%
      mutate(Month = as.character(Month),  
             Month = trimws(Month),
             Year = as.numeric(as.character(Year)),
             Quarter = as.character(Quarter))
    
    # Set choices for month, quarter, and year with "All" as default option
    month_choices <- c("All" = "All", unique(sales_data_health$Month[!is.na(sales_data_health$Month)]))
    quarter_choices <- c("All" = "All", unique(sales_data_health$Quarter[!is.na(sales_data_health$Quarter)]))
    year_choices <- c("All" = "All", unique(sales_data_health$Year[!is.na(sales_data_health$Year)]))
    
    # Update the selectInput for months and years
    updateSelectInput(session, "health_business_month", choices = month_choices, selected = "All")
    updateSelectInput(session, "health_business_quarter", choices = quarter_choices, selected = "All")                  
    updateSelectInput(session, "health_business_year", choices = year_choices, selected = "All")
  })
  
  # Reactive expression to filter the data based on selected month, quarter, and year for health business
  filtered_data__health_business <- reactive({
    req(sales_data_health)
    # Start with the full data and apply filters conditionally
    data <- sales_data_health
    if (input$health_business_year != "All") {
      data <- data %>% filter(Year == as.numeric(input$health_business_year))
    }
    if (input$health_business_month != "All") {
      data <- data %>% filter(Month == input$health_business_month)
    }
    if (input$health_business_quarter != "All") {
      data <- data %>% filter(Quarter == as.character(input$health_business_quarter))
    }
    data
  })

#Medical Business Server Modules
  MedicalsalesDashboardServer("sales_dashboard_medical", filtered_data__health_business)
  MedicaldataTableServer("data_table_medical", filtered_data__health_business)
}

# Run the application
shinyApp(ui = ui, server = server)