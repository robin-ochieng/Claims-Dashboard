# UI for Claims dashboard including graphs
claimsDashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      class = "value-box-row",
      column(
        width = 4,
        uiOutput(ns("total_paid_os"))
      ),
      column(
        width = 4,
        uiOutput(ns("total_recovery"))
      ),
      column(
        width = 4,
        uiOutput(ns("total_xol_claims"))
      )
    ),
    fluidRow(
      column(12,
        div(class = "filters-section",
            div(class = "filters-header", h5("Filter by Policy Inception Period", class = "filters-title"), actionButton(ns("reset_filters"), "Reset Filters", class = "btn-reset-filters")),
            div(class = "premium-filters-container",
                div(class = "filter-item", selectInput(ns("claims_year"), "Year", choices = NULL, selected = "Select Year")),
                div(class = "filter-item", selectInput(ns("claims_quarter"), "Quarter", choices = NULL, selected = "Select Quarter")),
                div(class = "filter-item", selectInput(ns("claims_month"), "Month", choices = NULL, selected = "Select Month"))
            )
        )
      )
    )
  )
}


# Server logic for claims dashboard
claimsDashboardServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  

    observe({
      req(data())
      updateSelectInput(session, "claims_year",
                        choices = c("Select Year", sort(unique(data()$Year))),
                        selected = "Select Year")
      quarter_order <- c("Q1", "Q2", "Q3", "Q4")
      available_quarters <- intersect(quarter_order, unique(data()$Quarter))
      updateSelectInput(session, "claims_quarter",
                        choices = c("Select Quarter", available_quarters),
                        selected = "Select Quarter")
      month_order <- month.name
      available_months <- intersect(month_order, unique(data()$Month))
      updateSelectInput(session, "claims_month",
                        choices = c("Select Month", available_months),
                        selected = "Select Month")
    })

    filtered_data <- reactive({
      df <- data()
      req(input$claims_year, input$claims_quarter, input$claims_month)
      if (input$claims_year != "Select Year") {
        df <- df %>% filter(Year == input$claims_year)
      }
      if (input$claims_quarter != "Select Quarter") {
        df <- df %>% filter(Quarter == input$claims_quarter)
      }
      if (input$claims_month != "Select Month") {
        df <- df %>% filter(Month == input$claims_month)
      }
      df
    })

    observeEvent(input$reset_filters, {
      updateSelectInput(session, "claims_year", selected = "Select Year")
      updateSelectInput(session, "claims_quarter", selected = "Select Quarter")
      updateSelectInput(session, "claims_month", selected = "Select Month")
    })

    # Total Paid + Outstanding
    output$total_paid_os <- renderUI({
      df <- filtered_data()
      total_paid_os <- sum(df$PAID_OS, na.rm = TRUE)
      customValueBox("Total Paid/OS", comma(total_paid_os), "#2980B9")
    })

    # Total Recovery
    output$total_recovery <- renderUI({
      df <- filtered_data()
      total_recovery <- sum(df$RECOVARY, na.rm = TRUE)
      customValueBox("Total Recoveries", comma(total_recovery), "#27AE60")
    })

    # Total XOL Claims
    output$total_xol_claims <- renderUI({
      df <- filtered_data()
      total_xol <- sum(df$XOL_AMOUNT, na.rm = TRUE)
      customValueBox("XOL Claims Incurred", comma(total_xol), "#E67E22")
    })

  })
}