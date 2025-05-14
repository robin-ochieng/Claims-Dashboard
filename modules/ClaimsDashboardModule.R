# UI for Claims dashboard including graphs
claimsDashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    # 📄 Print-Only Report Title and Logo
    tags$div(
      class = "print-title",
      style = "text-align: center; margin-bottom: 20px;",
      tags$img(src = "images/jubilee.png", style = "height: 60px; margin-bottom: 10px;"),
      tags$h2("Claims Dashboard Report"),
      tags$p(format(Sys.Date(), "%B %d, %Y"), style = "font-size: 14px;")
    ),
    actionButton(ns("print_dashboard"), "Print as PDF", icon = icon("print"), class = "btn btn-primary control-button"),
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
        div(class = "filters-section no-print",
            div(class = "filters-header", h5("Filter by Policy Inception Period", class = "filters-title"), actionButton(ns("reset_filters"), "Reset Filters", class = "btn-reset-filters")),
            div(class = "premium-filters-container",
                div(class = "filter-item", selectInput(ns("claims_year"), "Year", choices = NULL, selected = "Select Year")),
                div(class = "filter-item", selectInput(ns("claims_quarter"), "Quarter", choices = NULL, selected = "Select Quarter")),
                div(class = "filter-item", selectInput(ns("claims_month"), "Month", choices = NULL, selected = "Select Month"))
            )
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "Average Time to Payment by Subclass",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotOutput(ns("approval_time_boxplot")), type = 6)
        )
      ),
      column(
        width = 6,
        box(
          title = "Average Time to Payment by Branch",
          width = 12,
          status = "white",
          solidHeader = TRUE,
          collapsible = TRUE,
          withSpinner(plotOutput(ns("approval_time_by_branch")), type = 6)
        )
      )
    )


  )
}


# Server logic for claims dashboard
claimsDashboardServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  

    observeEvent(input$print_dashboard, {
      session$sendCustomMessage(type = "printPage", message = list())
    })

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

    output$approval_time_boxplot <- renderPlot({
      df <- filtered_data()

      # Ensure both dates are properly parsed
      df <- df %>%
        filter(!is.na(LOSS_DATE), !is.na(APPRV_DATE1)) %>%
        mutate(
          LOSS_DATE = as.Date(LOSS_DATE),
          APPRV_DATE1 = as.Date(APPRV_DATE1),
          DaysToApproval = as.numeric(APPRV_DATE1 - LOSS_DATE)
        ) %>%
        filter(DaysToApproval >= 0 & DaysToApproval < 365)  # Remove negative or extreme values

      # Aesthetic boxplot grouped by Subclass
      ggplot(df, aes(x = fct_reorder(SUBCLASS_NAME, DaysToApproval, .fun = median, .desc = FALSE), 
                    y = DaysToApproval, fill = SUBCLASS_NAME)) +
        geom_boxplot(outlier.color = "red", outlier.shape = 1, alpha = 0.7) +
        labs(
          title = "Distribution of Time from Loss to Payment by Subclass",
          x = "Subclass",
          y = "Days Between Loss and Paid Date"
        ) +
        theme_minimal(base_family = "Mulish") +
        theme(
          plot.title = element_text(size = 16, hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        ) +
        scale_fill_viridis_d()
    })

    output$approval_time_by_branch <- renderPlot({
      df <- filtered_data()

      df <- df %>%
        filter(!is.na(LOSS_DATE), !is.na(APPRV_DATE1)) %>%
        mutate(
          LOSS_DATE = as.Date(LOSS_DATE),
          APPRV_DATE1 = as.Date(APPRV_DATE1),
          DaysToApproval = as.numeric(APPRV_DATE1 - LOSS_DATE)
        ) %>%
        filter(DaysToApproval >= 0 & DaysToApproval < 365)

      ggplot(df, aes(x = fct_reorder(BRANCH_NAME1, DaysToApproval, .fun = median, .desc = FALSE), 
                    y = DaysToApproval, fill = BRANCH_NAME1)) +
        geom_boxplot(outlier.color = "darkred", outlier.shape = 16, alpha = 0.7) +
        labs(
          title = "Distribution of Time from Loss to Payment by Branch",
          x = "Branch",
          y = "Days Between Loss and Payment"
        ) +
        theme_minimal(base_family = "Mulish") +
        theme(
          plot.title = element_text(size = 16, hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        ) +
        scale_fill_viridis_d()
    })


  })
}