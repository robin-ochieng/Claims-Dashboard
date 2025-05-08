# UI for sales dashboard including graphs
premiumsDashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      class = "value-box-row",
      column(
        width = 4,
        uiOutput(ns("total_gross_premium"))
      ),
      column(
        width = 4,
        uiOutput(ns("total_commission_expense"))
      ),
      column(
        width = 4,
        uiOutput(ns("total_reinsurance_ceded"))
      )
    ), 
    fluidRow(
      column(
        12,
        tags$div(
          class = "filters-section",
          tags$h5("Filter by Policy Inception Period", class = "filters-title"),  # Title line
          div(
            class = "premium-filters-container",
            div(class = "filter-item",
                selectInput(ns("premium_year"), "Year", choices = NULL, selected = "All")
            ),
            div(class = "filter-item",
                selectInput(ns("premium_quarter"), "Quarter", choices = NULL, selected = "All")
            ),
            div(class = "filter-item",
                selectInput(ns("premium_month"), "Month", choices = NULL, selected = "All")
            )
          )
        )
      )
    ),
    fluidRow( 
        bs4Card(
          title = "Premium by Class",
          solidHeader = TRUE,
          status = "white",
          width = 6,
          plotlyOutput(ns("premium_by_class")) %>% withSpinner(type = 6)
        ),
        bs4Card(
          title = "Policy Count by Class",
          solidHeader = TRUE,
          status = "white",
          width = 6,
          plotlyOutput(ns("count_by_class")) %>% withSpinner(type = 6)
        ),   
        bs4Card(
          title = "Premium by Customer Category",
          solidHeader = TRUE,
          status = "white",
          width = 6,
          plotOutput(ns("premium_by_customer_category")) %>% withSpinner(type = 6)
        ),
        bs4Card(
          title = "Policy Count by Customer Category",
          solidHeader = TRUE,
          status = "white",
          width = 6,
          plotOutput(ns("count_by_customer_category")) %>% withSpinner(type = 6)
        )
    )
  )
}


# Server logic for sales dashboard
premiumsDashboardServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace function to handle IDs

    observe({
      req(data())
      updateSelectInput(session, "premium_year",
                        choices = c("All", sort(unique(data()$Year))),
                        selected = "All")
      quarter_order <- c("Q1", "Q2", "Q3", "Q4")
      available_quarters <- intersect(quarter_order, unique(data()$Quarter))
      updateSelectInput(session, "premium_quarter",
                        choices = c("All", available_quarters),
                        selected = "All")
      month_order <- month.name
      available_months <- intersect(month_order, unique(data()$Month))
      updateSelectInput(session, "premium_month",
                        choices = c("All", available_months),
                        selected = "All")
    })

    filtered_data <- reactive({
      df <- data()
      if (input$premium_year != "All") {
        df <- df %>% filter(Year == input$premium_year)
      }
      if (input$premium_quarter != "All") {
        df <- df %>% filter(Quarter == input$premium_quarter)
      }
      if (input$premium_month != "All") {
        df <- df %>% filter(Month == input$premium_month)
      }
      df
    })

    # Total Gross Premium
    output$total_gross_premium <- renderUI({
      df <- filtered_data()
      total <- sum(df$BASE_PREMIUM, na.rm = TRUE)
      customValueBox("Total Gross Premium", comma(total), "#2176C7")
    })

    # Total Commission Expense (Broker + QS + Surplus + FAC)
    output$total_commission_expense <- renderUI({
      df <- filtered_data()
      total_comm <- sum(df$BROKER_COMM, df$QS_COMM, df$SURPLUS_01_COMM, df$FAC_COMM, na.rm = TRUE)
      customValueBox("Commission Expense", comma(total_comm), "#27ae60")
    })

    # Total Reinsurance Ceded (BASE_PREMIUM - RETN)
    output$total_reinsurance_ceded <- renderUI({
      df <- filtered_data()
      ceded <- sum(df$BASE_PREMIUM - df$RETN, na.rm = TRUE)
      customValueBox("Reinsurance Ceded", comma(ceded), "#F39C12")
    })
    
    # Total Gross Premium by CLASS_DESCRIPTION
    output$premium_by_class <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(SUB_CLASSNAME)) %>%
        group_by(SUB_CLASSNAME) %>%
        summarise(TotalPremium = sum(BASE_PREMIUM, na.rm = TRUE)) %>%
        arrange(desc(TotalPremium)) %>%
        mutate(TotalPremiumMillions = TotalPremium / 1e6)

      plot_ly(df, x = ~fct_reorder(SUB_CLASSNAME, -TotalPremium), y = ~TotalPremium, type = 'bar',
              text = ~paste0(formatC(TotalPremiumMillions, format = "f", digits = 0, big.mark = ","), " M"),
              textfont = list(size = 9, color = "black"),
              textposition = 'outside',
              hoverinfo = 'text',
              hovertext = ~paste("Class:", SUB_CLASSNAME, "<br>Total Premium:", scales::comma(TotalPremium), "KES"),
              marker = list(color = '#00BFA5')) %>%
        layout(
          title = list(
            text = "Gross Premium by Class",
            uniformtext = list(minsize = 11, mode = 'show'), 
            x = 0.01,  # left-align title
            xanchor = "left",
            font = list(size = 14)
          ),
          margin = list(b = 100), 
          xaxis = list(title = "Class", tickangle = -45, tickfont = list(size = 10)),
          yaxis = list(title = "Total Premium(Millions KES)", tickfont = list(size = 10)),
          font = list(family = "Mulish"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    # Policy Count by SUB_CLASSNAMEN
    output$count_by_class <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(SUB_CLASSNAME)) %>%
        group_by(SUB_CLASSNAME) %>%
        summarise(PolicyCount = n()) %>%
        arrange(desc(PolicyCount))

      plot_ly(df, x = ~fct_reorder(SUB_CLASSNAME, -PolicyCount), y = ~PolicyCount, type = 'bar',
              text = ~formatC(PolicyCount, format = "d", big.mark = ","),
              textposition = 'outside',
              hoverinfo = 'text',
              hovertext = ~paste("Class:", SUB_CLASSNAME, "<br>Policies:", formatC(PolicyCount, format = "d", big.mark = ",")),
              marker = list(color = '#EA80FC')) %>%
        layout(
          title = list(
            text = "Policy Count by Class",
            uniformtext = list(minsize = 11, mode = 'show'), 
            x = 0.01,  # left-align title
            xanchor = "left",
            font = list(size = 14)
          ),
          margin = list(b = 100), 
          xaxis = list(title = "Class", tickangle = -45, tickfont = list(size = 10)),
          yaxis = list(title = "Count", tickfont = list(size = 10)),
          font = list(family = "Mulish"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    output$premium_by_customer_category <- renderPlot({
      data <- filtered_data() %>%
        filter(!is.na(CUSTOMER_CATEGORY)) %>%
        group_by(CUSTOMER_CATEGORY) %>%
        summarise(TotalPremium = sum(BASE_PREMIUM, na.rm = TRUE)) %>%
        mutate(TotalPremiumMillions = TotalPremium / 1e6) %>%
        arrange(desc(TotalPremiumMillions))
      
      cat_order <- factor(data$CUSTOMER_CATEGORY, levels = rev(data$CUSTOMER_CATEGORY))

      ggplot(data, aes(x = cat_order, y = TotalPremiumMillions)) +
        geom_segment(aes(xend = cat_order, yend = 0), color = "#0d6efd", size = 1) +
        geom_point(color = "#198754", size = 3) +
        geom_text(aes(label = paste0(comma(TotalPremiumMillions, accuracy = 0.01), " M"),
                      y = TotalPremiumMillions + 0.02 * max(TotalPremiumMillions)),
                  hjust = -0.05, vjust = 0.5, color = "black", size = 3) +
        coord_flip(clip = "off") +
        theme_minimal() +
        theme(
          text = element_text(family = "Mulish"),
          legend.position = "none",
          plot.margin = margin(t = 10, r = 50, b = 10, l = 10, unit = "pt"), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "lightgrey", size = 0.5),
          panel.grid.minor.x = element_line(color = "lightgrey", size = 0.25),
          axis.title.x = element_text(),
          axis.title.y = element_text(),
          axis.text.y = element_text(color = "black"),
          plot.title.position = "plot",  # Ensures title aligns relative to plot area
          plot.title = element_text(hjust = 0, size = 14)
        ) +
        labs(
          x = "Customer Category",
          y = "Total Premium (Millions KES)",
          title = "Premium by Customer Category"
        )
    })

    output$count_by_customer_category <- renderPlot({
      data <- filtered_data() %>%
        filter(!is.na(CUSTOMER_CATEGORY)) %>%
        count(CUSTOMER_CATEGORY) %>%
        arrange(desc(n))
      
      cat_order <- factor(data$CUSTOMER_CATEGORY, levels = rev(data$CUSTOMER_CATEGORY))

      ggplot(data, aes(x = cat_order, y = n)) +
        geom_segment(aes(xend = cat_order, yend = 0), color = "#6c5ce7", size = 1) +
        geom_point(color = "#00b894", size = 3) +
        geom_text(aes(label = comma(n),
                      y = n + 0.02 * max(n)),
                  hjust = -0.05, vjust = 0.5, color = "black", size = 3) +
        coord_flip(clip = "off") +
        theme_minimal() +
        theme(
          text = element_text(family = "Mulish"),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "lightgrey", size = 0.5),
          panel.grid.minor.x = element_line(color = "lightgrey", size = 0.25),
          axis.title.x = element_text(),
          axis.title.y = element_text(),
          axis.text.y = element_text(color = "black"),
          plot.title = element_text(hjust = 0, size = 14),
          plot.title.position = "plot",
          plot.margin = margin(t = 10, r = 50, b = 10, l = 10, unit = "pt"), 
        ) +
        labs(
          x = "Customer Category",
          y = "Policy Count",
          title = "Count by Customer Category"
        )
    })



  })
}
