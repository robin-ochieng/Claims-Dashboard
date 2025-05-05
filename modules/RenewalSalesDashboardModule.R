# UI for sales dashboard including graphs
RenewalsalesDashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      class = "value-box-row",
      column(width = 4, uiOutput(ns("expected_renewals"))),
      column(width = 4, uiOutput(ns("actual_renewals_count"))),
      column(width = 4, uiOutput(ns("renewal_percentage")))                
    ),
    fluidRow(
        tabBox( 
          solidHeader = TRUE,
          selected = "Count by Day",
          status = "white",
          type = "tabs",
          id = ns("sales_by_day"),
          tabPanel("Sales by Day", plotlyOutput(ns("sales_by_day_renewals")) %>% withSpinner(type = 5)),
          tabPanel("Count by Day", plotlyOutput(ns("sales_count_by_day_renewals")) %>% withSpinner(type = 5))
        ),
        tabBox( 
          solidHeader = TRUE,
          selected = "Count Over Time",
          status = "white",
          type = "tabs",
          id = ns("sales_over_time"),
          tabPanel("Sales Over Time", plotlyOutput(ns("sales_over_time_renewals")) %>% withSpinner(type = 5)),
          tabPanel("Count Over Time", plotlyOutput(ns("sales_count_over_time_renewals")) %>% withSpinner(type = 5))
        ), 
      tabBox( 
          solidHeader = TRUE,
          selected = "Count of Cover Type",
          status = "white",
          type = "tabs",
          id = ns("cover_type_tabs"),
          tabPanel("Sales by Cover Type", plotOutput(ns("sales_by_Cover_type_renewals")) %>% withSpinner(type = 5)),
          tabPanel("Count of Cover Type", plotlyOutput(ns("distribution_of_cover_type_renewals")) %>% withSpinner(type = 5))
        ),
      tabBox( 
          solidHeader = TRUE,
          selected = "Count of Insurer",
          status = "white",
          type = "tabs",
          id = ns("insurer_tabs"),
          tabPanel("Sales by Insurer", plotOutput(ns("sales_by_insurer_renewals")) %>% withSpinner(type = 5)),
          tabPanel("Count of Insurer", plotOutput(ns("count_by_insurer_renewals")) %>% withSpinner(type = 5))
        ),
      tabBox( 
          solidHeader = TRUE,
          selected = "Count Renewal Status",
          status = "white",
          type = "tabs",
          height = "400px",
          id = ns("renewal_status_tabs"),
          tabPanel("Sales Renewal Status", plotlyOutput(ns("sales_by_renewal_status_renewals")) %>% withSpinner(type = 6)),
          tabPanel("Count Renewal Status", plotlyOutput(ns("count_by_renewal_status_renewals")) %>% withSpinner(type = 6))
        ),    
      box(title = "Comparison of Renewed vs Lost Renewal Sales", status = "info", solidHeader = TRUE,
          plotlyOutput(ns("lost_vs_renewed_donut_chart"))  %>% withSpinner(type = 5))
    )
  )
}





# Server logic for sales dashboard
RenewalsalesDashboardServer <- function(id, filtered_data__renewal_business) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace function to handle IDs

    # Expected Renewal Sales (KES)
    output$expected_renewals <- renderUI({
      # Calculate the expected renewal sales count (or amount if that's your metric)
      expected_value <- format(nrow(filtered_data__renewal_business()), big.mark = ",")
      
      # Call customValueBox with a title, the value, and a chosen color
      # Here we choose a professional purple tone (#8e44ad)
      customValueBox("Expected Renewal Count", expected_value, "#8e44ad")
    })

    # Count of Actual Renewals
    output$actual_renewals_count <- renderUI({
      actual_renewals_count <- filtered_data__renewal_business() %>%
        filter(`Final Comments` == "Renewed") %>%
        summarise(count_actual_renewals = n())
      value <- format(actual_renewals_count$count_actual_renewals, big.mark = ",")
      
      # Using a green tone for the count
      customValueBox("Actual Renewal Count", value, "#27ae60")
    })

    # Renewal Percentage (%)
    output$renewal_percentage <- renderUI({
      # Calculate total renewals premium sum (for renewed sales)
      sum_actual <- filtered_data__renewal_business() %>%
        filter(`Final Comments` == "Renewed") %>%
        summarize(sum_actual_renewals = sum(Premium, na.rm = TRUE)) %>%
        pull(sum_actual_renewals)
      
      # Get the total Premium value across all renewals
      total_premium <- sum(filtered_data__renewal_business()$Premium, na.rm = TRUE)
      # Compute the percentage
      renewal_percentage <- sum_actual / total_premium * 100
      
      value <- paste0(format(renewal_percentage, digits = 2), "%")
      
      # Use a red tone for the percentage value
      customValueBox("Renewal Percentage (%)", value, "#2176C7")
    })
  
  output$sales_by_day_renewals <- renderPlotly({
    data <- filtered_data__renewal_business() %>%
      filter(`Final Comments` == "Renewed") %>%
      mutate(DayOfWeek = lubridate::wday(Renewal_Date, label = TRUE)) %>%
      group_by(DayOfWeek) %>%
      summarize(TotalSales = sum(Premium, na.rm = TRUE)) %>%
      mutate(TotalSalesMillions = TotalSales / 1e6) %>%
      arrange(DayOfWeek)
    plot_ly(data, x = ~DayOfWeek, y = ~TotalSales, type = 'bar',
            text = ~paste(scales::comma(TotalSalesMillions, accuracy = 0.01), "M"),
            textposition = 'outside',
            hoverinfo = 'text',
            textfont = list(size = 9, color = "black"),
            marker = list(color = '#1CA4F8')) %>%
      layout(
        title = "Sales by day of the week",
        xaxis = list(title = "Day of the Week", tickfont = list(size = 10, color = "#333333")),
        yaxis = list(title = "Total Sales", tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      ) 
  })

  output$sales_count_by_day_renewals <- renderPlotly({
    data <- filtered_data__renewal_business() %>%
      filter(`Final Comments` == "Renewed") %>%
      mutate(DayOfWeek = lubridate::wday(Renewal_Date, label = TRUE)) %>%
      group_by(DayOfWeek) %>%
      dplyr::summarize(Count = n()) %>%
      arrange(DayOfWeek)
    plot_ly(data, x = ~DayOfWeek, y = ~Count, type = 'bar',
            text = ~Count,
            textposition = 'outside',
            hoverinfo = 'text',
            textfont = list(size = 9, color = "black"),
            marker = list(color = '#1CA4F8')) %>%
      layout(
        title = "Count of Renewals by Day of the Week",
        xaxis = list(title = "Day of the Week", tickfont = list(size = 10, color = "#333333")),
        yaxis = list(title = "Count of Renewals", tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      ) 
  })
        
  
  output$sales_over_time_renewals <- renderPlotly({
    data <- filtered_data__renewal_business()%>%
      filter(`Final Comments` == "Renewed") %>%
      mutate(Renewal_Date = as.Date(Renewal_Date, format = "%Y-%m-%d")) %>% # Adjust format as necessary
      arrange(Renewal_Date)
    plot_ly(data, x = ~Renewal_Date, y = ~Premium, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#1CA4F8'), marker = list(color = '#0d6efd')) %>%
      layout(
        title = "Sales by Renewal Date",
        xaxis = list(title = "Renewal_Date", tickfont = list(size = 10, color = "#333333")),
        yaxis = list(title = "Premium", tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })

  output$sales_count_over_time_renewals <- renderPlotly({
    data <- filtered_data__renewal_business() %>%
      filter(`Final Comments` == "Renewed") %>%
      mutate(Renewal_Date = as.Date(Renewal_Date, format = "%Y-%m-%d")) %>%
      group_by(Renewal_Date) %>%
      dplyr::summarize(Count = n()) %>%
      arrange(Renewal_Date)

    plot_ly(data, x = ~Renewal_Date, y = ~Count, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#1CA4F8'), marker = list(color = '#0d6efd')) %>%
      layout(
        title = "Count of Renewals by Renewal Date",
        xaxis = list(title = "Renewal Date", tickfont = list(size = 10, color = "#333333")),
        yaxis = list(title = "Count of Renewals", tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })

  
  output$sales_by_renewal_status_renewals <- renderPlotly({
    # Process the data
    data <- filtered_data__renewal_business() %>%
      filter(!is.na(`Final Comments`)) %>%
      group_by(`Final Comments`) %>%
      summarize(TotalSales = sum(Premium, na.rm = TRUE)) %>%
      mutate(TotalSalesMillions = TotalSales / 1e6) %>%
      arrange(desc(TotalSalesMillions))
    # Factor with levels in descending order to show the highest sales at the top
    data$`Final Comments` <- factor(data$`Final Comments`, levels = rev(data$`Final Comments`))
    # Creating the Plotly plot
    plot <- plot_ly(
      data,
      x = ~TotalSalesMillions,
      y = ~`Final Comments`,
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#007bff')
    ) %>%
      layout(
        xaxis = list(title = 'Total Sales (Millions KES)', tickformat = ',.2f'),
        yaxis = list(title = 'Renewal Status'), 
        showlegend = FALSE,
        margin = list(t = 10, r = 20, b = 10, l = 10),
        annotations = lapply(1:nrow(data), function(i) {
          list(
            x = data$TotalSalesMillions[i] + 0.05,
            y = data$`Final Comments`[i],
            text = paste(comma(data$TotalSalesMillions[i], accuracy = 0.01), "M"),
            showarrow = FALSE,
            xanchor = 'left',
            yanchor = 'middle',
            font = list(color = 'black', size = 12)
           )
        })
      )
    plot
  })
  
custom_colors3 <- c("#1E90FF", "#4682B4", "#87CEEB", "#5bc0de", "#ff7f0e", "#1f77b4", "#2ca02c", "#d62728", "#9467bd")
output$lost_vs_renewed_donut_chart <- renderPlotly({
  data <- filtered_data__renewal_business() %>%
    filter(!is.na(Premium))
  # Create a new column for Renewed vs Unrenewed
  data$Renewal_Status <- ifelse(data$`Final Comments` == "Renewed", "Renewed", "Unrenewed")
  # Aggregate the premium by the new Renewal_Status column
  premium_by_status <- aggregate(Premium ~ Renewal_Status, data, sum)
  premium_by_status$Premium <- round(premium_by_status$Premium, 0)  # Round to no decimal points
  premium_by_status$percentage <- round(premium_by_status$Premium / sum(premium_by_status$Premium) * 100, 0)
  colors <- custom_colors3
  p <- plot_ly(premium_by_status, labels = ~Renewal_Status, values = ~Premium, type = 'pie', hole = 0.4,
               textposition = 'inside', 
               textinfo = 'label+value+percent',
               insidetextorientation = 'tangential',
               marker = list(colors = colors), 
               textfont = list(color = 'white', family = "Mulish", size = 12))
  p <- p %>% layout(showlegend = TRUE,
                    font = list(family = "Mulish"))
  p
})

  output$sales_by_Cover_type_renewals <- renderPlot({
    data <- filtered_data__renewal_business() %>%
      filter(`Final Comments` == "Renewed") %>%
      filter(!is.na(`Cover_Type`)) %>%
      group_by(Cover_Type) %>%
      summarize(TotalSales = sum(Premium, na.rm = TRUE)) %>%
      mutate(TotalSalesMillions = TotalSales / 1e6) %>%
      arrange(desc(TotalSalesMillions))
    
    ggplot(data, aes(x = reorder(Cover_Type, -TotalSalesMillions), y = TotalSalesMillions, fill = TotalSalesMillions)) +
      geom_col(show.legend = FALSE, color = "#333333", size = 0.2) +  # Black border for bars
      geom_text(aes(label = sprintf("%.3f M", TotalSalesMillions)), vjust = -0.3, size = 3.5, color = "#000000") +
      scale_fill_gradient(low = "#48d1cc", high = "#48d1cc") +  # Blue gradient fill
      theme_minimal(base_family = "Mulish") +
      theme(
        text = element_text(family = "Mulish"),  # Modern, clean font
        axis.title = element_text(size = 12, color = "#333333"),
        axis.text = element_text(size = 10, color = "#333333"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),  # White background for clean look
        plot.background = element_rect(fill = "#f8f9fa", colour = NA),  # Match panel background
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()
      ) +
      labs(
        title = "",
        y = "Total Sales (Million KES)",
        x = "Cover Type"
      ) +
      scale_y_continuous(labels = scales::comma)
  })
  
  custom_colors4 <- c("#4ac4b5", "#34897f", "#00acc1", "#ff7f0e", "#1f77b4", "#9467bd","#17a2b8", "#d62728")
  output$distribution_of_cover_type_renewals <- renderPlotly({
    # Load data and filter relevant columns and rows
    data <- filtered_data__renewal_business() %>%
      filter(!is.na(`Cover_Type`)) %>%
      filter(`Final Comments` == "Renewed") %>%
      group_by(Cover_Type) %>%
      summarise(Count = n()) %>%
      mutate(percentage = Count / sum(Count) * 100) %>%
      ungroup()
    # Generate color palette
    num_categories <- n_distinct(data$Cover_Type)
    colors <- custom_colors4
    # Create the donut chart
    p <- plot_ly(data, labels = ~Cover_Type, values = ~Count, type = 'pie', 
                 textposition = 'inside',  # Set labels outside
                 textinfo = 'label+value+percent',
                 insidetextorientation = 'radial',
                 marker = list(colors = colors),
                 textfont = list(color = 'white', family = "Mulish", size = 10))
    p <- p %>%  layout(title = "",
                       showlegend = TRUE,
                       font = list(family = "Mulish", size = 7))
    
    p
  })
  
  output$sales_by_insurer_renewals <- renderPlot({
    data <- filtered_data__renewal_business() %>%
      filter(`Final Comments` == "Renewed") %>% 
      filter(!is.na(`Underwriter`), !is.na(`Underwriter`)) %>%
      group_by(`Underwriter`) %>%
      summarize(TotalSales = sum(Premium, na.rm = TRUE)) %>%
      mutate(TotalSalesMillions = TotalSales / 1e6) %>%
      arrange(desc(TotalSalesMillions))
    insurers_ordered <- factor(data$Underwriter, levels = rev(data$Underwriter))
    # Creating the lollipop plot
    ggplot(data, aes(x = insurers_ordered, y = TotalSalesMillions)) +
      geom_segment(aes(xend = insurers_ordered, yend = 0), color = "#0d6efd", size = 1) +  # Lines for lollipop sticks
      geom_point(color = "#198754", size = 3) +  # Points as lollipop heads
      geom_text(aes(label = paste(comma(TotalSalesMillions, accuracy = 0.01), "M"),
                    y = TotalSalesMillions + 0.02 * max(data$TotalSalesMillions)), 
                hjust = -0.05, vjust = 0.5, color = "black", size = 3.0) +
      coord_flip() +
      theme_minimal() +
      theme(
        text = element_text(family = "Mulish", face = "plain"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "lightgrey", size = 0.5), # Add vertical major grid lines
        panel.grid.minor.x = element_line(color = "lightgrey", size = 0.25),
        axis.title.x = element_text(angle = 0, hjust = 0.5),
        axis.title.y = element_text(angle = 90, vjust = 0.5),
        axis.text.y = element_text(color = "black"),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 3, unit = "pt")
      ) +
      labs(
        x = "Insurer",
        y = "Total Sales (Millions KES)",
        title = "",
        subtitle = "",
        caption = "Data Source: E.M."
      )
  })
  
  output$count_by_insurer_renewals <- renderPlot({
  data <- filtered_data__renewal_business() %>%
    filter(`Final Comments` == "Renewed") %>% 
    filter(!is.na(`Underwriter`), !is.na(`Underwriter`)) %>%
    group_by(`Underwriter`) %>%
    dplyr::summarize(TotalRenewals = n()) %>% 
    arrange(desc(TotalRenewals))
  insurers_ordered <- factor(data$Underwriter, levels = rev(data$Underwriter))
  # Creating the lollipop plot
  ggplot(data, aes(x = insurers_ordered, y = TotalRenewals)) +
    geom_segment(aes(xend = insurers_ordered, yend = 0), color = "#0d6efd", size = 0.7) +  # Lines for lollipop sticks
    geom_point(color = "#00acc1", size = 2.5) +  # Points as lollipop heads
    geom_text(aes(label = TotalRenewals,
                  y = TotalRenewals + 0.02 * max(TotalRenewals)), 
              hjust = -0.05, vjust = 0.5, color = "black", size = 3.0) +
    coord_flip() +
    theme_minimal() +
    theme(
      text = element_text(family = "Mulish", face = "plain"),
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "lightgrey", size = 0.5), # Add vertical major grid lines
      panel.grid.minor.x = element_line(color = "lightgrey", size = 0.25),
      axis.title.x = element_text(angle = 0, hjust = 0.5),
      axis.title.y = element_text(angle = 90, vjust = 0.5),
      axis.text.y = element_text(color = "black"),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 3, unit = "pt")
    ) +
    labs(
      x = "Insurer",
      y = "Number of Renewals",
      title = ""
    )
})

  
  
  output$count_by_renewal_status_renewals <- renderPlotly({
    # Process the data
    data <- filtered_data__renewal_business() %>%
      filter(!is.na(`Final Comments`)) %>%
      group_by(`Final Comments`) %>%
      dplyr::summarize(TotalCounts = n()) %>%
      arrange(desc(TotalCounts))
    # Factor with levels in descending order to show the highest counts at the top
    data$`Final Comments` <- factor(data$`Final Comments`, levels = data$`Final Comments`)
    # Creating the Plotly plot
    plot <- plot_ly(
      data,
      x = ~TotalCounts,
      y = ~`Final Comments`,
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#48d1cc')  # Set the bar color to Bootstrap's success color
    ) %>%
      layout(
        xaxis = list(title = 'Count', tickformat = ','),
        yaxis = list(title = 'Renewal Status Count', title_standoff = 20),  # Add title_standoff to create space between title and ticks
        showlegend = FALSE,
        margin = list(t = 20, r = 25, b = 10, l = 25),
        annotations = lapply(1:nrow(data), function(i) {
          list(
            x = data$TotalCounts[i] + 0.05,
            y = data$`Final Comments`[i],
            text = comma(data$TotalCounts[i]),
            showarrow = FALSE,
            xanchor = 'left',
            yanchor = 'middle',
            font = list(color = 'black', size = 12)
          )
        }),
        font = list(family = "Mulish", size = 12)
      ) %>%
      layout(
        margin = list(t = 20, r = 25, b = 10, l = 25),  # Adds border around the plot
        xaxis = list(title = "Count"),
        yaxis = list(title = "Renewal Status Count")
      )
    plot
  })
  
  })
}
