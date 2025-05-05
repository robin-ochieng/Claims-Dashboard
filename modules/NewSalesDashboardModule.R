# UI for sales dashboard including graphs
salesDashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      class = "value-box-row",
      column(
        width = 4,
        uiOutput(ns("total_sales"))
      ),
      column(
        width = 4,
        uiOutput(ns("avg_sales_day"))
      ),
      column(
        width = 4,
        uiOutput(ns("sales_by_insurer_"))
      )
    ), 
    fluidRow(  
        tabBox( 
          solidHeader = TRUE,
          selected = "Count Over Time",
          status = "white",
          type = "tabs",
          id = ns("insurer_tabs"),
          tabPanel("Sales Over Time", plotlyOutput(ns("sales_over_time")) %>% withSpinner(type = 6)),
          tabPanel("Count Over Time", plotlyOutput(ns("salesCount_over_time")) %>% withSpinner(type = 6))
        ),
        tabBox( 
          solidHeader = TRUE,
          selected = "Count by Day",
          status = "white",
          type = "tabs",
          id = ns("insurer_tabs"),
          tabPanel("Sales by Day", plotlyOutput(ns("sales_by_day")) %>% withSpinner(type = 6)),
          tabPanel("Count by Day", plotlyOutput(ns("count_sales_by_day")) %>% withSpinner(type = 6))
        ),
        tabBox( 
          solidHeader = TRUE,
          selected = "Count Heatmap",
          status = "white",
          type = "tabs",
          id = ns("insurer_tabs"),
          tabPanel("Sales Heatmap", plotlyOutput(ns("calendar_plot")) %>% withSpinner(type = 6)),
          tabPanel("Count Heatmap", plotlyOutput(ns("calendar_plot_count")) %>% withSpinner(type = 6))
        ),
      tabBox(  
          solidHeader = TRUE,
          selected = "Count by Insurer",
          status = "white",
          type = "tabs",
          id = ns("insurer_tabs"),
          tabPanel("Sales by Insurer", plotlyOutput(ns("sales_by_insurer")) %>% withSpinner(type = 6)),
          tabPanel("Count by Insurer", plotlyOutput(ns("salescount_by_insurer")) %>% withSpinner(type = 6))
        ),
      tabBox( 
          solidHeader = TRUE,
          selected = "Count by Time of Day",
          status = "white",
          type = "tabs",
          id = ns("timeofday_tabs"),
          tabPanel("Sales by Time of Day", plotlyOutput(ns("sales_by_time_of_day")) %>% withSpinner(type = 6)),
          tabPanel("Count by Time of Day", plotlyOutput(ns("salescount_by_time_of_day")) %>% withSpinner(type = 6))
        ),
      tabBox( 
          solidHeader = TRUE,
          selected = "Sales by Cover Type",
          status = "white",
          type = "tabs",
          id = ns("covertype_tabs"),
          tabPanel("Count by Cover Type", plotlyOutput(ns("salescount_by_cover_type")) %>% withSpinner(type = 6)),
          tabPanel("Sales by Cover Type", plotlyOutput(ns("sales_by_cover_type")) %>% withSpinner(type = 6))
        ),
      tabBox( 
          solidHeader = TRUE,
          selected = "Count by SalesPerson",
          status = "white",
          type = "tabs",
          id = ns("sales_tabs"),
          tabPanel("Sales by SalesPerson", plotlyOutput(ns("sales_by_person")) %>% withSpinner(type = 6)),
          tabPanel("Count by SalesPerson", plotOutput(ns("sales_count_by_person")) %>% withSpinner(type = 6))
        ),
      tabBox(
          solidHeader = TRUE,
          selected = "Count by Source",
          type = "tabs",
          status = "white",
          id = ns("count_tabs"),
          tabPanel("Sales by Source", plotlyOutput(ns("sales_by_source")) %>% withSpinner(type = 6)),
          tabPanel("Count by Source", plotOutput(ns("sales_count_by_source")) %>% withSpinner(type = 6))
        )
    )
  )
}


# Server logic for sales dashboard
salesDashboardServer <- function(id, filtered_data__new_business, new_business_month) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace function to handle IDs

    # Total Sales Card
    output$total_sales <- renderUI({
      total_sales <- nrow(filtered_data__new_business())
      # You can choose a professional color for the card (e.g., a blue shade)
      customValueBox("Count of Sales", formatC(total_sales, format = "d", big.mark = ","), "#2176C7")
    })

    # Average Sales per Day Card
    output$avg_sales_day <- renderUI({
      avg_count_per_day <- filtered_data__new_business() %>%
        group_by(Date) %>%
        summarize(DailyCount = n()) %>%
        summarise(AverageCount = mean(DailyCount)) 
      
      value_to_show <- format(round(avg_count_per_day$AverageCount, 0), big.mark = ",")
      # Choose a different color (e.g., a green shade)
      customValueBox("Average Sales Per Day", value_to_show, "#27ae60")
    })

    # Top Insurer Card
    output$sales_by_insurer_ <- renderUI({
      insurer_counts <- filtered_data__new_business() %>%
        count(Insurer) %>%
        arrange(desc(n)) %>%
        slice(1)
      
      value_text <- paste(insurer_counts$Insurer, ":", insurer_counts$n)
      # And another color (e.g., a red tone)
      customValueBox("Top Insurer by Count", value_text, "#858484")
    })

    # Plot for sales by day of the week
    output$sales_by_day <- renderPlotly({
      data <- filtered_data__new_business() %>%
        mutate(DayOfWeek = lubridate::wday(Date, label = TRUE)) %>%
        group_by(DayOfWeek) %>%
        summarize(TotalSales = sum(Sales, na.rm = TRUE)) %>%
        mutate(TotalSalesMillions = TotalSales / 1e6) %>%
        arrange(DayOfWeek)

      plot_ly(data, x = ~DayOfWeek, y = ~TotalSales, type = 'bar',
              text = ~paste(scales::comma(TotalSalesMillions, accuracy = 0.01), "M"),
              textposition = 'outside',
              hoverinfo = 'text',
              textfont = list(size = 9, color = "black"),
              marker = list(color = '#1CA4F8')) %>%
        layout(
          title = "Sales by Day of the Week",
          xaxis = list(title = "Day of the Week", tickfont = list(size = 10, color = "#333333")),
          yaxis = list(title = "Total Sales", tickfont = list(size = 10, color = "#333333")),
          font = list(family = "Mulish", color = "#333333"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    # Plot for sales by day of the week
    output$count_sales_by_day <- renderPlotly({
      data <- filtered_data__new_business() %>%
        mutate(DayOfWeek = lubridate::wday(Date, label = TRUE)) %>%
        group_by(DayOfWeek) %>%
        summarize(CountSales = n()) %>%
        arrange(DayOfWeek)
      plot_ly(data, x = ~DayOfWeek, y = ~CountSales, type = 'bar',
              text = ~CountSales,
              textposition = 'outside',
              hoverinfo = 'text',
              textfont = list(size = 9, color = "black"),
              marker = list(color = '#1CA4F8')) %>%
        layout(
          title = "Count of Sales by Day of the Week",
          xaxis = list(title = "Day of the Week", tickfont = list(size = 10, color = "#333333")),
          yaxis = list(title = "Total Sales", tickfont = list(size = 10, color = "#333333")),
          font = list(family = "Mulish", color = "#333333"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })

    output$salesCount_over_time <- renderPlotly({
      data <- filtered_data__new_business() %>%
        mutate(Date = as.Date(Date)) %>%
        group_by(Date) %>%
        summarize(SalesCount = n()) %>%  # Count of sales per day
        arrange(Date)
      
      plot_ly(data, x = ~Date, y = ~SalesCount, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#1CA4F8'), marker = list(color = '#0d6efd')) %>%
        layout(
          title = "Sales Count Over Time",
          xaxis = list(title = "Date", tickfont = list(size = 10, color = "#333333")),
          yaxis = list(title = "Count of Sales", tickfont = list(size = 10, color = "#333333")),
          font = list(family = "Mulish", color = "#333333"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })


    output$sales_over_time <- renderPlotly({
    data <- filtered_data__new_business() %>%
      mutate(Date = as.Date(Date)) %>% # Ensure Date is in Date format
      arrange(Date)
    plot_ly(data, x = ~Date, y = ~Sales, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#1CA4F8'), marker = list(color = '#0d6efd')) %>%
      layout(
        title = "Sales Over Time",
        xaxis = list(title = "Date", tickfont = list(size = 10, color = "#333333")),
        yaxis = list(title = "Sales", tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  
  output$sales_by_insurer <- renderPlotly({
    data <- filtered_data__new_business() %>%
      filter(!is.na(Insurer)) %>%
      group_by(Insurer) %>%
      summarize(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      mutate(TotalSalesMillions = TotalSales / 1e6) %>%
      arrange(-TotalSalesMillions)  # Ensuring data is sorted in descending order
    # Resetting the factor levels based on descending order of TotalSalesMillions
    data$Insurer <- factor(data$Insurer, levels = data$Insurer)
    # Create the plot
    p <- plot_ly(data, x = ~TotalSalesMillions, y = ~Insurer, type = 'bar', orientation = 'h',
                 text = ~paste(scales::comma(TotalSalesMillions, accuracy = 0.1), "M"),
                 textposition = 'outside',
                 hoverinfo = 'text',
                 textfont = list(size = 8, color = "black"),
                 marker = list(color = '#99CCFF')) %>%
      layout(
        yaxis = list(title = "Insurer", automargin = TRUE, showgrid = FALSE, zeroline = FALSE),
        xaxis = list(title = "Total Sales (Millions KES)", showgrid = FALSE, zeroline = FALSE),
        title = "Sum of Sales by Insurer",
        margin = list(l = 100, r = 25, t = 25, b = 70), # Adjust margins to fit labels
        hoverlabel = list(bgcolor = "white"),
        font = list(family = "Mulish", size = 10), # Apply global font styling
        paper_bgcolor = 'rgba(0,0,0,0)', # Transparent background
        plot_bgcolor = 'rgba(0,0,0,0)' # Transparent background
      )
    
    return(p)
  })

  output$salescount_by_insurer <- renderPlotly({
    data <- filtered_data__new_business() %>%
      filter(!is.na(Insurer)) %>%
      group_by(Insurer) %>%
      summarize(Count = n()) %>%
      arrange(-Count)  # Ensuring data is sorted in descending order of count
    # Resetting the factor levels based on descending order of counts
    data$Insurer <- factor(data$Insurer, levels = data$Insurer)
    
    # Create the plot
    p <- plot_ly(data, x = ~Count, y = ~Insurer, type = 'bar', orientation = 'h',
                text = ~Count,
                textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(size = 8, color = "black"),
                marker = list(color = '#17a2b8')) %>%
      layout(
        yaxis = list(title = "Insurer", automargin = TRUE, showgrid = FALSE, zeroline = FALSE),
        xaxis = list(title = "Number of Transactions", showgrid = FALSE, zeroline = FALSE),
        title = "Distribution of Sales Transactions by Insurer",
        margin = list(l = 100, r = 25, t = 25, b = 70), # Adjust margins to fit labels
        hoverlabel = list(bgcolor = "white"),
        font = list(family = "Mulish", size = 10), # Apply global font styling
        paper_bgcolor = 'rgba(0,0,0,0)', # Transparent background
        plot_bgcolor = 'rgba(0,0,0,0)' # Transparent background
      )
    return(p)
  })


  
  output$sales_by_person <- renderPlotly({
    data <- filtered_data__new_business() %>%
      filter(!is.na(SalesPerson)) %>%
      group_by(SalesPerson) %>%
      summarize(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      mutate(TotalSalesMillions = TotalSales / 1e6) %>%
      mutate(SalesPerson = fct_reorder(SalesPerson, TotalSalesMillions)) %>%  # Reorder factor levels
      arrange((TotalSalesMillions))
    
    plot_ly(data, x = ~TotalSalesMillions, y = ~SalesPerson, type = 'bar', orientation = 'h',
            marker = list(color = '#0d6efd')) %>%
      layout(
        title = "",
        yaxis = list(title = "SalesPerson", tickangle = 0, tickfont = list(size = 10, color = "#333333")),
        xaxis = list(title = "Total Sales (Million KES)", tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        margin = list(b = 100), # To accommodate rotated x-axis labels
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      ) %>%
      add_annotations(
        x = ~TotalSalesMillions,
        y = ~SalesPerson,
        text = ~sprintf("%.2f M", TotalSalesMillions),
        showarrow = FALSE,
        xshift = 15,
        font = list(size = 9, color = "#333333")
      )
  })
  
custom_colors <- c("#87CEFA", "#6495ED", "#4682B4", "#006064", "#007bff", "#9467bd", "#ff7f0e", "#1f77b4", "#d62728")

output$sales_by_cover_type <- renderPlotly({
  # Load and filter the data
  data <- filtered_data__new_business()
  data <- data[!is.na(data$Sales) & !is.na(data$`Type of Cover`), ]
  
  # Aggregate sales by cover type and order by descending sales
  sales_by_cover <- data %>%
    group_by(`Type of Cover`) %>%
    summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(Sales))
  
  # Calculate total sales and percentage share for each cover type
  total_sales <- sum(sales_by_cover$Sales)
  sales_by_cover <- sales_by_cover %>%
    mutate(Percentage = Sales / total_sales * 100,
           # Create detailed hover labels showing the cover type, sales figures, and percentage
           Label = paste0(
             `Type of Cover`, "<br>",
             "Sales: ", formatC(Sales, format = "d", big.mark = ","), "<br>",
             formatC(Percentage, format = "f", digits = 2), "%"
           ))
  
  # Set a constant pull value to detach (explode) each slice
  pull_values <- rep(0.1, nrow(sales_by_cover))
  
  # Create an interactive donut chart with on-slice labels (label and percent) and detailed hover text
  p <- plot_ly(
    sales_by_cover,
    labels = ~`Type of Cover`,
    values = ~Sales,
    type = 'pie',
    hole = 0.5,                      # Determines the donut hole size
    textinfo = 'label+value+percent',      # Show the category labels and percent on the slices
    insidetextorientation = 'radial',
    hoverinfo = 'text',              # Use custom hover text
    hovertext = ~Label,              # Detailed information on hover
    marker = list(colors = custom_colors[1:nrow(sales_by_cover)]),
    pull = pull_values               # Detach each slice slightly
  ) %>%
    layout(
      title = list(text = "Sales by Cover Type", x = 0.5, font = list(family = "Mulish", size = 12)),
      showlegend = TRUE,
      # Optional: centered annotation inside the donut
      annotations = list(
        list(
          text = "",
          x = 0.5,
          y = 0.5,
          font = list(family = "Mulish", size = 16, color = "#555"),
          showarrow = FALSE
        )
      ),
      font = list(family = "Mulish")
    )
  
  p
})



  
custom_colors_cover <- c("#34897f", "#2176C7", "#008b8b", "#00838f", "#2ca02c", "#9467bd", "#F28E2B", "#1f77b4", "#d62728")
output$salescount_by_cover_type <- renderPlotly({
  # Load and filter data
  data <- filtered_data__new_business()
  data <- data[!is.na(data$Sales) & !is.na(data$`Type of Cover`), ]
  
  # Aggregate count of sales by cover type and sort in descending order  
  sales_by_cover <- data %>%
    group_by(`Type of Cover`) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    arrange(desc(Count))
  
  # Calculate total count for percentage calculation  
  total_count <- sum(sales_by_cover$Count)
  
  # Create a funnel chart with creative enhancements
  p <- plot_ly(
    sales_by_cover,
    x = ~Count,
    y = ~`Type of Cover`,
    type = 'funnel',
    marker = list(
      color = custom_colors_cover,
      line = list(color = "white", width = 2)
    ),
    textposition = 'inside',
    textinfo = 'value+percent total+',
    # Create a custom text label that displays the category, count, and percentage share  
    text = ~paste0(`Type of Cover`, "\n", 
                   formatC(Count, format = "d", big.mark = ","), " (", 
                   round(Count/total_count*100, 2), "%)"),
    hoverinfo = 'text',
    # Define a custom hover template for richer details  
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Count: %{x:,}<br>",
      "Percentage: ", round(sales_by_cover$Count/total_count*100,2),"%<extra></extra>"
    )
  ) %>%
    layout(
      title = list(
        text = "Distribution of Sales by Cover Type", 
        x = 0.5, 
        font = list(family = "Mulish", size = 12, color = "#333")
      ),
      xaxis = list(
        title = "Count",
        gridcolor = "#e1e1e1",
        zerolinecolor = "#e1e1e1"
      ),
      yaxis = list(
        title = "",
        automargin = TRUE
      ),
      font = list(
        family = "Mulish",
        size = 12,
        color = "#333"
      ),
      paper_bgcolor = "#fff",
      plot_bgcolor = "#f9f9f9",
      margin = list(l = 100, r = 50, t = 80, b = 50),
      # Add an annotation to display the total count above the funnel chart  
      annotations = list(
        list(
          text = paste("<b>Total Sales Count:</b>", formatC(total_count, format = "d", big.mark = ",")),
          x = 1,
          y = 1.08,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(family = "Mulish", size = 14, color = "#333")
        )
      )
    )
  
  p
})



  output$calendar_plot <- renderPlotly({
    # Prepare data
    sales_data <- filtered_data__new_business() %>%
      filter(!is.na(Date))%>%
      mutate(
        Date = as.Date(Date),  # Ensure Date is in Date format
        Day = day(Date),
        WeekOfMonth = (day(Date) - 1) %/% 7 + 1,  # Calculate week of the month
        DayOfWeek = lubridate::wday(Date, label = TRUE, abbr = TRUE)  # Get abbreviated weekday names
      ) %>%
      group_by(WeekOfMonth, DayOfWeek, Day) %>%
      summarize(TotalSales = sum(Sales, na.rm = TRUE), .groups = 'drop')
    # Check if a specific month is selected (assuming input$new_business_month exists)
    specific_month_selected <- !is.null(new_business_month()) && new_business_month() != "All"
    # Generate the heatmap
    fig <- plot_ly(data = sales_data, x = ~DayOfWeek, y = ~WeekOfMonth, z = ~TotalSales, type = "heatmap",
                   colorscale = list(c(0, "lightblue"), c(1, "blue")),
                   colorbar = list(title = 'Total Sales (KES)'),
                   text = ~paste("Sales: ", scales::comma(TotalSales), " KES"), 
                   hoverinfo = "text+x+y")
    # Add day annotations
    annotations <- if (specific_month_selected) {
      lapply(1:nrow(sales_data), function(i) {
        list(
          x = sales_data$DayOfWeek[i],
          y = sales_data$WeekOfMonth[i],
          text = as.character(sales_data$Day[i]),
          xref = 'x',
          yref = 'y',
          showarrow = FALSE,
          font = list(size = 10, color = "white")
        )
      })
    } else {
      list()  # Empty list if 'All' is selected
    }
    # Adjust layout
    fig <- fig %>% layout(title = "Weekly Sales Heatmap",
                          yaxis = list(title = "Week of Month", dtick = 1, tickmode = "array",
                                       tickvals = 1:max(sales_data$WeekOfMonth, na.rm = TRUE),
                                       ticktext = paste("Week", 1:max(sales_data$WeekOfMonth, na.rm = TRUE))),
                          xaxis = list(title = "Day of Week", dtick = 1, tickmode = "array",
                                       tickvals = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                                       ticktext = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                          annotations = annotations,
                          font = list(family = "Mulish", color = "#333333"),
                          plot_bgcolor = "white",
                          paper_bgcolor = "white")
    fig
  })
  
  output$calendar_plot_count <- renderPlotly({
    # Prepare data
    sales_data <- filtered_data__new_business() %>%
      filter(!is.na(Date))%>%
      mutate( # Ensure Date is in Date format
        Date = as.Date(Date),
        Day = day(Date),
        WeekOfMonth = (day(Date) - 1) %/% 7 + 1,  # Calculate week of the month
        DayOfWeek = lubridate::wday(Date, label = TRUE, abbr = TRUE)  # Get abbreviated weekday names
      ) %>%
      group_by(WeekOfMonth, DayOfWeek, Day) %>%
      summarize(Count = n(), .groups = 'drop')%>%
      as.data.frame()  # Convert to data.frame explicitly
    # Change TotalSales to Count

      # Safeguard: get max week; if no valid value, set a default (e.g., 1)
      maxWeek <- max(sales_data$WeekOfMonth, na.rm = TRUE)
      if (is.infinite(maxWeek)) {
        maxWeek <- 1
      }
    
    # Check if a specific month is selected (assuming input$new_business_month exists)
    specific_month_selected <- !is.null(new_business_month()) && new_business_month() != "All"
    
    # Generate the heatmap
    fig <- plot_ly(data = sales_data, x = ~DayOfWeek, y = ~WeekOfMonth, z = ~Count, type = "heatmap",
                  colorscale = list(c(0, "lightblue"), c(1, "blue")),
                  colorbar = list(title = 'Count'),
                  text = ~paste("Count: ", scales::comma(Count)), 
                  hoverinfo = "text+x+y")
    
    # Add day annotations
    annotations <- if (specific_month_selected) {
      lapply(1:nrow(sales_data), function(i) {
        list(
          x = sales_data$DayOfWeek[i],
          y = sales_data$WeekOfMonth[i],
          text = as.character(sales_data$Day[i]),
          xref = 'x',
          yref = 'y',
          showarrow = FALSE,
          font = list(size = 10, color = "white")
        )
      })
    } else {
      list()
    }
    # Adjust layout
    fig <- fig %>% layout(title = "Weekly Sales Count Heatmap",
                          yaxis = list(title = "Week of Month", 
                                      dtick = 1,
                                      tickmode = "array",
                                      tickvals = 1:maxWeek,
                                      ticktext = paste("Week", 1:maxWeek)),
                          xaxis = list(title = "Day of Week", dtick = 1, tickmode = "array",
                                      tickvals = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                                      ticktext = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                          annotations = annotations,
                          font = list(family = "Mulish", color = "#333333"),
                          plot_bgcolor = "white",
                          paper_bgcolor = "white")
    fig
})
  
    output$sales_count_by_person <- renderPlot({
      data <- filtered_data__new_business() %>%
        filter(!is.na(SalesPerson)) %>%
        group_by(SalesPerson) %>%
        summarize(SalesCount = n(), TotalSales = sum(Sales)) %>%
        arrange(desc(SalesCount))
      ggplot(data, aes(x = reorder(SalesPerson, SalesCount), y = SalesCount, fill = "#17a2b8")) +
        geom_bar(stat = "identity") +  # Bars filled based on TotalSales
        scale_fill_identity() +  # Gradient from blue to red
        geom_text(aes(label = SalesCount), vjust = 0.5, hjust = -0.15, color = "#333333", size=3.02) +
        coord_flip() +
        theme_minimal() +
        theme(legend.position = "none",
              legend.title = element_text(family = "Mulish", size = 10),
              text = element_text(family = "Mulish"),
              axis.title = element_text(family = "Mulish", size = 12),
              axis.text = element_text(family = "Mulish"),
              axis.text.y = element_text(family = "Mulish", size = 12),
              plot.title = element_text(family = "Mulish", size = 14),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(title = "",
            y = "Count of Sales", x = "SalesPerson")
    })
    
custom_colors2 <- c("#17a2b8", "#48d1cc", "#9467bd", "#F28E2B",  "#d62728","#1f77b4", "#F28E2B")
output$sales_by_time_of_day <- renderPlotly({
  data <- filtered_data__new_business()  
  sales_by_time_of_day <- data %>%
    filter(!is.na(Sales), !is.na(`Time of Day`)) %>%  # Remove rows with NA values in Sales or Time of Day
    group_by(`Time of Day`) %>%
    summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
    arrange(`Time of Day`) 
  # Create the waterfall chart
  p <- plot_ly(sales_by_time_of_day, x = ~`Time of Day`, y = ~Sales, type = 'waterfall',
               text = ~paste(formatC(Sales, format = "d", big.mark = ","), " (", formatC(100 * Sales / sum(Sales), format = "f", digits = 2), "%)", sep=""),
               textposition = "inside",
               textfont = list(color = "white", size = 12),
               hoverinfo = "text+y",
               connector = list(line = list(color = 'rgb(150, 150, 150)', width = 2)),
               increasing = list(marker = list(color = custom_colors2)),
               totals = list(marker = list(color = custom_colors2[length(custom_colors2)])))
  # Add title and display the plot
  p <- p %>% layout(title = list(text = "Sum of Sales by Time of Day", x = 0.5),  # Center title
                    xaxis = list(title = "Time of Day"),
                    yaxis = list(title = "Sales", autorange = TRUE),
                    font = list(family = "Mulish"))
  p
})



    custom_colors_time_of_day <- c("#17a2b8", "#008b8b", "#48d1cc", "#F28E2B",  "#d62728","#1f77b4", "#F28E2B")
    output$salescount_by_time_of_day <- renderPlotly({
      data <- filtered_data__new_business()  # Load the necessary sales data
      # Filter out rows where Sales or `Time of Day` might be NA
      data <- data[!is.na(data$Sales) & !is.na(data$`Time of Day`), ]
      # Group data by 'Time of Day' and count the number of transactions
      sales_by_time_of_day <- data %>%
        group_by(`Time of Day`) %>%
        summarise(TransactionCount = n(), .groups = 'drop')
      # Create the donut chart
      p <- plot_ly(sales_by_time_of_day, labels = ~`Time of Day`, values = ~TransactionCount, type = 'pie', hole = 0.4,
                  textposition = 'inside', 
                  textinfo = 'label+value+percent',  
                  insidetextorientation = 'tangential', 
                  marker = list(colors = custom_colors_time_of_day),
                  textfont = list(color = 'white', family = "Mulish", size = 12))
      # Add title and display the plot increase the font of the title
      p <- p %>% layout(title = "Distribution of Sales by Time of Day" ,showlegend = TRUE,
                        font = list(family = "Mulish"))
      p
    })

    output$sales_by_source <- renderPlotly({
    data <- filtered_data__new_business() %>%
      group_by(Source) %>%
      summarize(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      mutate(TotalSalesMillions = TotalSales / 1e6) %>%
      mutate(Source = fct_reorder(Source, TotalSalesMillions)) %>%  # Reorder factor levels
      arrange((TotalSalesMillions))
    
    plot_ly(data, x = ~TotalSalesMillions, y = ~Source, type = 'bar',
            marker = list(color = '#17a2b8')) %>%
      layout(
        title = "Sales by Source",
        yaxis = list(title = "Source", tickangle = 0, tickfont = list(size = 10, color = "#333333")),
        xaxis = list(title = "Total Sales (Million KES)", tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        margin = list(b = 100), # To accommodate rotated x-axis labels
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      ) %>%
      add_annotations(
        x = ~TotalSalesMillions,
        y = ~Source,
        text = ~sprintf("%.2f M", TotalSalesMillions),
        showarrow = FALSE,
        xshift = 15,
        font = list(size = 9, color = "#333333")
      )
  })

    output$sales_count_by_source <- renderPlot({
    data <- filtered_data__new_business() %>%
      filter(!is.na(Source)) %>%
      group_by(Source) %>%
      summarize(SalesCount = n(), TotalSales = sum(Sales)) %>%
      arrange(desc(SalesCount))
    ggplot(data, aes(x = reorder(Source, SalesCount), y = SalesCount, fill = "#0d6efd")) +
      geom_bar(stat = "identity") +  
      scale_fill_identity() +  
      geom_text(aes(label = SalesCount), vjust = 0.5, hjust = -0.15, color = "#333333", size=3.02) +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none",
            legend.title = element_text(family = "Mulish", size = 10),
            text = element_text(family = "Mulish"),
            axis.title = element_text(family = "Mulish", size = 12),
            axis.text = element_text(family = "Mulish"),
            axis.text.y = element_text(family = "Mulish", size = 12),
            plot.title = element_text(family = "Mulish", size = 14),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(title = "Sales Count by Source",
           y = "Count of Sales", x = "Source")
  })

  })
}
