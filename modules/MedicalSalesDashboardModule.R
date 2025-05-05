# UI for sales dashboard including graphs
MedicalsalesDashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      class = "value-box-row",
      column(width = 4, uiOutput(ns("total_sales_health"))),
      column(width = 4, uiOutput(ns("count_new_bus_health"))),
      column(width = 4, uiOutput(ns("count_renewal_bus_health")))
    ),
    fluidRow(
      tabBox( 
          solidHeader = TRUE,
          selected = "Count by Salesperson",
          status = "white",
          type = "tabs",
          id = ns("salesperson_tabs"),
          tabPanel("Sales by Salesperson", plotlyOutput(ns("sales_by_salesperson_health")) %>% withSpinner(type = 5)),
          tabPanel("Count by Salesperson", plotlyOutput(ns("count_by_salesperson_health")) %>% withSpinner(type = 5))
        ),
      tabBox( 
          solidHeader = TRUE,
          selected = "Count Business Type",
          status = "white",
          type = "tabs",
          id = ns("salesperson_tabs"),
          tabPanel("Sales Business Type", plotlyOutput(ns("sales_by_business_category_health")) %>% withSpinner(type = 5)),
          tabPanel("Count Business Type", plotlyOutput(ns("count_by_business_category_health")) %>% withSpinner(type = 5))
        ),
      box(
        title = "Sales by Insurer", 
        status = "white", 
        solidHeader = TRUE,
        plotlyOutput(ns("sales_by_Insurer_health"))  %>% withSpinner(type = 5)),
      box(
        title = "Count by Insurer", 
        status = "white", 
        solidHeader = TRUE,
        plotlyOutput(ns("count_by_insurer_health"))  %>% withSpinner(type = 5)),
      box(
        title = "Sales by Source of Business", 
        status = "white", 
        solidHeader = TRUE,
        plotlyOutput(ns("sales_by_source_of_business_health"))  %>% withSpinner(type = 5)),
      box(title = "Distribution of Sales by Source", status = "success", solidHeader = TRUE,
          plotlyOutput(ns("count_by_source_of_business_health"))  %>% withSpinner(type = 5))         
    )
  )
}



# Server logic for Medical Sales dashboard
MedicalsalesDashboardServer <- function(id, filtered_data__health_business) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace function to handle IDs

    output$total_sales_health <- renderUI({
      # Total Sales (KES) based on the row count of the filtered health business data
      total_sales <- nrow(filtered_data__health_business())
      # Use a professional blue for total sales
      customValueBox("Total Sales Count", format(total_sales, big.mark = ","), "#2176C7")
    })

    output$count_new_bus_health <- renderUI({
      # Calculate count for New Business sales
      count_new_bus <- filtered_data__health_business() %>%
        filter(`Busines Category` == "New Business") %>%
        summarize(CountSalesNew = n(), .groups = 'drop')
      
      count_to_display <- if (nrow(count_new_bus) == 0) 0 else count_new_bus$CountSalesNew
      # Use a professional green color for new business counts
      customValueBox("New Business Sales Count", format(count_to_display, big.mark = ","), "#27ae60")
    })

    output$count_renewal_bus_health <- renderUI({
      # Calculate count for Renewal Business sales
      count_renewal_bus <- filtered_data__health_business() %>%
        filter(`Busines Category` == "Renewal") %>%
        summarize(CountSalesReNewal = n(), .groups = 'drop')
      
      count_to_display <- if (nrow(count_renewal_bus) == 0) 0 else count_renewal_bus$CountSalesReNewal
      # Use a professional red color for renewal counts
      customValueBox("Renewals Sales Count", format(count_to_display, big.mark = ","), "#c0392b")
    })

  

  custom_colors_bus_category_health <- c( "#00acc1","#80deea", "#ff7f0e", "#1f77b4", "#9467bd", "#d62728")
  output$sales_by_business_category_health <- renderPlotly({
    sales_by_business_category <- filtered_data__health_business() %>%
      # Filter out rows where Sales or Business Category might be NA
      filter(!is.na(Sales), !is.na(`Busines Category`)) %>%
      # Group data by 'Busines Category' and sum 'Sales'
      group_by(`Busines Category`) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
      # Calculate percentages
      mutate(percentage = Sales / sum(Sales) * 100)
    # Generate color palette
    num_categories <- length(unique(sales_by_business_category$`Busines Category`))
    colors <- custom_colors_bus_category_health 
    # Create the donut chart
    p <- plot_ly(sales_by_business_category, labels = ~`Busines Category`, values = ~Sales, type = 'pie', hole = 0.6,
                 textposition = 'outside',
                 textinfo = 'label+value+percent',
                 insidetextorientation = 'tangential',
                 marker = list(colors = colors),
                 textfont = list(color = 'black', family = "Mulish", size = 12))
    # Add title and display the plot
    p <- p %>% layout(showlegend = TRUE,
                      font = list(family = "Mulish"))
    p
  })

custom_colors5 <- c("#48d1cc", "#00838f", "#ff7f0e", "#1f77b4", "#9467bd", "#d62728")
output$count_by_business_category_health <- renderPlotly({
  count_by_business_category <- filtered_data__health_business() %>%
    # Filter out rows where Business Category might be NA
    filter(!is.na(Sales), !is.na(`Busines Category`)) %>%
    # Group data by 'Business Category' and count occurrences
    group_by(`Busines Category`) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    # Calculate percentages
    mutate(percentage = Count / sum(Count) * 100)
  # Generate color palette
  num_categories <- length(unique(count_by_business_category$`Busines Category`))
  colors <- custom_colors5 
  # Create the donut chart
  p <- plot_ly(count_by_business_category, labels = ~`Busines Category`, values = ~Count, type = 'pie', hole = 0.6,
               textposition = 'outside',
               textinfo = 'label+value+percent',
               insidetextorientation = 'tangential',
               marker = list(colors = colors),
               textfont = list(color = 'black', family = "Mulish", size = 12))
  # Add title and display the plot
  p <- p %>% layout(showlegend = TRUE,
                    font = list(family = "Mulish"))
  p
})




output$sales_by_salesperson_health <- renderPlotly({
    # Process the data
    data <- filtered_data__health_business() %>%
      filter(!is.na(Salesperson)) %>%
      filter(`Busines Category` == "New Business") %>%
      group_by(`Salesperson`) %>%
      summarize(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      mutate(TotalSalesMillions = TotalSales / 1e6) %>%
      arrange(desc(TotalSalesMillions))
    # Factor with levels in descending order to show the highest sales at the top
    data$Salesperson <- factor(data$Salesperson, levels = rev(data$Salesperson))
    maxTotalSalesMillions <- max(data$TotalSalesMillions)
    # Creating the Plotly plot
    plot <- plot_ly(
      data,
      x = ~TotalSalesMillions,
      y = ~Salesperson,
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#17a2b8')  # Set the bar color to Bootstrap's info color
    ) %>%
      layout(
        xaxis = list(title = 'Total Sales (Millions KES)', tickformat = ',.1f'),
        yaxis = list(title = 'Salesperson', title_standoff = 20),  # Add title_standoff to create space between title and ticks
        showlegend = FALSE,
        margin = list(t = 10, r = 20, b = 10, l = 10),
        annotations = lapply(1:nrow(data), function(i) {
          list(
            x = data$TotalSalesMillions[i] + (maxTotalSalesMillions * 0.02),
            y = data$Salesperson[i],
            text = paste(comma(data$TotalSalesMillions[i], accuracy = 0.01), "M"),
            showarrow = FALSE,
            xanchor = 'left',
            yanchor = 'middle',
            font = list(color = 'black', size = 9)
          )
        }),
        font = list(family = "Mulish", size = 12)
      )
    
    plot
  })
    
  custom_colors6 <- c("#17a2b8", "#48d1cc", "#ff7f0e","#2ca02c", "#1f77b4", "#9467bd", "#d62728")
  output$sales_by_location_health <- renderPlotly({
    sales_by_location <- filtered_data__health_business() %>%
      # Filter out rows where Sales or Business Category might be NA
      filter(!is.na(Sales), !is.na(Business_Location_Type)) %>%
      # Group data by 'Busines Category' and sum 'Sales'
      group_by(Business_Location_Type) %>%
      summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
      # Calculate percentages
      mutate(percentage = Sales / sum(Sales) * 100)
    # Generate color palette
    num_categories <- length(unique(sales_by_location$Business_Location_Type))
    colors <- custom_colors6 
    # Create the donut chart
    p <- plot_ly(sales_by_location, labels = ~Business_Location_Type, values = ~Sales, type = 'pie', hole = 0.6, 
                 textposition = 'outside',
                 textinfo = 'label+value+percent',
                 insidetextorientation = 'tangential',
                 marker = list(colors = colors),
                 textfont = list(color = 'black', family = "Mulish", size = 12))
    # Add title and display the plot
    p <- p %>% layout(showlegend = TRUE,
                      font = list(family = "Mulish"))
    p
  })


custom_colors_location_health <- c("#17a2b8", "#80deea", "#ff7f0e", "#2ca02c", "#1f77b4", "#9467bd", "#d62728")
output$count_by_location_health <- renderPlotly({
  sales_by_location <- filtered_data__health_business() %>%
    # Filter out rows where Business Location Type might be NA
    filter(!is.na(Business_Location_Type)) %>%
    # Group data by 'Business Location Type' and count occurrences
    group_by(Business_Location_Type) %>%
    summarise(Count = n()) %>%
    # Calculate percentages
    mutate(percentage = Count / sum(Count) * 100)
  # Generate color palette
  num_categories <- length(unique(sales_by_location$Business_Location_Type))
  colors <- custom_colors_location_health
  # Create the donut chart
  p <- plot_ly(sales_by_location, labels = ~Business_Location_Type, values = ~Count, type = 'pie', hole = 0.6, 
               textposition = 'outside',
               textinfo = 'label+value+percent',
               insidetextorientation = 'tangential',
               marker = list(colors = colors),
               textfont = list(color = 'black', family = "Mulish", size = 12))
  # Add title and display the plot
  p <- p %>% layout(showlegend = TRUE,
                    font = list(family = "Mulish"))
  p
})

  
  output$sales_by_Insurer_health <- renderPlotly({
    # Process the data
    data <- filtered_data__health_business() %>%
      group_by(`Insurers Proposed`) %>%
      summarize(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      mutate(TotalSalesMillions = TotalSales / 1e6) %>%
      arrange(desc(TotalSalesMillions))
    # Creating the Plotly plot
    plot <- plot_ly(
      data,
      x = ~reorder(`Insurers Proposed`, -TotalSalesMillions),
      y = ~TotalSalesMillions,
      type = 'bar',
      marker = list(color = '#17a2b8', line = list(color = '#333333', width = 0.2))  # Set the bar color to Bootstrap's warning color and add a black border
    ) %>%
      layout(
        xaxis = list(title = 'Insurer', tickangle = -45),
        yaxis = list(title = 'Total Sales (Millions KES)', tickformat = ',.0f'),
        showlegend = FALSE,
        margin = list(t = 20, r = 20, b = 80, l = 20),
        annotations = lapply(1:nrow(data), function(i) {
          list(
            x = i - 1,
            y = data$TotalSalesMillions[i],
            text = sprintf("%.1f M", data$TotalSalesMillions[i]),
            showarrow = FALSE,
            xanchor = 'center',
            yanchor = 'bottom',
            font = list(color = 'black', size = 9)
          )
        }),
        font = list(family = "Mulish"),
        plot_bgcolor = '#f8f9fa',  # Match panel background
        paper_bgcolor = '#f8f9fa'  # Match panel background
      )
    
    plot
  })
  
  output$count_by_insurer_health <- renderPlotly({
    # Process the data
    data <- filtered_data__health_business() %>%
      group_by(`Insurers Proposed`) %>%
      summarize(CountSales = n()) %>%
      arrange(desc(CountSales))
    # Calculate the maximum value of CountSales
    maxCountSales <- max(data$CountSales)
    # Factor with levels in descending order to show the highest counts at the top
    data$`Insurers Proposed` <- factor(data$`Insurers Proposed`, levels = rev(data$`Insurers Proposed`))
    # Creating the Plotly lollipop plot
    plot <- plot_ly(
      data,
      x = ~CountSales,
      y = ~`Insurers Proposed`,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = '#17a2b8', width = 2),  
      marker = list(color = '#17a2b8', size = 8, line = list(color = '#17a2b8', width = 1)), 
      text = ~CountSales,
      textposition = 'right',
      hoverinfo = 'text'
    ) %>%
      layout(
        xaxis = list(title = 'Number of Sales Transactions'),
        yaxis = list(title = '`Insurers Proposed`', title_standoff = 20),  
        showlegend = FALSE,
        margin = list(t = 10, r = 20, b = 10, l = 10),
        annotations = lapply(1:nrow(data), function(i) {
          list(
            x = data$CountSales[i] + (maxCountSales * 0.05),
            y = data$`Insurers Proposed`[i],
            text = data$CountSales[i],
            showarrow = FALSE,
            xanchor = 'left',
            yanchor = 'middle',
            font = list(color = 'black', size = 9)
          )
        }),
        font = list(family = "Mulish", size = 12),
        plot_bgcolor = '#f8f9fa',  
        paper_bgcolor = '#f8f9fa' 
      )
    
    plot
  })
  
  output$count_by_salesperson_health <- renderPlotly({
    # Process the data
    data <- filtered_data__health_business() %>%
      filter(!is.na(Salesperson)) %>%
      filter(`Busines Category` == "New Business") %>%
      group_by(`Salesperson`) %>%
      summarize(CountSales = n()) %>%
      arrange(desc(CountSales))
    # Calculate the maximum value of CountSales
    maxCountSales <- max(data$CountSales)
    # Factor with levels in descending order to show the highest counts at the top
    data$Salesperson <- factor(data$Salesperson, levels = rev(data$Salesperson))
    # Creating the Plotly lollipop plot
    plot <- plot_ly(
      data,
      x = ~CountSales,
      y = ~Salesperson,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = '#17a2b8', width = 2),  # Line color set to Bootstrap's "info" color
      marker = list(color = '#17a2b8', size = 8, line = list(color = '#17a2b8', width = 1)),  # Marker color set to Bootstrap's "info" color
      text = ~CountSales,
      textposition = 'right',
      hoverinfo = 'text'
    ) %>%
      layout(
        xaxis = list(title = 'Number of Sales Transactions'),
        yaxis = list(title = 'Salesperson', title_standoff = 20),  # Add title_standoff to create space between title and ticks
        showlegend = FALSE,
        margin = list(t = 10, r = 20, b = 10, l = 10),
        annotations = lapply(1:nrow(data), function(i) {
          list(
            x = data$CountSales[i] + (maxCountSales * 0.05),
            y = data$Salesperson[i],
            text = data$CountSales[i],
            showarrow = FALSE,
            xanchor = 'left',
            yanchor = 'middle',
            font = list(color = 'black', size = 9)
          )
        }),
        font = list(family = "Mulish", size = 12),
        plot_bgcolor = '#f8f9fa',  # Match panel background
        paper_bgcolor = '#f8f9fa'  # Match panel background
      )
    
    plot
  })
  
  output$sales_by_source_of_business_health <- renderPlotly({
      data <- filtered_data__health_business() %>%
        filter(!is.na(Sales), !is.na(Source)) %>%
        group_by(Source) %>%
        summarise(Sales = sum(Sales, na.rm = TRUE)) %>%
        arrange(desc(Sales)) %>%
        mutate(Source = factor(Source, levels = Source))
      # Create funnel chart in Plotly
      plot_ly(data, 
              y = ~Source, 
              x = ~Sales, 
              type = 'funnel', 
              textinfo = "value+percent",
              marker = list(color = '#0d6efd')) %>%
        layout(
          title = "Sales by Source of Business Health",
          yaxis = list(title = "Source"),
          xaxis = list(title = "Sales"),
          hoverlabel = list(bgcolor = '#0d6efd', font = list(color = 'white')),
          plot_bgcolor = 'white',
          paper_bgcolor = 'white',
          font = list(family = "Mulish")
        )
  })

  output$count_by_source_of_business_health <- renderPlotly({
      data <- filtered_data__health_business() %>%
        filter(!is.na(Sales), !is.na(Source)) %>%
        group_by(Source) %>%
        dplyr::summarise(TotalCounts = n()) %>%
        arrange(desc(TotalCounts)) %>%
        mutate(Source = factor(Source, levels = Source))
      # Create funnel chart in Plotly
      plot_ly(data, 
              y = ~Source, 
              x = ~TotalCounts, 
              type = 'funnel', 
              textinfo = "value+percent",
              marker = list(color = '#0d6efd')) %>%
        layout(
          title = "Count of Sales by Source of Business Health",
          yaxis = list(title = "Source"),
          xaxis = list(title = "Count of Sales"),
          hoverlabel = list(bgcolor = '#0d6efd', font = list(color = 'white')),
          plot_bgcolor = 'white',
          paper_bgcolor = 'white',
          font = list(family = "Mulish")
        )
  })


  })
}
