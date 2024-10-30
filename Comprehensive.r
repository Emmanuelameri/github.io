# Load required libraries
library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(forecast)
library(cluster)
library(factoextra)
library(readr)
library(GGally)
library(dplyr)
library(htmltools)
library(tidyverse)
library(rsconnect)


# Load the data
Data_Sheet1<- read_csv("data/shiny/Data - Sheet1.csv",col_types = cols(`Facebook users per year` = col_number()))

ui <- fluidPage(
  theme = bs_theme(preset = "superhero"),
  titlePanel("Comprehensive Analysis of Key Metrics"),
  
  tabsetPanel(
    tabPanel("Home",
             fluidRow(
               column(12,
                      h2("Introduction"),
                      p("In today's data-driven world, the ability to analyze 
                        and visualize complex datasets is crucial for making 
                        informed business decisions. I have developed a 
                        comprehensive Shiny application that facilitates the 
                        exploration of key metrics related to technology and 
                        social media companies, showcasing different
                        visualizations, correlation, and predictive modeling."),
                      
                      h2("Project Overview"),
                      p("The application I created offers a user-friendly 
                        interface for stakeholders to analyze trends,
                        correlations, and growth rates of various metrics, 
                        including sales figures, user engagement, and revenue 
                        generation from major players like Meta, Google, and
                        Amazon. By leveraging the R programming language and 
                        its powerful libraries, I aimed to provide insightful 
                        visualizations that drive strategic decision-making."),
                      
                      h2("Key Features"),
                      tags$ul(
                        tags$li("Trend Analysis: Users can select multiple metrics to visualize trends over the years."),
                        tags$li("Correlation Analysis: The application includes a correlation analysis feature."),
                        tags$li("Annual Revenue Comparison: Users can compare the annual revenues of major companies."),
                        tags$li("Growth Rates: The application calculates and visualizes growth rates for key metrics."),
                        tags$li("Predictive Modeling: Users can input the number of years they wish to predict future revenues.")
                      ),
                      
                      tags$h3("Conclusion"),
                      tags$p("The Shiny application provides a robust framework for analyzing key metrics within the technology and social media sectors, offering valuable insights through interactive visualizations and statistical analyses."),
                      
                      tags$h3("Key Findings"),
                      tags$ul(
                        tags$li("Correlation Analysis: The correlation matrix reveals strong positive relationships among critical variables, such as the number of internet users, Meta Platforms' revenue, and Google's advertising revenue. These correlations highlight the interconnectedness of these metrics, suggesting that as user engagement increases, revenue generation across platforms also rises. This underscores the importance of user acquisition and retention strategies for sustained growth."),
                        tags$li("Growth Rates: The analysis of growth rates indicates significant increases in key metrics, particularly in the late 2020s, reflecting rapid technological evolution and changing consumer behaviors. The COVID-19 pandemic has notably accelerated digital engagement, leading to heightened revenue for major players like Meta and Amazon."),
                        tags$li("Predictive Modeling: The predictive modeling component allows users to forecast future revenues based on historical data, empowering stakeholders to make informed decisions regarding investments, marketing strategies, and resource allocation. The expected upward trends in revenue metrics suggest a promising outlook for the technology and social media landscape.")
                      ),
                      
                      tags$h3("Implications for Stakeholders"),
                      tags$p("The insights derived from this application are crucial for various stakeholders, including investors, marketers, and business strategists. Understanding the relationships between user engagement, revenue growth, and market dynamics can guide strategic planning and resource allocation. Companies can leverage these insights to enhance their competitive positioning in an increasingly digital economy."),
                      
                      tags$h3("Future Directions"),
                      tags$p("As technology continues to advance and user behaviors evolve, ongoing analysis of these metrics will be essential. Future iterations of the application could incorporate additional data sources, machine learning algorithms for more sophisticated predictive analytics, and further exploration of user demographics to refine strategies and enhance decision-making."),
                      
                      tags$p("In conclusion, this Shiny application serves as a powerful tool for visualizing and understanding the complex relationships within the technology and social media sectors, providing stakeholders with the insights needed to navigate an ever-changing landscape effectively."),
                      h2("References"),
                      tags$ul(
                        tags$li("Advertising Revenue of Google Network Sites 2018 | Statista. (2018). Statista.
                         Retrieved from https://www.statista.com/statistics/266245/advertising-revenue-of-google-network-sites/ "),
                        tags$li("Bianchi, T. (2024). Google: Revenue 2017 | Statista. Statista.
                         Retrieved from https://www.statista.com/statistics/266206/googles-annual-global-revenue/"),
                        tags$li("Laricchia, F. (2024). Cell Phone Sales Worldwide 2007-2017 | Statista. Statista.
                         Retrieved from https://www.statista.com/statistics/263437/global-smartphone-sales-to-end-users-since-2007/"),
                        tags$li("Statista. (2023). Facebook - Revenue and Net Income 2018 | Statista.
                         Retrieved from https://www.statista.com/statistics/277229/facebooks-annual-revenue-and-net-income/"),
                        tags$li("Statista. (2024). Number of Monthly Active Facebook Users Worldwide as of 3rd Quarter 2023.
                         Retrieved from https://www.statista.com/statistics/264810/number-of-monthly-active-facebook-users-worldwide/"),
                        tags$li("Statista. (2024). Annual Net Sales Revenue of Amazon from 2004 to 2023.
                         Retrieved from https://www.statista.com/statistics/"),
                      )
                      
               )
             )
    ),
    
  
    tabPanel("Trend Analysis",
             fluidRow(
               column(12,
                      h2("Trend Analysis"),
                      p("In this section, you can visualize trends over the years for selected metrics.")
               )
             ),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("metrics", "Choose metrics to visualize:",
                                    choices = c(
                                      "Phones Sold" = "Number of Phones sold", 
                                      "Internet Users" = "Number of internet users", 
                                      "Meta Revenue" = "Annual revenue generated by Meta Platforms",
                                      "Google Revenue" = "Annual revenue of google",
                                      "Facebook users"= "Facebook users per year", 
                                      "Amazon revenue"= "Amazon annual sales",                           
                                      "Advertising revenue on google"= "Advertising revenue of Google network websites"
                                    ),
                                    selected = c("Number of Phones sold", "Number of internet users")),
                 p("Visualize trends over the years for selected metrics.")
               ),
               mainPanel(
                 plotlyOutput("customPlot")
               )
             )
    ),
    
    tabPanel("Correlation Analysis",
             fluidRow(
               column(12,
                      h2("Correlation Analysis"),
                      p("This section provides a correlation analysis feature that visually represents the relationship between different metrics.")
               ),
               tags$h3("Key Insights on Revenue Models"),
               tags$ul(
                 tags$li("Predictive models for all types of revenue are expected to see an increase following model revisions."),
                 tags$li("Growth rates for most metrics are primarily observed in the late 2020s, with the highest growth rates recorded for Meta, Amazon, Google advertising revenue, Google revenue, internet users, and Facebook revenue during its early stages. Interestingly, Facebook's revenue increased in the 2020s, likely driven by the COVID-19 pandemic and the rise of Facebook Marketplace. Overall trends remain uncertain."),
                 tags$li("Amazon's annual sales revenue significantly surpasses Google's, reaching approximately $572 billion in 2023, driven by its vast product selection and strong customer loyalty through Amazon Prime. Amazon controls about 40% of the U.S. e-commerce market, providing it with a substantial customer base and valuable consumer data for targeted advertising."),
                 tags$li("Google's revenue, primarily from its advertising business, amounts to around $321 billion, focusing on a broader range of industries. Its advertising model drives traffic to external sites, while Amazon's is more closely linked to consumer purchasing intent on its platform."),
                 tags$li("Meta's total revenue stands at $100 billion, significantly higher than both Amazon and Google. However, Meta’s revenue is primarily derived from Google and Amazon."),
                 tags$li("The revenue disparity between Amazon and Google is largely due to Amazon's dominant position in e-commerce, its ability to leverage consumer data for targeted advertising, and its effective integration of advertising within its shopping platform.")
               )
               
             ),
             mainPanel(
               plotlyOutput("correlationPlot")
             )
    ),
    
    tabPanel("Annual Revenue Comparison",
             fluidRow(
               column(12,
                      h2("Annual Revenue Comparison"),
                      p("In this section, users can compare the annual revenues of major companies side by side.")
               
             ),
             tags$h3("Key Insights on Revenue Models"),
             tags$ul(
               tags$li("Predictive models for all types of revenue are expected to see an increase following my model revisions."),
               tags$li("Except for Facebook users, growth rates for all other metrics are primarily observed in the late 2020s. In 2008 and the early 2020s, the highest growth rates were seen for Meta, Amazon, Google advertising revenue, Google revenue, internet users, and Facebook revenue during its early stages. Interestingly, Facebook's revenue saw an increase in the 2020s, likely driven by the COVID-19 pandemic and the rise of Facebook Marketplace. At this point, I am uncertain about the overall trends."),
               tags$li("Amazon's annual sales revenue significantly surpasses Google's due to its extensive e-commerce operations. In 2023, Amazon's e-commerce revenue reached approximately $572 billion, driven by its vast product selection and strong customer loyalty, particularly through its Amazon Prime subscription service. Amazon controls about 40% of the U.S. e-commerce market, providing it with a substantial customer base and valuable consumer data, allowing for targeted advertising and marketing strategies."),
               tags$li("Google's revenue primarily comes from its advertising business, which, while substantial at around $321 billion, is focused on a broader range of industries beyond retail. Google’s advertising model is built on driving traffic to external websites, whereas Amazon's advertising is more directly linked to consumer purchasing intent within its own platform."),
               tags$li("In contrast, Meta's total revenue stands at $100 billion, which is significantly Higher than both Amazon and Google. Meta's revenue is primarily derived from google and amazon."),
               tags$li("The disparity in revenue between Amazon and Google can be attributed to Amazon's dominant position in the e-commerce sector, its ability to leverage consumer data for targeted advertising, and its successful integration of advertising within its shopping platform.")
             ),
             ),
             mainPanel(
               plotOutput("barChart")
             )
    ),
    
    tabPanel("Growth Rates",
             fluidRow(
               column(12,
                      h2("Growth Rates"),
                      p("This section calculates and visualizes growth rates for key metrics."),
                      p("Except for Facebook users, growth rates for all other metrics are primarily observed in the late 2020s. In 2008 and the early 2020s, the highest growth rates were seen for Meta, Amazon, Google advertising revenue, Google revenue, internet users, and Facebook revenue during its early stages. Interestingly, Facebook's revenue saw an increase in the 2020s, likely driven by the COVID-19 pandemic and the rise of Facebook Marketplace. At this point, I am uncertain about the overall trends.")
               )
             ),
             mainPanel(
               plotOutput("growthRatePlot")
             )
    ),
    
    tabPanel("Predictive Modeling",
             fluidRow(
               column(12,
                      h2("Predictive Modeling"),
                      p("In this section, users can input the number of years they wish to predict future revenues."),
                      p(" All predictive models for all revenue metrics are expected to see a positive increase in revenue in the upcoming years following my model investigation.")
               
               )
             ),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("revenue_metrics", "Choose revenue metrics to visualize:",
                                    choices = c(
                                      "Meta Revenue" = "Annual revenue generated by Meta Platforms",
                                      "Google Revenue" = "Annual revenue of google",
                                      "Amazon Revenue" = "Amazon annual sales"
                                    ),
                                    selected = c("Annual revenue generated by Meta Platforms")),
                 numericInput("years", "Number of Years to Predict:", value = 5, min = 1)
               ),
               mainPanel(
                 plotOutput("predictionPlot")
               )
             )
    )
  )
)

server <- function(input, output) {
  
  output$customPlot <- renderPlotly({
    long_data <- Data_Sheet1 %>%
      select(Year, `Number of Phones sold`, `Number of internet users`, 
             `Annual revenue generated by Meta Platforms`, `Annual revenue of google`, 
             `Advertising revenue of Google network websites`, `Amazon annual sales`,
             `Facebook users per year`) %>%
      pivot_longer(cols = -Year, names_to = "Metric", values_to = "Value")
    
    selected_metrics <- input$metrics
    
    plot_list <- lapply(selected_metrics, function(metric) {
      ggplot(long_data %>% filter(Metric == metric), aes(x = Year, y = Value)) +
        geom_line() +
        geom_point() +
        labs(
          x = "Year",
          y = metric,
          title = paste("Trend of", gsub("_", " ", metric), "Over Time")
        ) +
        theme_dark()
    })
    
    plotly_list <- lapply(plot_list, ggplotly)
    
    subplot(plotly_list, nrows = ceiling(sqrt(length(plotly_list))), shareX = TRUE, titleX = TRUE)
  })
  
  output$correlationPlot <- renderPlotly({
    # Select relevant columns for correlation
    correlation_data <- Data_Sheet1 %>%
      select(`Number of Phones sold`, `Number of internet users`, 
             `Annual revenue generated by Meta Platforms`, 
             `Annual revenue of google`, 
             `Advertising revenue of Google network websites`, 
             `Amazon annual sales`,
             `Facebook users per year`)
    
    # Calculate correlation matrix
    correlation_matrix <- cor(correlation_data, use = "pairwise.complete.obs")
    
    # Convert to long format for plotly
    correlation_long <- as.data.frame(as.table(correlation_matrix))
    colnames(correlation_long) <- c("Metric1", "Metric2", "Correlation")
    
    # Create an interactive heatmap
    plot_ly(data = correlation_long, x = ~Metric1, y = ~Metric2, z = ~Correlation, 
            type = "heatmap", colorscale = "Viridis") %>%
      layout(title = "Correlation Matrix Heatmap",
             xaxis = list(title = "Metrics"),
             yaxis = list(title = "Metrics"),
             colorbar = list(title = "Correlation"))
  })
  
  
  output$barChart <- renderPlot({
    long_data <- Data_Sheet1 %>%
      select(Year, `Annual revenue generated by Meta Platforms`, `Annual revenue of google`, `Amazon annual sales`) %>%
      pivot_longer(cols = -Year, names_to = "Company", values_to = "Revenue")
    
    ggplot(long_data, aes(x = Year, y = Revenue, fill = Company)) +
      geom_col(position = "dodge") +
      labs(title = "Annual Revenue Comparison", y = "Revenue", fill = "Company") +
      theme_minimal() +
      facet_wrap(~ Company, scales = "free_y")
  })
  
  # Growth Rates Plot
  output$growthRatePlot <- renderPlot({
    # Calculate growth rates for all relevant metrics
    growth_data <- Data_Sheet1 %>%
      mutate(
        Growth_Rate_Phones = (lead(`Number of Phones sold`) - `Number of Phones sold`) / `Number of Phones sold`,
        Growth_Rate_Internet_Users = (lead(`Number of internet users`) - `Number of internet users`) / `Number of internet users`,
        Growth_Rate_Meta_Revenue = (lead(`Annual revenue generated by Meta Platforms`) - `Annual revenue generated by Meta Platforms`) / `Annual revenue generated by Meta Platforms`,
        Growth_Rate_Google_Revenue = (lead(`Annual revenue of google`) - `Annual revenue of google`) / `Annual revenue of google`,
        Growth_Rate_Amazon_Revenue = (lead(`Amazon annual sales`) - `Amazon annual sales`) / `Amazon annual sales`,
        Growth_Rate_Advertising_Revenue = (lead(`Advertising revenue of Google network websites`) - `Advertising revenue of Google network websites`) / `Advertising revenue of Google network websites`,
        Growth_Rate_Facebook_Users = (lead(`Facebook users per year`) - `Facebook users per year`) / `Facebook users per year`
      )
    
    # Create a long format data frame for plotting
    growth_long <- growth_data %>%
      select(Year, Growth_Rate_Phones, Growth_Rate_Internet_Users, 
             Growth_Rate_Meta_Revenue, Growth_Rate_Google_Revenue,
             Growth_Rate_Amazon_Revenue, Growth_Rate_Advertising_Revenue,
             Growth_Rate_Facebook_Users) %>%
      pivot_longer(-Year, names_to = "Metric", values_to = "Growth_Rate")
    
    # Generate the plot
    ggplot(growth_long, aes(x = Year, y = Growth_Rate, color = Metric)) +
      geom_line() +
      geom_point() +
      labs(title = "Growth Rates of Various Metrics", 
           y = "Growth Rate", 
           x = "Year", 
           color = "Metrics") +
      scale_color_manual(values = c(
        "Growth_Rate_Phones" = "blue",
        "Growth_Rate_Internet_Users" = "green",
        "Growth_Rate_Meta_Revenue" = "red",
        "Growth_Rate_Google_Revenue" = "purple",
        "Growth_Rate_Amazon_Revenue" = "orange",
        "Growth_Rate_Advertising_Revenue" = "cyan",
        "Growth_Rate_Facebook_Users" = "magenta"
      )) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  
  
  tabPanel("Predictive Modeling",
           fluidRow(
             column(12,
                    h2("Predictive Modeling"),
                    p("In this section, users can input the number of years they wish to predict future revenues.")
             )
           ),
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("revenue_metrics", "Choose revenue metrics to visualize:",
                                  choices = c(
                                    "Meta Revenue" = "Annual revenue generated by Meta Platforms",
                                    "Google Revenue" = "Annual revenue of google",
                                    "Amazon Revenue" = "Amazon annual sales"
                                  ),
                                  selected = c("Annual revenue generated by Meta Platforms")),
               numericInput("years", "Number of Years to Predict:", value = 5, min = 1)
             ),
             mainPanel(
               plotOutput("predictionPlot")
             )
           )
  )
  
  output$predictionPlot <- renderPlot({
    req(input$revenue_metrics)  # Ensure that metrics are selected
    
    all_data <- data.frame()
    
    # Loop through selected revenue metrics
    for (metric in input$revenue_metrics) {
      # Print the metric for debugging
      print(paste("Processing metric:", metric))
      
      # Fit a linear model for the selected metric
      formula_str <- paste0("`", metric, "` ~ Year")
      model <- lm(as.formula(formula_str), data = Data_Sheet1)
      
      # Create a data frame for future years
      future_years <- data.frame(Year = seq(max(Data_Sheet1$Year) + 1, 
                                            max(Data_Sheet1$Year) + input$years))
      
      # Predict future revenues
      future_years$Prediction <- predict(model, newdata = future_years)
      
      # Combine historical and future data into a single data frame
      historical_data <- Data_Sheet1 %>%
        select(Year, !!sym(metric)) %>%
        rename(Revenue = !!sym(metric))
      
      # Add a column to distinguish between historical and predicted data
      historical_data$Type <- "Historical"
      
      future_data <- future_years %>%
        rename(Revenue = Prediction) %>%
        mutate(Type = "Predicted")
      
      # Bind historical and future data
      all_data <- rbind(all_data, historical_data, future_data)
    }
    
    # Check if all_data has values
    print(head(all_data))  # Debugging line
    
    # Generate the plot if data exists
    if (nrow(all_data) > 0) {
      ggplot(all_data, aes(x = Year, y = Revenue, color = Type)) +
        geom_line() +
        geom_point() +
        labs(title = "Predictive Modeling of Revenues", y = "Revenue", x = "Year") +
        theme_minimal()
    } else {
      ggplot() + labs(title = "No Data to Display")
    }
  })
}

shinyApp(ui = ui, server = server)
