# Load required libraries
# Core Packages
library(shiny)
library(shinydashboard)

# Data Manipulation
library(dplyr)

# Data Visualization
library(ggplot2)
library(plotly)

# 1.1 Load .Rdata (You"ve already done this)
load("C:/Users/rudy.desplan/Downloads/R/AirBnB (1).Rdata")

# 1.2 Filter Irrelevant Columns

# Choose only relevant columns from L for our analysis
L <- L[, c("id", "host_id", "description", "minimum_nights", "price",
    "weekly_price", "monthly_price",
    "cleaning_fee", "zipcode", "property_type", "room_type", "bed_type",
    "accommodates", "bathrooms", "bedrooms", "beds",
    "guests_included", "neighbourhood_cleansed", "amenities", "number_of_reviews",
    "review_scores_rating","review_scores_accuracy", "review_scores_cleanliness",
    "review_scores_checkin","review_scores_communication","review_scores_location",
    "review_scores_value")]

# 1.3 Convert Data Types

# Convert from factor to numeric in L
L$price <- as.numeric(gsub("[$,]", "", as.character(L$price)))
L$weekly_price <- as.numeric(gsub("[$,]", "", as.character(L$weekly_price)))
L$monthly_price <- as.numeric(gsub("[$,]", "", as.character(L$monthly_price)))
L$cleaning_fee <- as.numeric(gsub("[$,]", "", as.character(L$cleaning_fee)))

# Create a new column 'number_amenities' based on the 'amenities' column
L$number_amenities <- sapply(strsplit(gsub("[{}\"]", "", L$amenities), ","), length)

# Handle cases where amenities are empty (i.e., "{}")
L$number_amenities[L$amenities == "{}"] <- 0

# Create a new column 'length_of_description' based on the 'description' column
L$length_of_description <- sapply(L$description, function(x) nchar(as.character(x)))

# Handle cases where description is NA or NULL
L$length_of_description[is.na(L$description)] <- 0

# Replace NA cleaning_fee with 0
L$cleaning_fee[is.na(L$cleaning_fee)] <- 0

# Convert `date` from factor to Date in R
R$date <- as.Date(as.character(R$date))


# 1.4 Data Integrity Checks

# Check for missing values in L
missing_values_by_column_L <- colSums(is.na(L))

# If you decide to impute missing values (mean for numeric columns, mode for categorical)

# Define UI for the app
ui <- dashboardPage(
  dashboardHeader(title = "AirBnB Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Price vs Features", tabName = "price_features"),
      menuItem("Apartments per Owner", tabName = "apartments_owner"),
      menuItem("Price per Quarter", tabName = "price_quarter"),
      menuItem("Visit Frequency", tabName = "visit_frequency")
    )
  ),
  dashboardBody(
    tabItems(
        # Price vs Features with feature selection
      tabItem(tabName = "price_features", 
              fluidRow(
                column(4, selectInput("propertyType", "Choose a Property Type:", choices = unique(L$property_type), selected = "All", multiple = TRUE)),
                column(4, selectInput("roomType", "Choose a Room Type:", choices = unique(L$room_type), selected = "All", multiple = TRUE)),
                column(4, selectInput("bedType", "Choose a Bed Type:", choices = unique(L$bed_type), selected = "All", multiple = TRUE)),
                column(4, selectInput("multipleFeatures", "Select Multiple Features:", 
                choices = c("accommodates", "bathrooms", "bedrooms", "beds","number_amenities","length_of_description","number_of_reviews","review_scores_rating","review_scores_accuracy", "review_scores_cleanliness","review_scores_checkin","review_scores_communication","review_scores_location","review_scores_value"), 
                multiple = TRUE, selected = "accommodates")),
                column(4, selectInput("scatterFeature", "Choose a feature for Scatter Plot:", choices = NULL)),
                column(8, plotlyOutput("corrPlot")),
                column(8, plotlyOutput("scatterPlot"))
              )),
        # Apartments per Owner with minimum number of apartments filter
      tabItem(tabName = "apartments_owner", 
              fluidRow(
                column(4, numericInput("minApartments", "Minimum number of apartments:", min = 1, value = 1)),
                column(4, numericInput("maxApartments", "Maximum number of apartments:", min = 1, value = 100)),
                column(4, selectInput("selectedPropertyType", "Select Property Type:", choices = unique(L$property_type), selected = "All", multiple = TRUE)),
                column(8, tableOutput("apartmentsTable"))
              )),
        # Price per Quarter with multiple quarter selection
      tabItem(tabName = "price_quarter", 
              fluidRow(
                column(4, selectInput("selectedAvgQuarter", "Select Quarters:", choices = unique(L$neighbourhood_cleansed), selected = "All", multiple = TRUE)),
                column(8, plotlyOutput("avgPricePlot"))
              )),
      tabItem(tabName = "visit_frequency", 
              fluidRow(
                column(4, dateRangeInput("dateRange", "Select Date Range:", start = min(R$date), end = max(R$date))),
                column(4, selectInput("selectedQuarter", "Select Quarter:", choices = unique(L$neighbourhood_cleansed), selected = "All", multiple = TRUE)),
                column(8, plotlyOutput("visitFreqPlot"))
              ))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "scatterFeature", choices = input$multipleFeatures)
  })
  
  # Dynamically calculate correlation matrix based on user input
  observe({
    selected_property_type <- input$propertyType
    selected_room_type <- input$roomType
    selected_bed_type <- input$bedType
    selected_features_for_corr <- input$multipleFeatures
    selected_feature_for_scatter <- input$scatterFeature
    
    # Filter data based on user input
    filtered_L <- L
    if (length(selected_property_type) > 0) {
        filtered_L <- filtered_L %>% filter(property_type %in% selected_property_type)
    }
    if (length(selected_room_type) > 0) {
        filtered_L <- filtered_L %>% filter(room_type %in% selected_room_type)
    }
    if (length(selected_bed_type) > 0) {
        filtered_L <- filtered_L %>% filter(bed_type %in% selected_bed_type)
    }

    # Calculate correlation matrix
    corr_matrix <- cor(filtered_L[, c("price", selected_features_for_corr)], use = "pairwise.complete.obs")

    # Convert correlation matrix into tidy data.frame
    corr_melted <- as.data.frame(as.table(corr_matrix))

    # Logic for generating correlation matrix based on selected_features_for_corr
    output$corrPlot <- renderPlotly({
        p <- ggplot(data = corr_melted, aes(x = Var1, y = Var2)) +
            geom_tile(aes(fill = Freq), color = "white") +
            geom_text(aes(label = sprintf("%.2f", Freq)), vjust = 1) +
            scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                midpoint = 0, limit = c(-1,1), space = "Lab", 
                                name="Correlation") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                            size = 12, hjust = 1)) +
            coord_fixed()
        ggplotly(p)
    })

    # Logic for scatter plot
    output$scatterPlot <- renderPlotly({
      if (!is.null(selected_feature_for_scatter) && selected_feature_for_scatter != "") {
        p_scatter <- ggplot(data = filtered_L, aes_string(x = selected_feature_for_scatter, y = "price")) +
                    geom_point(aes(color = price), alpha = 0.6) +
                    labs(title = paste("Scatter Plot of Price vs", selected_feature_for_scatter),
                         x = selected_feature_for_scatter,
                         y = "Price") +
                    theme_minimal()
        ggplotly(p_scatter)
      }
    })
  })

  # Dynamically filter number of apartments per owner based on user input
  observe({
    min_apartments <- input$minApartments
    max_apartments <- input$maxApartments
    selected_property_types <- input$selectedPropertyType
  
    apartments_per_owner <- L %>% group_by(host_id, property_type) %>% summarise(n_apartments = n_distinct(id))
  
    if (length(selected_property_types) > 0) {
      apartments_per_owner <- apartments_per_owner %>% filter(property_type %in% selected_property_types)
    }
  
    filtered_data <- apartments_per_owner %>% filter(n_apartments >= min_apartments & n_apartments <= max_apartments)
    sorted_data <- filtered_data %>% arrange(n_apartments, host_id)
  
    output$apartmentsTable <- renderTable({sorted_data})
  })



  # Calculate average renting price per city quarter
  avg_price_per_quarter_filtered <- reactive({
    selected_quarters <- input$selectedAvgQuarter
    avg_price_data <- L %>% group_by(neighbourhood_cleansed) %>% summarise(avg_price = mean(price + cleaning_fee, na.rm = TRUE))
  
    # Filter based on selected quarters
    if (length(selected_quarters) > 0) {
      avg_price_data <- avg_price_data %>% filter(neighbourhood_cleansed %in% selected_quarters)
    }
  
    # Sort the data
    avg_price_data <- avg_price_data %>% arrange(desc(avg_price))
    return(avg_price_data)

  })
  
    output$avgPricePlot <- renderPlotly({
        sorted_data <- avg_price_per_quarter_filtered()
        sorted_data$neighbourhood_cleansed <- factor(sorted_data$neighbourhood_cleansed, levels = sorted_data$neighbourhood_cleansed)
    
        p <- ggplot(sorted_data, aes(x = neighbourhood_cleansed, y = avg_price)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(p)
    })

    # Dynamically calculate visit frequency of different quarters based on user input
  observe({
    selected_dates <- input$dateRange
    selected_quarters <- input$selectedQuarter
    merged_data <- merge(L, R, by.x = "id", by.y = "listing_id")
    filtered_data <- merged_data %>% filter(date >= selected_dates[1] & date <= selected_dates[2])

    # If specific quarters are selected, filter the data accordingly
    if (length(selected_quarters) > 0) {
      filtered_data <- filtered_data %>% filter(neighbourhood_cleansed %in% selected_quarters)
    }

    visit_frequency <- filtered_data %>% group_by(neighbourhood_cleansed, date) %>% summarise(n_visits = n())
    output$visitFreqPlot <- renderPlotly({
      p <- ggplot(visit_frequency, aes(x = date, y = n_visits, color = neighbourhood_cleansed)) +
           geom_line()
      ggplotly(p)
    })
  })
}

# Run the app
shinyApp(ui, server)