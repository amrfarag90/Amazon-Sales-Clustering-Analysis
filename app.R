

library(shiny)
library(shinydashboard)
library(tidyverse)
library(caret)
library(mclust)
library(shinyWidgets)
library(DT)
library(rpart)
library(viridis)
library(openxlsx)


# 1. LOAD REAL DATA FROM CSV FILE


file_path <- "C:/Users/youss/Desktop/Project Data Analytics/Clustring/Amazon.csv"

tryCatch({
  # Read the CSV file
  cat("Loading data from:", file_path, "\n")
  df_raw <- read.csv(file_path)
  cat("Successfully loaded", nrow(df_raw), "rows and", ncol(df_raw), "columns\n")
  
  # Display basic information about the dataset
  cat("\nDataset Structure:\n")
  str(df_raw)
  cat("\nColumn Names:\n")
  print(names(df_raw))
  cat("\nFirst few rows:\n")
  print(head(df_raw))
  
}, error = function(e) {
  cat("Error loading file:", e$message, "\n")
  cat("Creating sample data as fallback...\n")
  
  # Create fallback sample data if file loading fails
  set.seed(123)
  n <- 1000
  df_raw <- data.frame(
    OrderID = sprintf("ORD%06d", 1:n),
    Category = sample(c("Electronics", "Clothing", "Home", "Books", "Beauty"), 
                      n, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.1)),
    TotalAmount = round(runif(n, 10, 500), 2),
    OrderStatus = sample(c("Delivered", "Cancelled", "Shipped", "Processing"), 
                         n, replace = TRUE, prob = c(0.7, 0.1, 0.15, 0.05)),
    PaymentMethod = sample(c("Credit Card", "PayPal", "Cash", "Amazon Pay"), 
                           n, replace = TRUE, prob = c(0.5, 0.3, 0.1, 0.1)),
    Quantity = sample(1:5, n, replace = TRUE),
    ShippingCost = round(runif(n, 0, 20), 2),
    Discount = round(runif(n, 0, 0.5), 2)
  )
})


# 2. DATA PREPARATION WITH COMPREHENSIVE PREPROCESSING


# Function for comprehensive data preprocessing
preprocess_amazon_data <- function(data) {
  cat("Starting Data Preprocessing...\n")
  cat("========================================\n")
  
  # Create a copy for preprocessing
  dff <- data
  
  # 1. Data Structure
  cat("1. Data Structure:\n")
  cat("   Rows:", nrow(dff), "\n")
  cat("   Columns:", ncol(dff), "\n")
  str(dff)
  cat("\n")
  
  # 2. Check for duplicates
  cat("2. Checking for Duplicates:\n")
  duplicated_count <- sum(duplicated(dff))
  cat("   Total duplicates found:", duplicated_count, "\n")
  
  if (duplicated_count > 0) {
    cat("   Removing duplicates...\n")
    dff_without_duplicates <- unique(dff)
    cat("   Remaining rows after removing duplicates:", nrow(dff_without_duplicates), "\n")
    dff <- dff_without_duplicates
  }
  cat("\n")
  
  # 3. Check for missing values (NA)
  cat("3. Checking for Missing Values:\n")
  na_count <- sum(is.na(dff))
  cat("   Total NA values:", na_count, "\n")
  
  if (na_count > 0) {
    cat("   Removing rows with missing values...\n")
    dff <- na.omit(dff)
    cat("   Remaining rows after removing NA:", nrow(dff), "\n")
    
    # Show columns with missing values
    na_by_col <- colSums(is.na(data))
    na_cols <- names(na_by_col[na_by_col > 0])
    if (length(na_cols) > 0) {
      cat("   Columns with missing values:", paste(na_cols, collapse = ", "), "\n")
    }
  }
  cat("\n")
  
  # 4. Handling Outliers for numeric columns
  cat("4. Handling Outliers:\n")
  
  # Identify numeric columns
  numeric_cols <- names(dff)[sapply(dff, is.numeric)]
  cat("   Numeric columns found:", paste(numeric_cols, collapse = ", "), "\n")
  
  outlier_stats <- list()
  
  for (col in numeric_cols) {
    if (col %in% names(dff)) {
      # Calculate IQR method for outlier detection
      Q1 <- quantile(dff[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(dff[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      outliers <- dff[[col]] < lower_bound | dff[[col]] > upper_bound
      outlier_count <- sum(outliers, na.rm = TRUE)
      
      if (outlier_count > 0) {
        cat(sprintf("   %s: %d outliers detected (%.1f%% of data)\n", 
                    col, outlier_count, (outlier_count/nrow(dff))*100))
        
        outlier_stats[[col]] <- list(
          count = outlier_count,
          percentage = (outlier_count/nrow(dff))*100,
          lower_bound = lower_bound,
          upper_bound = upper_bound,
          min_value = min(dff[[col]], na.rm = TRUE),
          max_value = max(dff[[col]], na.rm = TRUE)
        )
        
        # Display outlier records
        cat("     Outlier records:\n")
        print(dff[which(dff[[col]] %in% dff[[col]][outliers]), c("OrderID", col)])
        
        # Remove outliers
        dff <- dff[-which(dff[[col]] %in% dff[[col]][outliers]), ]
        cat(sprintf("     %d outliers removed\n", outlier_count))
      }
    }
  }
  cat("\n")
  
  # 5. Convert categorical variables to factors
  cat("5. Converting Categorical Variables:\n")
  
  # Identify categorical columns (non-numeric, non-date)
  categorical_cols <- names(dff)[!sapply(dff, is.numeric)]
  
  # Remove OrderDate if it exists (we'll handle separately)
  if ("OrderDate" %in% categorical_cols) {
    categorical_cols <- categorical_cols[categorical_cols != "OrderDate"]
  }
  
  # Also remove OrderID, CustomerID, ProductID, SellerID as they are identifiers
  id_cols <- c("OrderID", "CustomerID", "ProductID", "SellerID", 
               "CustomerName", "ProductName", "City", "State", "Country", "Brand")
  categorical_cols <- categorical_cols[!categorical_cols %in% id_cols]
  
  for (col in categorical_cols) {
    if (col %in% names(dff)) {
      dff[[col]] <- as.factor(dff[[col]])
      cat(sprintf("   %s converted to factor (%d levels)\n", 
                  col, length(levels(dff[[col]]))))
    }
  }
  cat("\n")
  
  # 6. Create additional features
  cat("6. Feature Engineering:\n")
  
  # Create new features if the required columns exist
  if ("Discount" %in% names(dff) && "TotalAmount" %in% names(dff)) {
    dff$TotalAfterDiscount <- dff$TotalAmount * (1 - dff$Discount)
    cat("   Created feature: TotalAfterDiscount\n")
  }
  
  if ("ShippingCost" %in% names(dff) && "TotalAmount" %in% names(dff)) {
    dff$ShippingRatio <- dff$ShippingCost / dff$TotalAmount
    dff$ShippingRatio <- ifelse(is.infinite(dff$ShippingRatio) | is.na(dff$ShippingRatio), 
                                0, dff$ShippingRatio)
    cat("   Created feature: ShippingRatio\n")
  }
  
  # Create order date features if OrderDate exists
  if ("OrderDate" %in% names(dff)) {
    tryCatch({
      dff$OrderDate <- as.Date(dff$OrderDate)
      dff$OrderYear <- year(dff$OrderDate)
      dff$OrderMonth <- month(dff$OrderDate)
      dff$OrderDay <- day(dff$OrderDate)
      dff$OrderWeekday <- weekdays(dff$OrderDate)
      cat("   Created date features: OrderYear, OrderMonth, OrderDay, OrderWeekday\n")
    }, error = function(e) {
      cat("   Warning: Could not parse OrderDate:", e$message, "\n")
    })
  }
  
  cat("\n========================================\n")
  cat("Preprocessing Complete!\n")
  cat("Final dataset dimensions:", nrow(dff), "rows ×", ncol(dff), "columns\n")
  
  return(list(
    data = dff,
    preprocessing_stats = list(
      duplicates_removed = duplicated_count,
      na_removed = ifelse(na_count > 0, na_count, 0),
      outliers_handled = outlier_stats,
      original_rows = nrow(data),
      final_rows = nrow(dff)
    )
  ))
}

# Apply preprocessing to the real data
preprocessed_result <- preprocess_amazon_data(df_raw)
df_processed <- preprocessed_result$data
preprocessing_stats <- preprocessed_result$preprocessing_stats

# Display key information about processed data
cat("\nProcessed Data Information:\n")
cat("Available columns:", paste(names(df_processed), collapse = ", "), "\n")
cat("Numeric columns:", paste(names(df_processed)[sapply(df_processed, is.numeric)], collapse = ", "), "\n")
cat("Factor columns:", paste(names(df_processed)[sapply(df_processed, is.factor)], collapse = ", "), "\n")



# 3. UI DESIGN - ENHANCED WITH PREPROCESSING TAB


ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Amazon Sales Analytics Dashboard",
    titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Detailed Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Data Preprocessing", tabName = "preprocessing", icon = icon("database")),
      menuItem("GMM Clustering", tabName = "clustering", icon = icon("project-diagram"))
    ),
    
    br(),
    
    # Filters Section
    box(
      title = strong("SALES FILTERS"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      style = "background-color: #f8f9fa;",
      
      # Category Filter
      h5(strong("Category Filter"), style = "color:#2c3e50; margin-top: 10px;"),
      selectizeInput(
        "category_filter", 
        label = NULL,
        choices = c("All", if("Category" %in% names(df_processed)) levels(df_processed$Category) else unique(df_processed$Category)),
        selected = "All",
        multiple = TRUE,
        options = list(
          placeholder = 'Select categories',
          'plugins' = list('remove_button'),
          'create' = FALSE
        )
      ),
      
      hr(style = "border-color: #ddd; margin: 10px 0;"),
      
      # Status Filter
      h5(strong("Order Status Filter"), style = "color: #2c3e50;"),
      div(style = "background-color: white; color: black; padding: 8px; border-radius: 5px; border: 1px solid #ddd;",
          checkboxGroupInput(
            "status_filter",
            label = NULL,
            choices = if("OrderStatus" %in% names(df_processed)) levels(df_processed$OrderStatus) else unique(df_processed$OrderStatus),
            selected = if("OrderStatus" %in% names(df_processed)) levels(df_processed$OrderStatus) else unique(df_processed$OrderStatus),
            inline = FALSE
          )
      ),
      
      hr(style = "border-color: #ddd; margin: 10px 0;"),
      
      # Amount Filter
      h5(strong("Amount Range ($)"), style = "color: #2c3e50;"),
      div(style = "padding: 0 10px;",
          sliderInput(
            "amount_filter",
            label = NULL,
            min = floor(min(df_processed$TotalAmount)),
            max = ceiling(max(df_processed$TotalAmount)),
            value = c(min(df_processed$TotalAmount), max(df_processed$TotalAmount)),
            ticks = TRUE
          )
      ),
      
      hr(style = "border-color: #ddd; margin: 10px 0;"),
      
      # Quantity Filter
      if("Quantity" %in% names(df_processed)) {
        tagList(
          h5(strong("Quantity Range"), style = "color: #2c3e50;"),
          div(style = "padding: 0 10px;",
              sliderInput(
                "quantity_filter",
                label = NULL,
                min = min(df_processed$Quantity),
                max = max(df_processed$Quantity),
                value = c(min(df_processed$Quantity), max(df_processed$Quantity)),
                step = 1
              )
          ),
          hr(style = "border-color: #ddd; margin: 10px 0;")
        )
      },
      
      # Payment Method Filter
      h5(strong("Payment Method"), style = "color: #2c3e50;"),
      pickerInput(
        inputId = "payment_filter",
        label = NULL,
        choices = if("PaymentMethod" %in% names(df_processed)) levels(df_processed$PaymentMethod) else unique(df_processed$PaymentMethod),
        selected = if("PaymentMethod" %in% names(df_processed)) levels(df_processed$PaymentMethod) else unique(df_processed$PaymentMethod),
        options = list(
          `actions-box` = TRUE,
          `deselect-all-text` = "Clear All",
          `select-all-text` = "Select All",
          `none-selected-text` = "All Methods"
        ),
        multiple = TRUE
      ),
      
      hr(style = "border-color: #ddd; margin: 15px 0;"),
      
      # Action Buttons
      fluidRow(
        column(6,
               actionButton("apply_filters", "Apply", 
                            class = "btn-primary",
                            icon = icon("filter"),
                            width = "100%")
        ),
        column(6,
               actionButton("reset_filters", "Reset", 
                            class = "btn-warning",
                            icon = icon("redo"),
                            width = "100%")
        )
      ),
      
      hr(style = "border-color: #ddd; margin: 15px 0;"),
      
      # Current Selection Summary
      div(
        style = "background-color: #e8f4f8; padding: 10px; border-radius: 5px; border: 1px solid #b3d9ff;",
        h6(strong("Current Selection:"), style = "color: #0056b3; margin-bottom: 5px;"),
        uiOutput("filter_summary")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        .info-box {
          border-radius: 8px;
          margin-bottom: 15px;
        }
        .btn-primary {
          background-color: #232f3e;
          border-color: #232f3e;
        }
        .btn-primary:hover {
          background-color: #37475a;
          border-color: #37475a;
        }
        .btn-warning {
          background-color: #ff9900;
          border-color: #ff9900;
          color: white;
        }
        .btn-warning:hover {
          background-color: #e68900;
          border-color: #e68900;
          color = white;
        }
        .irs-bar {
          background-color: #232f3e;
          border-color: #232f3e;
        }
        .irs-from, .irs-to, .irs-single {
          background-color: #232f3e;
        }
        .checkbox {
          margin-top: 5px;
          margin-bottom: 5px;
        }
        .preprocessing-step {
          padding: 10px;
          margin: 5px 0;
          border-radius: 5px;
          background-color: #f8f9fa;
          border-left: 4px solid #232f3e;
        }
        .cluster-info {
          padding: 15px;
          margin: 10px 0;
          border-radius: 8px;
          background-color: #f0f8ff;
          border: 1px solid #d1e7ff;
        }
      "))
    ),
    
    tabItems(
      #  TAB 1: Overview 
      tabItem(
        tabName = "overview",
        h2(strong("Sales Overview Dashboard"), style = "color: #232f3e;"),
        
        # KPI Boxes
        fluidRow(
          infoBoxOutput("total_revenue_box", width = 3),
          infoBoxOutput("total_orders_box", width = 3),
          infoBoxOutput("avg_order_box", width = 3),
          infoBoxOutput("success_rate_box", width = 3)
        ),
        
        # First Row of Plots
        fluidRow(
          box(
            title = strong("Sales by Category (Bar Plot)"),
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("plot_bar", height = "400px"),
            footer = "Distribution of orders across different product categories"
          ),
          box(
            title = strong("Order Status Distribution (Pie Chart)"),
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("plot_pie", height = "400px"),
            footer = "Percentage breakdown of order statuses"
          )
        ),
        
        # Second Row of Plots
        fluidRow(
          box(
            title = strong("Order Value Distribution (Histogram)"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotOutput("plot_hist", height = "300px"),
            footer = "Frequency distribution of order amounts"
          )
        )
      ),
      
      #  TAB 2: Detailed Analysis
      tabItem(
        tabName = "analysis",
        h2(strong("Detailed Sales Analysis"), style = "color: #232f3e;"),
        
        fluidRow(
          box(
            title = strong("Discount vs Total Amount (Scatter Plot)"),
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotOutput("plot_scatter", height = "400px"),
            footer = "Relationship between discount percentage and order total"
          ),
          box(
            title = strong("Order Value by Payment Method (Box Plot)"),
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            plotOutput("plot_box", height = "400px"),
            footer = "Comparison of order values across different payment methods"
          )
        ),
        
        fluidRow(
          box(
            title = strong("Sales Data Table"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            DTOutput("data_table"),
            br(),
            fluidRow(
              column(4, downloadButton("download_csv", "Download CSV", class = "btn-success")),
              column(4, downloadButton("download_excel", "Download Excel", class = "btn-primary")),
              column(4, actionButton("show_stats", "Show Data Stats", class = "btn-info"))
            )
          )
        )
      ),
      
      #  TAB 3: Data Preprocessing
      tabItem(
        tabName = "preprocessing",
        h2(strong("Data Preprocessing Dashboard"), style = "color: #232f3e;"),
        
        fluidRow(
          valueBoxOutput("preprocessing_original", width = 3),
          valueBoxOutput("preprocessing_duplicates", width = 3),
          valueBoxOutput("preprocessing_missing", width = 3),
          valueBoxOutput("preprocessing_final", width = 3)
        ),
        
        fluidRow(
          box(
            title = strong("Preprocessing Steps"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            div(class = "preprocessing-step",
                h4(icon("search"), "1. Duplicate Detection"),
                p(paste("Found", preprocessing_stats$duplicates_removed, "duplicate records")),
                verbatimTextOutput("duplicate_info")
            ),
            div(class = "preprocessing-step",
                h4(icon("exclamation-triangle"), "2. Missing Values"),
                p(paste("Removed", preprocessing_stats$na_removed, "rows with missing values")),
                verbatimTextOutput("missing_info")
            ),
            div(class = "preprocessing-step",
                h4(icon("filter"), "3. Outlier Detection"),
                p("Outliers detected using IQR method and removed"),
                verbatimTextOutput("outlier_info")
            ),
            div(class = "preprocessing-step",
                h4(icon("cog"), "4. Data Transformation"),
                p("Categorical variables converted to factors, new features created"),
                verbatimTextOutput("transformation_info")
            )
          )
        ),
        
        fluidRow(
          box(
            title = strong("Before Preprocessing"),
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotOutput("boxplot_raw", height = "300px"),
            footer = "Raw data with outliers"
          ),
          box(
            title = strong("After Preprocessing"),
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("boxplot_processed", height = "300px"),
            footer = "Cleaned data without outliers"
          )
        ),
        
        fluidRow(
          box(
            title = strong("Data Comparison"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            DTOutput("comparison_table"),
            br(),
            actionButton("view_raw", "View Raw Data", icon = icon("eye"), class = "btn-warning"),
            actionButton("view_processed", "View Processed Data", icon = icon("check"), class = "btn-success")
          )
        )
      ),
      
      # ========== TAB 4: GMM Clustering ==========
      tabItem(
        tabName = "clustering",
        h2(strong("Customer Segmentation using Gaussian Mixture Models"), style = "color: #232f3e;"),
        
        fluidRow(
          box(
            title = strong("GMM Configuration"),
            status = "info",
            solidHeader = TRUE,
            width = 4,
            selectInput("gmm_var1", "Variable 1:", 
                        choices = c("TotalAmount", "Quantity", "ShippingCost", "Discount", 
                                    "TotalAfterDiscount", "ShippingRatio"),
                        selected = "TotalAmount"),
            selectInput("gmm_var2", "Variable 2:", 
                        choices = c("TotalAmount", "Quantity", "ShippingCost", "Discount",
                                    "TotalAfterDiscount", "ShippingRatio"),
                        selected = "ShippingCost"),
            selectInput("gmm_model", "Model Type:",
                        choices = c("EII" = "EII", "VII" = "VII", "EEI" = "EEI",
                                    "VEI" = "VEI", "EVI" = "EVI", "VVI" = "VVI",
                                    "EEE" = "EEE", "VEE" = "VEE", "EVE" = "EVE",
                                    "VVE" = "VVE", "EEV" = "EEV", "VEV" = "VEV",
                                    "EVV" = "EVV", "VVV" = "VVV",
                                    "Automatic" = "auto"),
                        selected = "VVV"),
            sliderInput("gmm_clusters", "Number of Clusters:", 
                        min = 1, max = 10, value = 3),
            checkboxInput("gmm_bic", "Show BIC Plot", value = FALSE),
            hr(),
            actionButton("run_gmm", "Run GMM Clustering", 
                         icon = icon("play"), 
                         class = "btn-success btn-block"),
            br(), br(),
            p("Gaussian Mixture Models (GMM) use probability to assign data points to clusters. 
              Each cluster is represented by a Gaussian distribution.", 
              style = "color: #666; font-size: 0.9em;")
          ),
          
          box(
            title = strong("Cluster Visualization"),
            status = "success",
            solidHeader = TRUE,
            width = 8,
            plotOutput("plot_gmm", height = "400px"),
            conditionalPanel(
              condition = "input.gmm_bic == true",
              plotOutput("plot_bic", height = "300px")
            ),
            footer = "GMM clustering visualization showing customer segments"
          )
        ),
        
        fluidRow(
          box(
            title = strong("Cluster Analysis"),
            status = "warning",
            solidHeader = TRUE,
            width = 8,
            div(class = "cluster-info",
                h4(icon("chart-pie"), "Cluster Distribution"),
                plotOutput("cluster_distribution", height = "200px")
            ),
            div(class = "cluster-info",
                h4(icon("table"), "Cluster Characteristics"),
                DTOutput("cluster_table")
            )
          ),
          
          box(
            title = strong("GMM Model Details"),
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            verbatimTextOutput("gmm_summary"),
            br(),
            downloadButton("download_clusters", "Download Cluster Assignments", 
                           class = "btn-primary btn-block")
          )
        ),
        
        fluidRow(
          box(
            title = strong("Cluster Profiles"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            uiOutput("cluster_profiles")
          )
        )
      )
    )
  )
)


# 4. SERVER LOGIC


server <- function(input, output, session) {
  
  # Reactive data with automatic updates
  filtered_data <- reactive({
    data <- df_processed
    
    # Filter by category
    if ("Category" %in% names(data)) {
      if (!is.null(input$category_filter) && !"All" %in% input$category_filter) {
        data <- data %>% filter(Category %in% input$category_filter)
      }
    }
    
    # Filter by status
    if ("OrderStatus" %in% names(data)) {
      if (!is.null(input$status_filter)) {
        data <- data %>% filter(OrderStatus %in% input$status_filter)
      }
    }
    
    # Filter by amount
    if ("TotalAmount" %in% names(data)) {
      data <- data %>% 
        filter(TotalAmount >= input$amount_filter[1] & 
                 TotalAmount <= input$amount_filter[2])
    }
    
    # Filter by quantity
    if ("Quantity" %in% names(data)) {
      data <- data %>% 
        filter(Quantity >= input$quantity_filter[1] & 
                 Quantity <= input$quantity_filter[2])
    }
    
    # Filter by payment method
    if ("PaymentMethod" %in% names(data)) {
      if (!is.null(input$payment_filter)) {
        data <- data %>% filter(PaymentMethod %in% input$payment_filter)
      }
    }
    
    return(data)
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    if ("Category" %in% names(df_processed)) {
      updateSelectizeInput(session, "category_filter", selected = "All")
    }
    
    if ("OrderStatus" %in% names(df_processed)) {
      updateCheckboxGroupInput(session, "status_filter", 
                               selected = if(is.factor(df_processed$OrderStatus)) 
                                 levels(df_processed$OrderStatus) 
                               else unique(df_processed$OrderStatus))
    }
    
    if ("TotalAmount" %in% names(df_processed)) {
      updateSliderInput(session, "amount_filter", 
                        value = c(min(df_processed$TotalAmount), max(df_processed$TotalAmount)))
    }
    
    if ("Quantity" %in% names(df_processed)) {
      updateSliderInput(session, "quantity_filter", 
                        value = c(min(df_processed$Quantity), max(df_processed$Quantity)))
    }
    
    if ("PaymentMethod" %in% names(df_processed)) {
      updatePickerInput(session, "payment_filter", 
                        selected = if(is.factor(df_processed$PaymentMethod)) 
                          levels(df_processed$PaymentMethod) 
                        else unique(df_processed$PaymentMethod))
    }
  })
  

  # PREPROCESSING OUTPUTS

  
  output$preprocessing_original <- renderValueBox({
    valueBox(
      value = preprocessing_stats$original_rows,
      subtitle = "Original Rows",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$preprocessing_duplicates <- renderValueBox({
    valueBox(
      value = preprocessing_stats$duplicates_removed,
      subtitle = "Duplicates Removed",
      icon = icon("copy"),
      color = "yellow"
    )
  })
  
  output$preprocessing_missing <- renderValueBox({
    valueBox(
      value = preprocessing_stats$na_removed,
      subtitle = "Missing Values",
      icon = icon("question-circle"),
      color = "orange"
    )
  })
  
  output$preprocessing_final <- renderValueBox({
    valueBox(
      value = preprocessing_stats$final_rows,
      subtitle = "Final Clean Rows",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$duplicate_info <- renderPrint({
    cat("Duplicates removed:", preprocessing_stats$duplicates_removed, "\n")
    cat("Percentage of data:", 
        round((preprocessing_stats$duplicates_removed/preprocessing_stats$original_rows)*100, 2), "%\n")
  })
  
  output$missing_info <- renderPrint({
    cat("Rows with NA:", preprocessing_stats$na_removed, "\n")
    cat("Percentage of data:", 
        round((preprocessing_stats$na_removed/preprocessing_stats$original_rows)*100, 2), "%\n")
  })
  
  output$outlier_info <- renderPrint({
    if (length(preprocessing_stats$outliers_handled) > 0) {
      cat("Outliers handled in columns:\n")
      for (col in names(preprocessing_stats$outliers_handled)) {
        stats <- preprocessing_stats$outliers_handled[[col]]
        cat(sprintf("  %s: %d outliers (%.1f%%)\n", 
                    col, stats$count, stats$percentage))
      }
    } else {
      cat("No outliers detected using IQR method\n")
    }
  })
  
  output$transformation_info <- renderPrint({
    cat("Categorical variables converted to factors:\n")
    
    # Find factor columns
    factor_cols <- names(df_processed)[sapply(df_processed, is.factor)]
    for (col in factor_cols) {
      cat(sprintf("  %s: %d levels\n", col, length(levels(df_processed[[col]]))))
    }
    
    cat("\nNew features created:\n")
    if ("TotalAfterDiscount" %in% names(df_processed)) cat("  TotalAfterDiscount\n")
    if ("ShippingRatio" %in% names(df_processed)) cat("  ShippingRatio\n")
    if ("OrderYear" %in% names(df_processed)) cat("  OrderYear, OrderMonth, OrderDay, OrderWeekday\n")
  })
  
  output$boxplot_raw <- renderPlot({
    # Select numeric columns for boxplot
    numeric_cols <- names(df_raw)[sapply(df_raw, is.numeric)]
    if (length(numeric_cols) > 0) {
      plot_cols <- head(numeric_cols, 5)  # Show up to 5 numeric columns
      boxplot(df_raw[, plot_cols],
              main = "Raw Data with Outliers",
              col = rainbow(length(plot_cols)),
              ylab = "Value",
              las = 2)
    }
  })
  
  output$boxplot_processed <- renderPlot({
    # Select numeric columns for boxplot
    numeric_cols <- names(df_processed)[sapply(df_processed, is.numeric)]
    if (length(numeric_cols) > 0) {
      plot_cols <- head(numeric_cols, 5)  # Show up to 5 numeric columns
      boxplot(df_processed[, plot_cols],
              main = "Cleaned Data (Outliers Removed)",
              col = rainbow(length(plot_cols)),
              ylab = "Value",
              las = 2)
    }
  })
  
  # Data comparison table
  show_raw <- reactiveVal(FALSE)
  
  observeEvent(input$view_raw, { show_raw(TRUE) })
  observeEvent(input$view_processed, { show_raw(FALSE) })
  
  output$comparison_table <- renderDT({
    if (show_raw()) {
      data <- head(df_raw, 50)  # Show first 50 rows of raw data
      caption <- "Raw Data Sample (First 50 Rows)"
    } else {
      data <- head(df_processed, 50)  # Show first 50 rows of processed data
      caption <- "Processed Data Sample (First 50 Rows)"
    }
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      class = 'display nowrap stripe hover',
      rownames = FALSE,
      caption = tags$caption(h4(caption), style = "text-align: center;")
    )
  })
  

  # FILTER SUMMARY & KPI BOXES

  
  output$filter_summary <- renderUI({
    dat <- filtered_data()
    categories_selected <- if("All" %in% input$category_filter || is.null(input$category_filter)) {
      "All"
    } else {
      paste(length(input$category_filter), "selected")
    }
    
    status_selected <- if(is.null(input$status_filter)) {
      "0 selected"
    } else {
      paste(length(input$status_filter), "selected")
    }
    
    total_amount <- if("TotalAmount" %in% names(dat)) sum(dat$TotalAmount) else 0
    
    tagList(
      p(icon("shopping-cart"), paste(" Orders:", nrow(dat))),
      p(icon("dollar-sign"), paste(" Revenue: $", format(round(total_amount, 0), big.mark = ","))),
      p(icon("tag"), paste(" Categories:", categories_selected)),
      p(icon("check-circle"), paste(" Status:", status_selected))
    )
  })
  
  output$total_revenue_box <- renderInfoBox({
    dat <- filtered_data()
    revenue <- if("TotalAmount" %in% names(dat)) sum(dat$TotalAmount) else 0
    infoBox(
      title = "Total Revenue",
      value = paste0("$", format(round(revenue, 0), big.mark = ",")),
      icon = icon("dollar-sign"),
      color = "green",
      fill = TRUE
    )
  })
  
  output$total_orders_box <- renderInfoBox({
    orders <- nrow(filtered_data())
    infoBox(
      title = "Total Orders",
      value = format(orders, big.mark = ","),
      icon = icon("shopping-cart"),
      color = "blue",
      fill = TRUE
    )
  })
  
  output$avg_order_box <- renderInfoBox({
    dat <- filtered_data()
    avg <- if("TotalAmount" %in% names(dat) && nrow(dat) > 0) {
      mean(dat$TotalAmount)
    } else 0
    infoBox(
      title = "Avg Order Value",
      value = paste0("$", round(avg, 2)),
      icon = icon("chart-line"),
      color = "light-blue",
      fill = TRUE
    )
  })
  
  output$success_rate_box <- renderInfoBox({
    dat <- filtered_data()
    success_pct <- if(nrow(dat) > 0 && "OrderStatus" %in% names(dat)) {
      round(sum(dat$OrderStatus == "Delivered") / nrow(dat) * 100, 1)
    } else 0
    
    infoBox(
      title = "Success Rate",
      value = paste0(success_pct, "%"),
      icon = icon("check-circle"),
      color = if(success_pct > 80) "green" else "yellow",
      fill = TRUE
    )
  })
  

  
  
  
  # GMM CLUSTERING

  
  # Reactive for GMM results
  gmm_results <- reactiveValues(
    model = NULL,
    data = NULL,
    clusters = NULL
  )
  
  # Run GMM when button is clicked
  observeEvent(input$run_gmm, {
    req(input$gmm_var1, input$gmm_var2)
    
    # Check if selected variables exist in the dataset
    if (!(input$gmm_var1 %in% names(df_processed) && input$gmm_var2 %in% names(df_processed))) {
      showNotification("Selected variables not found in dataset", type = "error")
      return()
    }
    
    # Get selected data
    dat_cluster <- filtered_data()[, c(input$gmm_var1, input$gmm_var2)]
    dat_cluster <- na.omit(dat_cluster)
    
    if (nrow(dat_cluster) < 10) {
      showNotification("Not enough data for clustering", type = "warning")
      return()
    }
    
    # Run GMM
    tryCatch({
      if (input$gmm_model == "auto") {
        fit <- Mclust(dat_cluster, G = input$gmm_clusters, verbose = FALSE)
      } else {
        fit <- Mclust(dat_cluster, G = input$gmm_clusters, 
                      modelNames = input$gmm_model, verbose = FALSE)
      }
      
      # Store results
      gmm_results$model <- fit
      gmm_results$data <- dat_cluster
      gmm_results$clusters <- fit$classification
      
      showNotification("GMM clustering completed successfully!", type = "success")
      
    }, error = function(e) {
      showNotification(paste("Error in GMM:", e$message), type = "error")
    })
  })
  
  # GMM Plot
  output$plot_gmm <- renderPlot({
    req(gmm_results$model)
    
    classification <- gmm_results$clusters
    plot_df <- data.frame(
      x = gmm_results$data[,1],
      y = gmm_results$data[,2],
      cluster = as.factor(classification)
    )
    
    ggplot(plot_df, aes(x = x, y = y, color = cluster)) +
      geom_point(alpha = 0.7, size = 3) +
      theme_minimal() +
      labs(x = input$gmm_var1, y = input$gmm_var2,
           title = paste("GMM Clustering (G =", input$gmm_clusters, ")"),
           color = "Cluster") +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14)) +
      scale_color_viridis_d() +
      stat_ellipse(level = 0.95, alpha = 0.2, size = 1)
  })
  
  # BIC Plot
  output$plot_bic <- renderPlot({
    req(input$gmm_bic, gmm_results$model)
    
    plot(gmm_results$model, what = "BIC", 
         main = "Bayesian Information Criterion (BIC)")
  })
  
  # Cluster Distribution Plot
  output$cluster_distribution <- renderPlot({
    req(gmm_results$clusters)
    
    cluster_counts <- table(gmm_results$clusters)
    plot_df <- data.frame(
      Cluster = names(cluster_counts),
      Count = as.numeric(cluster_counts),
      Percentage = as.numeric(cluster_counts) / sum(cluster_counts) * 100
    )
    
    ggplot(plot_df, aes(x = Cluster, y = Count, fill = Cluster)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")), 
                vjust = -0.3, size = 4) +
      theme_minimal() +
      labs(title = "Cluster Size Distribution") +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold")) +
      scale_fill_viridis_d()
  })
  
  # Cluster Characteristics Table
  output$cluster_table <- renderDT({
    req(gmm_results$model, gmm_results$data, gmm_results$clusters)
    
    # Combine data with clusters
    combined_data <- cbind(gmm_results$data, Cluster = gmm_results$clusters)
    
    # Calculate statistics for each cluster
    cluster_stats <- combined_data %>%
      group_by(Cluster) %>%
      summarise(
        Count = n(),
        Percentage = round(n()/nrow(combined_data)*100, 1),
        Mean_X = round(mean(.[[1]]), 2),
        SD_X = round(sd(.[[1]]), 2),
        Mean_Y = round(mean(.[[2]]), 2),
        SD_Y = round(sd(.[[2]]), 2),
        Min_X = round(min(.[[1]]), 2),
        Max_X = round(max(.[[1]]), 2),
        Min_Y = round(min(.[[2]]), 2),
        Max_Y = round(max(.[[2]]), 2)
      ) %>%
      mutate(Description = case_when(
        Mean_X > quantile(combined_data[[1]], 0.75) & Mean_Y > quantile(combined_data[[2]], 0.75) ~ "High-High",
        Mean_X > quantile(combined_data[[1]], 0.75) & Mean_Y < quantile(combined_data[[2]], 0.25) ~ "High-Low",
        Mean_X < quantile(combined_data[[1]], 0.25) & Mean_Y > quantile(combined_data[[2]], 0.75) ~ "Low-High",
        Mean_X < quantile(combined_data[[1]], 0.25) & Mean_Y < quantile(combined_data[[2]], 0.25) ~ "Low-Low",
        TRUE ~ "Mixed"
      ))
    
    datatable(
      cluster_stats,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      class = 'display nowrap stripe hover',
      rownames = FALSE,
      caption = "Statistical Characteristics of Each Cluster"
    ) %>%
      formatStyle(
        'Percentage',
        background = styleColorBar(cluster_stats$Percentage, 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # GMM Summary
  output$gmm_summary <- renderPrint({
    req(gmm_results$model)
    
    fit <- gmm_results$model
    
    cat("GAUSSIAN MIXTURE MODEL SUMMARY\n")
    cat("==============================\n\n")
    
    cat("Model Information:\n")
    cat("  Model Name:", fit$modelName, "\n")
    cat("  Number of Components (G):", fit$G, "\n")
    cat("  Variables:", input$gmm_var1, "and", input$gmm_var2, "\n")
    cat("  Observations:", nrow(gmm_results$data), "\n\n")
    
    cat("Log-likelihood:", round(fit$loglik, 2), "\n")
    cat("BIC:", round(fit$bic, 2), "\n")
    cat("Number of Parameters:", fit$df, "\n\n")
    
    cat("Mixing Probabilities:\n")
    for(i in 1:fit$G) {
      cat(sprintf("  Component %d: %.3f\n", i, fit$parameters$pro[i]))
    }
    cat("\n")
    
    cat("Component Means:\n")
    print(round(fit$parameters$mean, 2))
  })
  
  # Cluster Profiles
  output$cluster_profiles <- renderUI({
    req(gmm_results$clusters)
    
    # Get original data with clusters
    dat_with_clusters <- cbind(filtered_data()[rownames(gmm_results$data), ], 
                               Cluster = gmm_results$clusters,
                               Probability = apply(gmm_results$model$z, 1, max))
    
    profiles <- list()
    
    for(cluster in unique(gmm_results$clusters)) {
      cluster_data <- dat_with_clusters[dat_with_clusters$Cluster == cluster, ]
      
      profile <- div(
        class = "cluster-info",
        h4(icon("users"), paste("Cluster", cluster, "Profile")),
        p(strong("Size:"), nrow(cluster_data), 
          paste0("(", round(nrow(cluster_data)/nrow(dat_with_clusters)*100, 1), "%)")),
        p(strong("Avg Probability:"), round(mean(cluster_data$Probability, na.rm = TRUE), 3)),
        
        fluidRow(
          column(3,
                 if("Category" %in% names(cluster_data)) {
                   tagList(
                     h5("Demographics:"),
                     p("Most Common Category:", 
                       names(sort(table(cluster_data$Category), decreasing = TRUE))[1])
                   )
                 }
          ),
          column(3,
                 if("PaymentMethod" %in% names(cluster_data)) {
                   tagList(
                     h5("Payment:"),
                     p("Most Common Payment:", 
                       names(sort(table(cluster_data$PaymentMethod), decreasing = TRUE))[1])
                   )
                 }
          ),
          column(3,
                 if("TotalAmount" %in% names(cluster_data)) {
                   tagList(
                     h5("Financial:"),
                     p("Avg Total Amount: $", round(mean(cluster_data$TotalAmount), 2))
                   )
                 }
          ),
          column(3,
                 if("OrderStatus" %in% names(cluster_data)) {
                   tagList(
                     h5("Behavioral:"),
                     p("Success Rate:", 
                       round(sum(cluster_data$OrderStatus == "Delivered")/nrow(cluster_data)*100, 1), "%")
                   )
                 }
          )
        ),
        hr()
      )
      
      profiles[[cluster]] <- profile
    }
    
    do.call(tagList, profiles)
  })
  
  # Download cluster assignments
  output$download_clusters <- downloadHandler(
    filename = function() {
      paste("gmm_clusters_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(gmm_results$clusters)
      
      # Create dataset with clusters
      dat_with_clusters <- cbind(
        filtered_data()[rownames(gmm_results$data), ],
        Cluster = gmm_results$clusters,
        Probability = apply(gmm_results$model$z, 1, max)
      )
      
      write.csv(dat_with_clusters, file, row.names = FALSE)
    }
  )
  

  # PLOTS

  
  # 1. Bar Plot
  output$plot_bar <- renderPlot({
    dat <- filtered_data()
    
    if(nrow(dat) > 0 && "Category" %in% names(dat)) {
      plot_data <- dat %>%
        group_by(Category) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count))
      
      ggplot(plot_data, aes(x = reorder(Category, Count), y = Count, fill = Category)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme_minimal() +
        labs(x = "Category", y = "Number of Orders", 
             title = "Sales Distribution by Category") +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14)) +
        scale_fill_viridis_d(option = "C") +
        geom_text(aes(label = Count), hjust = -0.2, size = 4.5, fontface = "bold")
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No category data available with current filters", 
                 size = 5, color = "gray") +
        theme_void()
    }
  })
  
  # 2. Pie Chart
  output$plot_pie <- renderPlot({
    dat <- filtered_data()
    
    if(nrow(dat) > 0 && "OrderStatus" %in% names(dat)) {
      plot_data <- dat %>%
        group_by(OrderStatus) %>%
        summarise(Count = n()) %>%
        mutate(Percentage = Count / sum(Count) * 100,
               Label = paste0(round(Percentage, 1), "%\n(", Count, ")"))
      
      plot_data <- plot_data %>%
        arrange(desc(OrderStatus)) %>%
        mutate(Position = cumsum(Percentage) - Percentage/2)
      
      ggplot(plot_data, aes(x = "", y = Percentage, fill = OrderStatus)) +
        geom_bar(stat = "identity", width = 1, color = "white", size = 0.5) +
        coord_polar("y", start = 0) +
        theme_void() +
        labs(title = "Order Status Distribution") +
        scale_fill_manual(values = c("Delivered" = "#27ae60", 
                                     "Shipped" = "#3498db", 
                                     "Processing" = "#f39c12", 
                                     "Cancelled" = "#e74c3c",
                                     "Pending" = "#f39c12",
                                     "Returned" = "#e74c3c")) +
        geom_text(aes(label = Label, y = Position), 
                  color = "white", size = 5, fontface = "bold") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              legend.title = element_text(face = "bold", size = 12),
              legend.text = element_text(size = 11))
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No order status data available with current filters", 
                 size = 5, color = "gray") +
        theme_void()
    }
  })
  
  # 3. Histogram
  output$plot_hist <- renderPlot({
    dat <- filtered_data()
    
    if(nrow(dat) > 0 && "TotalAmount" %in% names(dat)) {
      ggplot(dat, aes(x = TotalAmount)) +
        geom_histogram(fill = "#232f3e", color = "white", bins = 30, alpha = 0.8) +
        theme_minimal() +
        labs(x = "Total Amount ($)", y = "Frequency",
             title = "Order Value Distribution") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14)) +
        scale_x_continuous(labels = scales::dollar_format())
    } else {
      ggplot() +
        annotate("text", x = 250, y = 10, 
                 label = "No amount data available with current filters", 
                 size = 5, color = "gray") +
        theme_minimal() +
        labs(x = "Total Amount ($)", y = "Frequency")
    }
  })
  
  # 4. Scatter Plot
  output$plot_scatter <- renderPlot({
    dat <- filtered_data()
    
    if(nrow(dat) > 0 && "Discount" %in% names(dat) && "TotalAmount" %in% names(dat)) {
      sample_size <- min(300, nrow(dat))
      sample_df <- dat[sample(nrow(dat), sample_size), ]
      
      ggplot(sample_df, aes(x = Discount, y = TotalAmount, color = OrderStatus)) +
        geom_point(alpha = 0.7, size = 3) +
        theme_minimal() +
        labs(x = "Discount", y = "Total Amount ($)",
             title = "Discount vs. Total Amount") +
        scale_color_manual(values = c("Delivered" = "#27ae60", 
                                      "Shipped" = "#3498db", 
                                      "Processing" = "#f39c12", 
                                      "Cancelled" = "#e74c3c")) +
        scale_y_continuous(labels = scales::dollar_format()) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              legend.position = "bottom",
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14))
    } else {
      ggplot() +
        annotate("text", x = 0.25, y = 250, 
                 label = "No discount or amount data available with current filters", 
                 size = 5, color = "gray") +
        theme_minimal() +
        labs(x = "Discount", y = "Total Amount ($)")
    }
  })
  
  # 5. Box Plot
  output$plot_box <- renderPlot({
    dat <- filtered_data()
    
    if(nrow(dat) > 0 && "PaymentMethod" %in% names(dat) && "TotalAmount" %in% names(dat)) {
      ggplot(dat, aes(x = PaymentMethod, y = TotalAmount, fill = PaymentMethod)) +
        geom_boxplot(alpha = 0.7, outlier.color = "#e74c3c", outlier.size = 2) +
        coord_flip() +
        theme_minimal() +
        labs(x = "Payment Method", y = "Total Amount ($)",
             title = "Order Value by Payment Method") +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14)) +
        scale_fill_viridis_d(option = "D") +
        scale_y_continuous(labels = scales::dollar_format())
    } else {
      ggplot() +
        annotate("text", x = 1, y = 250, 
                 label = "No payment method or amount data available with current filters", 
                 size = 5, color = "gray") +
        theme_minimal() +
        labs(x = "Payment Method", y = "Total Amount ($)")
    }
  })
  

  # DATA TABLE & DOWNLOADS

  
  output$data_table <- renderDT({
    datatable(filtered_data(),
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'print')
              ),
              extensions = 'Buttons',
              class = 'display nowrap stripe hover',
              rownames = FALSE) %>%
      formatCurrency("TotalAmount", "$") %>%
      formatCurrency("ShippingCost", "$") %>%
      formatPercentage("Discount", 2)
  })
  
  # Download handlers
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("amazon_sales_cleaned_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("amazon_sales_cleaned_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(filtered_data(), file)
    }
  )
  
  # Show data stats
  observeEvent(input$show_stats, {
    dat <- filtered_data()
    showModal(modalDialog(
      title = "Data Statistics",
      div(
        h4("Summary Statistics:"),
        verbatimTextOutput("data_stats"),
        br(),
        h4("Structure:"),
        verbatimTextOutput("data_str")
      ),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  output$data_stats <- renderPrint({
    dat <- filtered_data()
    numeric_cols <- names(dat)[sapply(dat, is.numeric)]
    if (length(numeric_cols) > 0) {
      summary(dat[, numeric_cols])
    } else {
      cat("No numeric columns available\n")
    }
  })
  
  output$data_str <- renderPrint({
    dat <- filtered_data()
    str(dat)
  })
}

# 5. RUN APPLICATION

shinyApp(ui, server)