Premium_Data <- read_excel("/data/Premium_Data.xlsx")

dashboard_data <- Premium_Data %>%
  select(                      # Line of Business (e.g. Fire, Motor, etc.)
    BUISNESS_DESC,             # Channel type (Direct w/o Co-ins, Broker)
    CUSTOMER_CATEGORY,         # Broker / Agency / Direct classification
    CORPORATE_OR_RETAIL,       # Segment analysis (Retail vs Corporate)
    POLICY_FROM_DATE,          # Policy start date
    POLICY_TO_DATE,            # Policy end date
    UW_YEAR,                   # Underwriting year
    BASE_PREMIUM,              # Gross premium
    RETN,                      # Retained premium (Net)
    TOTAL_REINS_PREMIUM,       # Total ceded premium
    BROKER_COMM,               # Broker commission
    QS_COMM,                   # Quota Share commission
    SURPLUS_01_COMM,           # Surplus commission
    FAC_COMM,                  # Facultative commission
    SUM_INSURED,               # For premium adequacy metrics
    BUSINESS_TYPE,             # Business type (e.g. Motor, Fire)  
    BRANCH_NAME1,              # Branch-level grouping
    CORPORATE_OR_RETAIL,       # Segment analysis (Retail vs Corporate)
    COVER_TYPE,                # Cover type (e.g. Comprehensive, Third Party)  
    LOCATION,                  # Location of the risk 
    MAIN_CLASS,                # Main class of business (e.g. Motor, Fire)
    BRANCH_NAME1,              # Branch-level grouping
    BRANCH_NAME2,              # Branch-level grouping
  )
 

  import_and_save_premium_data <- function(input_path, output_path) {
  library(readxl)
  library(dplyr)
  library(writexl)
  
  # Columns to select
  selected_columns <- c(
    "BUISNESS_DESC", "CUSTOMER_CATEGORY", "CORPORATE_OR_RETAIL",
    "POLICY_FROM_DATE", "POLICY_TO_DATE", "UW_YEAR", "BASE_PREMIUM",
    "RETN", "TOTAL_REINS_PREMIUM", "BROKER_COMM", "QS_COMM",
    "SURPLUS_01_COMM", "FAC_COMM", "SUM_INSURED", "BUSINESS_TYPE", "BRANCH_NAME",
    "BRANCH_NAME1", "COVER_TYPE", "LOCATION", "MAIN_CLASS"
  )
  
  # Read the Excel file
  premium_data <- read_excel(input_path)
  
  # Select the specified columns
  dashboard_data <- premium_data %>%
    select(all_of(selected_columns))
  
  # Write the selected data to a new Excel file
  write_xlsx(dashboard_data, output_path)
  
  message("Data has been successfully imported and saved to: ", output_path)
}


import_and_save_premium_data(
  input_path = "C:/Users/Robin Ochieng/OneDrive - Kenbright/Attachments/projects/2025/May/Jubilee Claims Dashboard/data/Premium_Data.xlsx",
  output_path = "C:/Users/Robin Ochieng/OneDrive - Kenbright/Attachments/projects/2025/May/Jubilee Claims Dashboard/data/Selected_Premium_Data.xlsx"
)


premium_data <- read_excel(input_path)
names(premium_data) <- make.names(names(premium_data), unique = TRUE)



premium_data <- read_excel(input_path, col_types = c("text", "numeric", "date", ...))
