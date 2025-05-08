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
    "BRANCH_NAME1", "COVER_TYPE", "LOCATION", "MAIN_CLASS", "SUB_CLASSNAME"
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