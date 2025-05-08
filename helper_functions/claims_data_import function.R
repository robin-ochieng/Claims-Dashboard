import_and_save_claims_data <- function(input_path, output_path) {
  library(readxl)
  library(dplyr)
  library(writexl)
  
  # Columns to select
  selected_columns <- c(
    "BRANCH_NAME", "ESTIMATE_TYPE",
    "LOSS_DATE", "SUBCLASS_NAME", "CORPORATE_OR_RETAIL",
    "BRANCH_NAME1", "PAID_OS", "RETENTION", "QS", "FIRST_SP", "SECOND_SP",
    "XOL_AMOUNT", "FAC_OUT", "RECOVARY", "APPRV_DATE1"
  )
  
  # Read the Excel file
  claims_data <- read_excel(input_path)
  
  # Select the specified columns
  dashboard_data <- claims_data %>%
    select(all_of(selected_columns))
  
  # Write the selected data to a new Excel file
  write_xlsx(dashboard_data, output_path)
  
  message("Data has been successfully imported and saved to: ", output_path)
}


import_and_save_claims_data(
  input_path = "C:/Users/Robin Ochieng/OneDrive - Kenbright/Attachments/projects/2025/May/Jubilee Claims Dashboard/data/Claims_Data.xlsx",
  output_path = "C:/Users/Robin Ochieng/OneDrive - Kenbright/Attachments/projects/2025/May/Jubilee Claims Dashboard/data/Selected_Claims_Data.xlsx"
)