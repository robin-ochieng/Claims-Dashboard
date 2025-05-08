import_and_save_premium_data <- function(input_path, output_path) {
  library(readxl)
  library(dplyr)
  library(stringr)
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
  df <- premium_data %>%
    select(all_of(selected_columns))
  # Remove duplicates
  df <- df %>% distinct()
  # Convert date columns to Date format
  df <- df %>%
    mutate(
      POLICY_FROM_DATE = as.Date(POLICY_FROM_DATE, format = "%Y-%m-%d"),
      POLICY_TO_DATE = as.Date(POLICY_TO_DATE, format = "%Y-%m-%d")
    )
  # Convert numeric columns
  numeric_cols <- c("BASE_PREMIUM", "RETN", "TOTAL_REINS_PREMIUM", "BROKER_COMM",
                    "QS_COMM", "SURPLUS_01_COMM", "FAC_COMM", "SUM_INSURED", "UW_YEAR")
  df <- df %>%
    mutate(across(all_of(numeric_cols), ~ suppressWarnings(as.numeric(.))))
  # Convert UW_YEAR to integer
  df$UW_YEAR <- as.integer(df$UW_YEAR)
  # Object columns cleaning
  object_cols <- c("BUISNESS_DESC", "CUSTOMER_CATEGORY", "CORPORATE_OR_RETAIL",
                   "BUSINESS_TYPE", "BRANCH_NAME1", "COVER_TYPE", "LOCATION",
                   "MAIN_CLASS", "BRANCH_NAME", "SUB_CLASSNAME")
  
  for (col in object_cols) {
    df[[col]] <- as.character(df[[col]])
    if (col == "CUSTOMER_CATEGORY") {
      # Split and clean
      df[[col]] <- str_split_fixed(df[[col]], "-", 2)[,2]
      df[[col]] <- str_replace_all(df[[col]], "-", " ")
    }
    if (col == "BRANCH_NAME1") {
      df[[col]] <- str_split_fixed(df[[col]], " ", 2)[,1]
    }
    # Title case and trim spaces
    df[[col]] <- str_to_title(str_trim(df[[col]]))
  }
  # Write the selected data to a new Excel file
  write_xlsx(df, output_path)
  message("Data has been successfully imported and saved to: ", output_path)
}


import_and_save_premium_data(
  input_path = "C:/Users/Work/OneDrive - Kenbright/Attachments/Premium_Data.xlsx",
  output_path = "C:/Users/Work/OneDrive - Kenbright/Attachments/Selected_Columns_Data.xlsx"
)