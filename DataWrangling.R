# Assignment 2
library(dplyr)
library(tidyr)

data = read.csv("US_Tax_dateset.csv", stringsAsFactors = FALSE)

data$name <- trimws(data$name)
data$gender <- as.factor(trimws(data$gender))
levels(data$gender)
data$profession <- as.factor(trimws(data$profession))
levels(data$profession)
data$marital_status <- as.factor(trimws(data$marital_status))
levels(data$marital_status)
data$city <- as.factor(trimws(data$city))
levels(data$city)
data$record_id <- trimws(data$record_id)

library(modeest)
colSums(is.na(data))

numCols = c("age", "yearly_income_2019", "yearly_income_2020", "yearly_income_2021", 
            "yearly_income_2022", "yearly_income_2023", "yearly_income_2024", 
            "yearly_income_2025", "yearly_tax_2019", "yearly_tax_2020", 
            "yearly_tax_2021", "yearly_tax_2022", "yearly_tax_2023", 
            "yearly_tax_2024", "yearly_tax_2025", "investments", 
            "deductions", "annual_bonus", "business_income", 
            "inflation_rate", "future_tax_2026")
for(col in numCols) {
  data[[col]] <- as.numeric(data[[col]])
}
colSums(is.na(data))
# Numeric data doesnt have missing value though but still to be at safe end we will replace missing value with median of that col
for (col in numCols) {
  if (any(is.na(data[[col]]))) {
    med_val <- median(data[[col]], na.rm = TRUE)
    data[[col]][is.na(data[[col]])] <- med_val
  }
}

# categorical data
catCols = c("gender", "profession", "marital_status", "city")
for (col in catCols) {
  if (any(data[[col]] == "" | is.na(data[[col]]))) {
    data[[col]][data[[col]] == ""] <- NA
    mode_val <- modeest::mfv(data[[col]], na.rm = TRUE)[1]
    data[[col]][is.na(data[[col]])] <- mode_val
  }
}
# noise removal 
data = data %>% select(-record_id, -inflation_rate,-name)
numCols = c("age", "yearly_income_2019", "yearly_income_2020", "yearly_income_2021", 
            "yearly_income_2022", "yearly_income_2023", "yearly_income_2024", 
            "yearly_income_2025", "yearly_tax_2019", "yearly_tax_2020", 
            "yearly_tax_2021", "yearly_tax_2022", "yearly_tax_2023", 
            "yearly_tax_2024", "yearly_tax_2025", "investments", 
            "deductions", "annual_bonus", "business_income", "future_tax_2026")
for(col in numCols) {
  q_vals <- quantile(data[[col]], probs = c(0.25, 0.75), na.rm = TRUE)
  iqr_val <- q_vals[2] - q_vals[1]
  lower_fence <- q_vals[1] - 1.5 * iqr_val
  upper_fence <- q_vals[2] + 1.5 * iqr_val
  data[[col]] <- ifelse(data[[col]] < lower_fence, lower_fence, data[[col]])
  data[[col]] <- ifelse(data[[col]] > upper_fence, upper_fence, data[[col]])
}

# Categorical encoding
data$gender = as.numeric(factor(data$gender, levels = c("Female", "Male"))) - 1

data$city = as.numeric(factor(data$city, 
                              levels = c("Austin", "Boston", "Chicago", "Columbus", "Dallas", "Denver", 
                                         "Fort Worth", "Houston", "Indianapolis", "Jacksonville", "Los Angeles", "Nashville", 
                                         "New York", "Philadelphia", "Phoenix", "Portland", "San Antonio", "San Diego", 
                                         "San Jose", "Seattle")))

data$profession = as.numeric(factor(data$profession, 
                                    levels = c("Businessman", "Designer", "Doctor", "Engineer", "Freelancer", "Teacher")))

data$marital_status = as.numeric(factor(data$marital_status, 
                                        levels = c("Single", "Married", "Divorced", "Widowed")))

#Scaling
scale_cols = c("yearly_income_2019", "yearly_income_2020", "yearly_income_2021", 
                "yearly_income_2022", "yearly_income_2023", "yearly_income_2024", 
                "yearly_income_2025", "yearly_tax_2019", "yearly_tax_2020", 
                "yearly_tax_2021", "yearly_tax_2022", "yearly_tax_2023", 
                "yearly_tax_2024", "yearly_tax_2025", "investments", 
                "deductions", "annual_bonus", "business_income", "future_tax_2026")

for(col in scale_cols) {
  data[[col]] = log1p(data[[col]])
}

min_age = min(data$age, na.rm = TRUE)
max_age = max(data$age, na.rm = TRUE)
data$age = (data$age - min_age) / (max_age - min_age)

write.csv(data, "US_Tax_dataset_updated.csv", row.names = FALSE)
