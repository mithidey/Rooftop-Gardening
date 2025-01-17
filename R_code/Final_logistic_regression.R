# Load  libraries
library(dplyr)

# Load the data
nutri <- read.csv("C:/Users/MITHI/OneDrive/Pictures/Desktop/Nutritional_gardening/Data/final_nutri_gardening.csv")

# View the unique values in the columns

unique(nutri$feel_nutritional_gardening)
unique(nutri$occupation)
unique(nutri$education)
unique(nutri$age_group)
unique(nutri$gender)


# Clean up any leading/trailing whitespace and convert to lowercase
nutri$feel_nutritional_gardening <- trimws(tolower(nutri$feel_nutritional_gardening))
nutri$education <- trimws(tolower(nutri$education))
nutri$occupation <- trimws(tolower(nutri$occupation))
nutri$age_group <- trimws(tolower(nutri$age_group))


# Converting 'feel_nutritional_gardening' to binary format

nutri <- nutri %>%
  mutate(feel_nutritional_gardening = case_when(
    feel_nutritional_gardening %in% c("strongly yes", "somewhat yes") ~ "yes",
    feel_nutritional_gardening %in% c("no", "not at all") ~ "no",
    TRUE ~ "unknown"  # Catch any unexpected values
  ))



unique(nutri$age_group)
unique(nutri$occupation)
unique(nutri$education)
unique(nutri$feel_nutritional_gardening)

# Convert 'feel_nutritional_binary' and other categorical variables into factors

nutri$feel_nutritional_gardening <- as.factor(nutri$feel_nutritional_gardening)
nutri$age_group <- as.factor(nutri$age_group)
nutri$gender <- as.factor(nutri$gender)
nutri$occupation <- as.factor(nutri$occupation)
nutri$education <- as.factor(nutri$education)


# Fit the logistic regression model
model <- glm(feel_nutritional_gardening ~ age_group + gender + occupation + education, 
             data = data, family = binomial)

# summary 
summary(model)


