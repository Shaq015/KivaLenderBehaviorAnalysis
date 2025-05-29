# Load necessary libraries
library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(caret)
library(randomForest)
library(forcats)
library(ggplot2)
library(pdp)
library(corrplot)
library(e1071)


# Creating new features based on the original data for TreeTableLoaners table

bigml <- read.csv("bigml.csv")
lenders <- read.csv("lenders.csv")
loans_lenders <- read.csv("loans_lenders.csv")
loans <- read.csv("loans.csv")

# Select the required columns for the new CSV file
# Replace "PERMANENT_NAME", "LOAN_BECAUSE", and "Cleaned" with actual column names
tree_table_loaners <- lenders %>%
  select(PERMANENT_NAME, LOAN_BECAUSE, Cleaned)

# Remove rows with NA values in the "Cleaned" column
tree_table_loaners <- tree_table_loaners %>%
  filter(!is.na(Cleaned))

merged_loansLenders_loansClean <- merge(loans_lenders, loans, by = "LOAN_ID")


# Categorize LOAN_BECAUSE column

# Key terms for each category
religion_terms <- c("faith", "worship", "prayer", "church", "god", "holy", "sacred", "bible", "temple", "mosque", "spirit", "divine", "belief", "ritual", "blessing", "pilgrimage", "clergy", "sin", "redemption", "scripture")
loving_kiva_terms <- c("kiva", "platform", "love", "loving")
global_community_terms <- c("unity", "diversity", "collaboration", "cooperation", "integration", "solidarity", "connection", "peace", "tolerance", "inclusion", "humanity", "network", "communication", "sustainability", "equality", "respect", "mutual aid", "globalization", "interdependence", "global", "community", "shared values", "understanding", "cultural exchange", "empathy", "international", "growth", "togetherness", "collective", "worldwide", "bridging gaps", "universal", "synergy", "cross-cultural", "alliances")
business_terms <- c("profit", "revenue", "marketing", "sales", "strategy", "innovation", "investment", "management", "leadership", "entrepreneurship", "customer", "growth", "market", "competition", "brand", "networking", "finance", "operations", "logistics", "product", "service", "business", "commerce", "corporate", "enterprise", "startup", "merger", "acquisition", "partnership", "negotiation", "resources", "supply chain", "efficiency", "scalability", "sustainability", "technology", "development", "economy", "website")

# Function to categorize based on key terms
categorize_loan_reason <- function(text) {
  text <- tolower(text)
  if (any(sapply(religion_terms, function(term) grepl(term, text)))) {
    return('Religion')
  } else if (any(sapply(loving_kiva_terms, function(term) grepl(term, text)))) {
    return('Loving Kiva')
  } else if (any(sapply(global_community_terms, function(term) grepl(term, text)))) {
    return('Global Community')
  } else if (any(sapply(business_terms, function(term) grepl(term, text)))) {
    return('Business')
  } else {
    return('Uncategorized')
  }
}

# Apply the function to create the new column
tree_table_loaners$Lender_Cause_to_Lend <- sapply(tree_table_loaners$LOAN_BECAUSE, categorize_loan_reason)

# Separate categorized and uncategorized rows
categorized_data <- tree_table_loaners %>% filter(Lender_Cause_to_Lend != 'Uncategorized')
uncategorized_data <- tree_table_loaners %>% filter(Lender_Cause_to_Lend == 'Uncategorized')

# Evenly distribute uncategorized rows among the categories
categories <- c('Religion', 'Loving Kiva', 'Global Community', 'Business')
uncategorized_data$Lender_Cause_to_Lend <- rep(categories, length.out = nrow(uncategorized_data))

# Concatenate the categorized and evenly distributed uncategorized data
tree_table_loaners <- bind_rows(categorized_data, uncategorized_data)

tree_table_loaners <- tree_table_loaners %>%
  select(-LOAN_BECAUSE)


# Split the lenders and count the loans to male and female borrowers
# Split the BORROWER_GENDERS into separate rows and trim whitespace
merged_loansLenders_loansClean <- merged_loansLenders_loansClean %>%
  separate_rows(BORROWER_GENDERS, sep = ",") %>%
  mutate(BORROWER_GENDERS = str_trim(BORROWER_GENDERS))


# Split the LENDERS into separate rows
lender_gender_counts <- merged_loansLenders_loansClean %>%
  separate_rows(LENDERS, sep = ", ") %>%
  group_by(LENDERS, BORROWER_GENDERS) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = BORROWER_GENDERS, values_from = count, values_fill = list(count = 0))

# Calculate lending tendencies
lender_gender_counts <- lender_gender_counts %>%
  mutate(Tendency = case_when(
    male > female ~ "more to men",
    female > male ~ "more to women",
    TRUE ~ "equal"
  ))

# Merge this information with the TreeTableLoaners table
tree_table_loaners <- merge(tree_table_loaners, lender_gender_counts[, c("LENDERS", "Tendency")],
                            by.x = "PERMANENT_NAME", by.y = "LENDERS", all.x = TRUE)

# Calculate the average age of borrowers for each lender
lender_age_avg <- merged_loansLenders_loansClean %>%
  separate_rows(LENDERS, sep = ", ") %>%
  group_by(LENDERS) %>%
  summarise(avg_age = round(mean(age, na.rm = TRUE)), .groups = 'drop')

# Merge this information with the TreeTableLoaners table
tree_table_loaners <- merge(tree_table_loaners, lender_age_avg,
                            by.x = "PERMANENT_NAME", by.y = "LENDERS", all.x = TRUE)

# Calculate the average loan amount of borrowers for each lender
lender_amount_avg <- merged_loansLenders_loansClean %>%
  separate_rows(LENDERS, sep = ", ") %>%
  group_by(LENDERS) %>%
  summarise(avg_loan_amount = round(mean(LOAN_AMOUNT, na.rm = TRUE)), .groups = 'drop')

# Merge this information with the TreeTableLoaners table
tree_table_loaners <- merge(tree_table_loaners, lender_amount_avg,
                            by.x = "PERMANENT_NAME", by.y = "LENDERS", all.x = TRUE)

# Determine the most frequent sector for each lender
lender_sector <- merged_loansLenders_loansClean %>%
  separate_rows(LENDERS, sep = ", ") %>%
  group_by(LENDERS, SECTOR_NAME) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%
  group_by(LENDERS) %>%
  slice(1) %>%
  select(LENDERS, SECTOR_NAME)

# Merge most frequent sector information with TreeTableLoaners table
tree_table_loaners <- merge(tree_table_loaners, lender_sector,
                            by.x = "PERMANENT_NAME", by.y = "LENDERS", all.x = TRUE)

# Determine the country name of which the lender lends the most
lender_country <- merged_loansLenders_loansClean %>%
  separate_rows(LENDERS, sep = ", ") %>%
  group_by(LENDERS, COUNTRY_NAME) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%
  group_by(LENDERS) %>%
  slice(1) %>%
  select(LENDERS, COUNTRY_NAME)

# Merge the country name of which the lender lends the most information with TreeTableLoaners table
tree_table_loaners <- merge(tree_table_loaners, lender_country,
                            by.x = "PERMANENT_NAME", by.y = "LENDERS", all.x = TRUE)


# Add Classification column and remove NA values and columns

# Remove rows with NA values in any column
final_data <- na.omit(cleaned_data)

# Rename the columns
renamed_data <- final_data %>%
  rename(
    Lender_Username = PERMANENT_NAME,
    Lender_Occupation = Cleaned,
    Gender_Loan_Preference = Tendency,
    Average_Age_of_Borrower = avg_age.y,
    Average_Loan_Amount = avg_loan_amount,
    Loan_Sector = SECTOR_NAME,
    Borrower_Country = COUNTRY_NAME
  )

# Add Borrower_Continent, Loan_Amount_range and Average_Age_range columns

# Define the function to determine the continent based on the country
get_continent <- function(country) {
  if (country %in% c("United States", "Mexico", "Guatemala", "Honduras", "Nicaragua", "Costa Rica", "El Salvador", "Dominican Republic", "Haiti", "Puerto Rico")) {
    return("North and Central America")
  } else if (country %in% c("Peru", "Bolivia", "Paraguay", "Chile", "Colombia", "Ecuador", "Brazil")) {
    return("South America")
  } else if (country %in% c("Azerbaijan", "Georgia", "Armenia", "Albania", "Kosovo", "Moldova", "Ukraine")) {
    return("Europe")
  } else if (country %in% c("Somalia", "South Africa", "South Sudan", "Malawi","Zambia", "Madagascar","Congo", "Mali", "Zimbabwe", "Burundi", "Togo", "The Democratic Republic of the Congo", "Senegal", "Ghana", "Kenya", "Rwanda", "Liberia", "Mozambique", "Benin", "Sierra Leone", "Nigeria", "Lesotho", "Burkina Faso", "Cameroon", "Uganda", "Tanzania", "Egypt")) {
    return("Africa")
  } else if (country %in% c("Samoa", "Solomon Islands")) {
    return("Oceania")
  } else if (country %in% c("Kyrgyzstan", "Timor-Leste", "Tajikistan", "Philippines", "India", "Vietnam", "Pakistan", "Indonesia", "Lao People's Democratic Republic", "Myanmar (Burma)", "Cambodia", "China", "Malaysia", "Mongolia", "Thailand")) {
    return("East Asia")
  } else if (country %in% c("Palestine", "Lebanon", "Jordan", "Iraq", "Yemen")) {
    return("Middle East")
  } else {
    return(NA)
  }
}

# Add the new column 'Borrower_Continent' based on 'Borrower_Country'
renamed_data$Borrower_Continent <- sapply(renamed_data$Borrower_Country, get_continent)

# Define the function to determine the age range
get_age_range <- function(age) {
  if (is.na(age)) {
    return(NA)
  } else if (age >= 1 & age <= 10) {
    return("1 to 10")
  } else if (age >= 11 & age <= 20) {
    return("11 to 20")
  } else if (age >= 21 & age <= 30) {
    return("21 to 30")
  } else if (age >= 31 & age <= 40) {
    return("31 to 40")
  } else if (age >= 41 & age <= 50) {
    return("41 to 50")
  } else if (age >= 51 & age <= 60) {
    return("51 to 60")
  } else if (age >= 61 & age <= 70) {
    return("61 to 70")
  } else if (age >= 71) {
    return("71+")
  }
}

# Define the function to determine the loan amount range
get_loan_amount_range <- function(amount) {
  if (is.na(amount)) {
    return(NA)
  } else if (amount >= 1 & amount <= 500) {
    return("1-500")
  } else if (amount >= 501 & amount <= 1000) {
    return("501-1000")
  } else if (amount >= 1001 & amount <= 1500) {
    return("1001-1500")
  } else if (amount >= 1501 & amount <= 2000) {
    return("1501-2000")
  } else if (amount >= 2001 & amount <= 2500) {
    return("2001-2500")
  } else if (amount >= 2501 & amount <= 3000) {
    return("2501-3000")
  } else if (amount >= 3001 & amount <= 3500) {
    return("3001-3500")
  } else if (amount >= 3501 & amount <= 4000) {
    return("3501-4000")
  } else if (amount >= 4001 & amount <= 4500) {
    return("4001-4500")
  } else if (amount >= 4501 & amount <= 5000) {
    return("4501-5000")
  } else if (amount >= 5001 & amount <= 9999) {
    return("5001-9999")
  } else {
    return("10000+")
  }
}

# Add the new columns 'Average_Age_Range' and 'Average_Loan_Amount_Range'
renamed_data <- renamed_data %>%
  mutate(Average_Age_Range = sapply(Average_Age_of_Borrower, get_age_range),
         Average_Loan_Amount_Range = sapply(Average_Loan_Amount, get_loan_amount_range))

# Define a function to merge similar occupations into broader categories
merge_occupations <- function(occupation) {
  occupation <- tolower(occupation)
  if (grepl("student", occupation)) {
    return("Student")
  } else if (grepl("elementar|academic|youth|learn|english|kindergar|kid|teacher|professor|instructor|educator|education|college|university|school", occupation)) {
    return("Education")
  } else if (grepl("robot|machin|light|engineer|technician|electric|mechanic", occupation)) {
    return("Engineer/Technician")
  } else if (grepl("optic|paramed|acpun|pediatri|respirat|clinic|mental|childcare|patholog|dent|massage|psycho|therapist|veterinar|nurse|doctor|healthcare|medical", occupation)) {
    return("Healthcare")
  } else if (grepl("hardware|app|hacker|process|automat|qa|telecom|data|senior|junior|internet|network|software|it|program|developer|computer|web|analyst|product|project|system", occupation)) {
    return("High Tech")
  } else if (grepl("retired", occupation)) {
    return("Retired")
  } else if (grepl("reception|risk|inovat|plan|organiz|coo|assist|corporate|company|communication|event|secretary|association|consultant|counsellor|operation|owner|insurance|hr|admin|business|entrepreneur|entrep|enterp|manage|ceo|executive|founder|enterpreneur", occupation)) {
    return("Business/Management")
  } else if (grepl("dance|museum|game|broadcast|news|potter|wood|carpent|paint|act|books|librar|entertain|singer|sculpture|media|video|author|publish|film|director|graphic|artist|designer|writer|photographer|musician|journalist|actor|producer|creative|art", occupation)) {
    return("Arts/Creative")
  } else if (grepl("trad|tax|fundraiser|mortgage|bookkeeper|finance|accountant|bank|eco|invest", occupation)) {
    return("Finance")
  } else if (grepl("cashier|cloth|fashion|purchas|online|sales|marketing|advertising|retailer|customer|commercial|commerce", occupation)) {
    return("Sales/Marketing")
  } else if (grepl("coach|social|humanitarian|volunteer|nonprofit|philantrop", occupation)) {
    return("Social Work")
  } else if (grepl("judge|court|law|attorney|legal", occupation)) {
    return("Legal")
  } else if (grepl("diplomat|garden|vp|resident|minister|mayor|government|public|politic|civil|fire|drive|service|police", occupation)) {
    return("Government/Public Service")
  } else if (grepl("logis|hous|construction|architect|builder|manufact|industr", occupation)) {
    return("Construction")
  } else if (grepl("agriculture|farmer|environment", occupation)) {
    return("Agriculture")
  } else if (grepl("self|freelancer|independent|emplo", occupation)) {
    return("Self-Employed")
  } else if (grepl("estate|realtor", occupation)) {
    return("Real Estate")
  } else if (grepl("housewive|housewife|homemake|unemployed|disable", occupation)) {
    return("Unemployed")
  } else if (grepl("astro|anthrop|histor|geo|lab|statistic|chemist|research|science|scientist|physic|biolog|math", occupation)) {
    return("Research/Science") 
  } else if (grepl("episc|pastor|priest|rabbi|church", occupation)) {
    return("Religion") 
  } else if (grepl("army|soldier", occupation)) {
    return("Army") 
  } else if (grepl("pilot|air|flight", occupation)) {
    return("Aircraft") 
  } else if (grepl("barist|fish|chef|food|baker|restaurant|coffee|catering|bartender|cook|waiter|waitress", occupation)) {
    return("Food Industry") 
  } else if (grepl("world|travel|tour|hotel|blog", occupation)) {
      return("Travel") 
  } else if (grepl("hair|barber", occupation)) {
    return("Hairstyle") 
  } else if (grepl("train|sport", occupation)) {
    return("Sports") 
  } else {
    return("Other")
  }
}

renamed_data$Lender_Occupation_Category <- sapply(renamed_data$Lender_Occupation, merge_occupations)
renamed_data <- renamed_data %>%
  select(-Lender_Occupation)
write.csv(renamed_data, "TreeTableLoaners.csv", row.names = FALSE)


# Construction of One Hot Encoded table

# Define specified columns
columns_to_encode <- c(
  'Gender_Loan_Preference',
  'Loan_Sector',
  'Lender_Cause_to_Lend',
  'Borrower_Continent',
  'Average_Age_Range',
  'Average_Loan_Amount_Range',
  'Lender_Occupation_Category'
)

one_hot_table <- tree_table_loaners %>%
  mutate(across(all_of(columns_to_encode), as.factor)) %>%
  mutate(across(all_of(columns_to_encode), ~model.matrix(~. - 1, .)[, -1]))

write.csv(one_hot_table, "One-Hot Encode - TreeTableLoaners - Category.csv", row.names = FALSE)


# Construction of kMeans clusters for a Random Forest model 


one_hot_table <- read.csv("One-Hot Encode - TreeTableLoaners - Category.csv")

# Remove the username column and other non-predictive columns
data_features <- one_hot_table %>% select(-Lender_Username, -Lender_Cause_to_Lend)

# Elbow Method Plot
wss <- function(k) {
  kmeans(data_features, k, nstart = 10)$tot.withinss
}

k.values <- 1:15
wss_values <- map_dbl(k.values, wss)

elbow_plot <- ggplot(data.frame(k = k.values, wss = wss_values), aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  ggtitle("Elbow Method for Optimal Clusters") +
  xlab("Number of Clusters") +
  ylab("Within-Cluster Sum of Squares")

print(elbow_plot)


# Perform k-means clustering to create the Lender_Type variable
set.seed(123) # For reproducibility
num_clusters <- 12
kmeans_result <- kmeans(data_features, centers = num_clusters, nstart = 25)

# Add the Lender_Type variable to the data
one_hot_table$cluster <- as.factor(kmeans_result$cluster)
one_hot_table$cluster <- as.integer(as.character(one_hot_table$cluster))

write.csv(one_hot_table, "One-Hot Encode - TreeTableLoaners - Category - With Clusters.csv", row.names = FALSE)

# Calculate the mean of each feature for each cluster
cluster_data <- data_features %>%
  mutate(cluster = kmeans_result$cluster) %>%
  group_by(cluster) %>%
  summarise_all(mean)

# Define the function to calculate thresholds
calculate_threshold <- function(data, prefix) {
  relevant_columns <- data %>% select(starts_with(prefix))
  column_means <- colMeans(relevant_columns, na.rm = TRUE)
  column_sds <- apply(relevant_columns, 2, sd, na.rm = TRUE)
  threshold <- mean(column_means + 2 * column_sds)
  return(threshold)
}

# Define the function to generate Lender_Type string
generate_lender_type <- function(row, thresholds) {
  lender_type <- ""
  
  for (col_name in names(row)) {
    value <- as.numeric(row[[col_name]])
    if (startsWith(col_name, "Gender_Loan_Preference_") && value > thresholds$gender) {
      lender_type <- paste0(lender_type, "Gender Loan Preference: ", gsub("Gender_Loan_Preference_", "", col_name), "\n")
    } else if (startsWith(col_name, "Loan_Sector_") && value > thresholds$loan_sector) {
      lender_type <- paste0(lender_type, "Loan Sector: ", gsub("Loan_Sector_", "", col_name), "\n")
    } else if (startsWith(col_name, "Lender_Cause_to_Lend_") && value > thresholds$cause_to_lend) {
      lender_type <- paste0(lender_type, "Lender Cause to Lend: ", gsub("Lender_Cause_to_Lend_", "", col_name), "\n")
    } else if (startsWith(col_name, "Borrower_Continent_") && value > thresholds$continent) {
      lender_type <- paste0(lender_type, "Borrower Continent: ", gsub("Borrower_Continent_", "", col_name), "\n")
    } else if (startsWith(col_name, "Average_Age_Range_") && value > thresholds$age_range) {
      lender_type <- paste0(lender_type, "Average Age Range: ", gsub("Average_Age_Range_", "", col_name), "\n")
    } else if (startsWith(col_name, "Average_Loan_Amount_") && value > thresholds$loan_amount) {
      lender_type <- paste0(lender_type, "Average Loan Amount: ", gsub("Average_Loan_Amount_", "", col_name), "\n")
    } else if (startsWith(col_name, "Lender_Occupation_Category_") && value > thresholds$occupation) {
      lender_type <- paste0(lender_type, "Lender Occupation: ", gsub("Lender_Occupation_Category_", "", col_name), "\n")
    }
  }
  
  # Remove the trailing newline
  lender_type <- gsub("\n$", "", lender_type)
  
  return(lender_type)
}


# Calculate thresholds for each category
thresholds <- list(
  gender = calculate_threshold(cluster_data, "Gender_Loan_Preference_"),
  loan_sector = calculate_threshold(cluster_data, "Loan_Sector_"),
  cause_to_lend = calculate_threshold(cluster_data, "Lender_Cause_to_Lend_"),
  continent = calculate_threshold(cluster_data, "Borrower_Continent_"),
  age_range = calculate_threshold(cluster_data, "Average_Age_Range_"),
  loan_amount = calculate_threshold(cluster_data, "Average_Loan_Amount_"),
  occupation = calculate_threshold(cluster_data, "Lender_Occupation_Category_")
)

# Apply the function to each row of the cluster data
cluster_data$Lender_Type <- apply(cluster_data, 1, generate_lender_type, thresholds = thresholds)

# Save the updated cluster data to a new CSV file
write.csv(cluster_data, "K_Means_Groups_Analysis_With_Lender_Type.csv", row.names = FALSE)


# Separate the Lender_Type into multiple rows
data_separated <- cluster_data %>%
  separate_rows(Lender_Type, sep = "\n")

# Remove any trailing spaces and empty rows
data_separated$Lender_Type <- trimws(data_separated$Lender_Type)
data_separated <- data_separated %>%
  filter(Lender_Type != "")

# Extract Category and Value
data_separated <- data_separated %>%
  separate(Lender_Type, into = c("Category", "Value"), sep = ": ")

# Summary table of Lender_Type by cluster
summary_table <- data_separated %>%
  group_by(cluster, Category, Value) %>%
  summarise(count = n()) %>%
  arrange(cluster, desc(count))

# Get the unique clusters and split them into two groups
unique_clusters <- unique(summary_table$cluster)
split_index <- ceiling(length(unique_clusters) / 2)
clusters_group1 <- unique_clusters[1:split_index]
clusters_group2 <- unique_clusters[(split_index + 1):length(unique_clusters)]

# Filter the summary table for each group
summary_table_group1 <- summary_table %>% filter(cluster %in% clusters_group1)
summary_table_group2 <- summary_table %>% filter(cluster %in% clusters_group2)

# Visualization: Bar Plot for Group 1
plot_group1 <- ggplot(summary_table_group1, aes(x = reorder(Value, count), y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ cluster, scales = "free_y") +
  coord_flip() +
  ggtitle("Frequency of Descriptive Terms in Each Cluster (Group 1)") +
  xlab("Descriptive Term") +
  ylab("Count") +
  theme_minimal()

print(plot_group1)

# Visualization: Bar Plot for Group 2
plot_group2 <- ggplot(summary_table_group2, aes(x = reorder(Value, count), y = count, fill = Category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ cluster, scales = "free_y") +
  coord_flip() +
  ggtitle("Frequency of Descriptive Terms in Each Cluster (Group 2)") +
  xlab("Descriptive Term") +
  ylab("Count") +
  theme_minimal()

print(plot_group2)


# Join the Lender_Type column to "One-Hot Encode - TreeTableLoaners - Category - With Clusters" table
updated_data <- one_hot_table %>%
  left_join(cluster_data %>% select(cluster, Lender_Type), by = "cluster")
write.csv(updated_data, "One-Hot Encode - TreeTableLoaners - Category - With Clusters.csv", row.names = FALSE)

# Convert 'Lender_Type' to a factor
updated_data$cluster <- as.factor(updated_data$cluster)

# Remove the columns that should not be used in the model
data_model <- updated_data %>% select(-Lender_Username, -Lender_Cause_to_Lend, -Lender_Type)

# Split the data into training and test sets
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(data_model$cluster, p = .8, 
                                  list = FALSE, 
                                  times = 1)
trainData <- data_model[trainIndex,]
testData <- data_model[-trainIndex,]

# Train the random forest model
rf_model <- randomForest(cluster ~ ., 
                         data = trainData, 
                         ntree = 500, 
                         mtry = 2, 
                         importance = TRUE)

# Make predictions on the test set
predictions <- predict(rf_model, testData)

# Evaluate the model
conf_matrix <- confusionMatrix(predictions, testData$cluster)

# Print the confusion matrix
print(conf_matrix)

# Displaying accuracy
print(paste("Accuracy: ", conf_matrix$overall['Accuracy']))


# Visualize top 10 important sub-categories

# Get the importance of the features
importance_values <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_values),
                            Importance = importance_values[, "MeanDecreaseGini"])

# Get the top 10 important features
top_features <- importance_df[order(importance_df$Importance, decreasing = TRUE), ][1:10, ]

# Plot the top 10 important features
ggplot(top_features, aes(x = reorder(Feature, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  scale_fill_gradientn(colors = c("#ffce00", "#ff5a00", "#ff4D00")) +
  labs(title = "Top 10 Important Features",
       x = "Features",
       y = "Importance") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))


# Visualize importance of features

# Get the importance of the features
importance_values <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_values),
                            Importance = importance_values[, "MeanDecreaseGini"])

# Define the feature categories
categories <- c("Gender_Loan_Preference", "Average_Age_Range",
                "Average_Loan_Amount_Range", "Loan_Sector", "Borrower_Continent",
                "Lender_Cause_to_Lend", "Lender_Occupation")

# Extract the category from the feature names
importance_df$Category <- sapply(importance_df$Feature, function(x) {
  matched <- categories[startsWith(x, categories)]
  if (length(matched) > 0) {
    return(matched)
  } else {
    return("Other")
  }
})

# Sum the importance scores for each category
category_importance <- importance_df %>%
  group_by(Category) %>%
  summarise(TotalImportance = sum(Importance)) %>%
  arrange(desc(TotalImportance))

# Get the top 10 important categories
top_categories <- category_importance[1:7, ]

# Plot the top 10 important categories
ggplot(top_categories, aes(x = reorder(Category, TotalImportance), y = TotalImportance, fill = TotalImportance)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  scale_fill_gradientn(colors = c("#ffce00","#ff5a00","#ff4D00")) +
  labs(title = "Importance of Categories",
       x = "Categories",
       y = "Total Importance") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))



