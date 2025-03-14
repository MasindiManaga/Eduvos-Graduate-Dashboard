#Question 1: Cleaning and pre-processing
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)

# Read a csv file
df <- read_csv("C:/Users/user/Downloads/graduate_survey.csv")

# Selecting relevant columns
selected_data <- select(df, Campus, StudyField, Branch, Role, EduLevel, ProgLang, Databases, Platform, WebFramework, Industry, AISearch,AITool, Employment)
View(selected_data)

# Count of missing values per row
rowSums(is.na(selected_data))

# Count of missing values per column
colSums(is.na(selected_data))

# Count total missing values in the data set
total_missing <- sum(is.na(selected_data))
cat("Total missing values in the dataset:", total_missing, "\n")

# Handle missing values by removing rows with missing data
selected_data_cleaned <- na.omit(selected_data)

# View the cleaned data
View(selected_data_cleaned)

# Standardizing categorical columns 
selected_data_cleaned$Campus <- str_replace(selected_data_cleaned$Campus, "Umhlanga", "Durban")

# View the data
View(selected_data_cleaned)

# Ensuring correct standardization
unique(selected_data_cleaned$Campus)

# Select the top 3-5 campuses based on response count
campus_counts <- selected_data_cleaned %>% count(Campus) %>% arrange(desc(n))

# View campus counts
print(campus_counts)

# Select top 3-5 campuses based on response count
top_campuses <- campus_counts$Campus[1:5]

# Subset the data to keep the specific campuses
subset_data <- selected_data_cleaned %>% filter(Campus %in% top_campuses)

# View the data
View(subset_data)

# Count unique campuses
length(unique(subset_data$Campus))
sort(unique(subset_data$Campus), decreasing = FALSE)


#Question 2, Visualization:

#I: Top Tech Tools Used by Graduates
library(ggplot2)
library(tidyr)

count_tools <- function(subset_data, col_name) {
  subset_data %>%
    select(all_of(col_name)) %>%
    mutate(temp = str_split(!!sym(col_name), ";")) %>%
             unnest(temp) %>%
             mutate(temp = str_trim(temp)) %>%
             count(temp, sort = TRUE) %>%
             rename(tool = temp, count = n)
             
}

#list of tool columns to analyze
tools <- c("ProgLang","Databases","AISearch","AITool","WebFramework","Platform")

#function
tool_list <- lapply(tools,function(col){
  result <- count_tools(subset_data,col)
  result$category <- col
  return(result)
  
})

tool_counts_df <- bind_rows(tool_list)

print(tool_counts_df)

#bar chart 
ggplot(tool_counts_df, aes(x = reorder(tool,count), y = count, fill = category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~category, scales = "free") +
  labs(title = "Top Tech Tools Used by Graduates", x= "Tool", y= "Count") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8, hjust = 1), 
    axis.text.x = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom" 
  )

#II: Popular Industries by Study Field
industry_counts <- subset_data %>%
  select(StudyField, Industry) %>%
  mutate(Industry = str_split(Industry, ";")) %>%
  unnest(Industry) %>%
  mutate(Industry = str_trim(Industry)) %>%
  count(StudyField, Industry, sort = TRUE)

ggplot(industry_counts, aes(x = reorder(Industry, n), y = n, fill = StudyField)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ StudyField, scales = "free_y") +
  coord_flip() +
  labs(title = "Popular Industries by Study Field", 
       x = "Industry", 
       y = "Count") +
  theme_minimal()

#III: Job roles by Study Field
role_counts <- subset_data %>%
  count(StudyField, Role, sort = TRUE)
ggplot(role_counts, aes(x = reorder(Role, n), y = n, fill = StudyField)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ StudyField, scales = "free_y") +
  coord_flip() +
  labs(title = "Top Job Roles by Study Field", 
       x = "Job Role", 
       y = "Count") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )

# IV: Employment Rate by Study Field (Pie Charts using ggplot with Percentages)
# Split and categorize employment status by study field
data <- subset_data %>%
  separate_rows(Employment, sep = ";") %>%
  group_by(StudyField, Employment) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100) %>% # Calculate percentage within each StudyField
  arrange(StudyField, desc(count))

# Plot Pie Charts using ggplot with Percentages
study_fields <- unique(data$StudyField)

for (field in study_fields) {
  field_data <- data %>% filter(StudyField == field)
  
  ggplot(field_data, aes(x = "", y = percentage, fill = Employment)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = paste("Employment Rate -", field), fill = "Employment Status") +
    theme_void() +
  theme(legend.position = "right") + # Move legend to the right
    geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) # Add percentages
  print(last_plot()) # Print the plot
}


#saves script as csv
#write_csv(subset_data, "subset_data.csv")