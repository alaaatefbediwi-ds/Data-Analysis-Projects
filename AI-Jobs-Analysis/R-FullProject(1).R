install.packages("tidyverse")
install.packages("xlsx")
library("tidyverse")
library(readr)
library(readxl)
library(xlsx)
library(ggplot2)
install.packages("openxlsx")
library(openxlsx)
library(dplyr)
library(tidyverse)
library(dplyr)
install.packages("corrplot")
install.packages("ggcorrplot")
library(corrplot)
library(ggcorrplot)
library(tidyr)

#Reading data
data <- read.csv("E:\\DEPI Data Analyst Specialist\\Ai-Ml-Salaries\\Ai-salaries.csv")
data

# Displaying first 5 rows 
head(data)

# Data Cleaning

#checking for missing values
colSums(is.na(data))  # the data has no missing values

#Exploring the shape of dataset
dataset_shape <- dim(data)
dataset_shape

print(paste("Number of rows: ", dataset_shape[1]))
print(paste("Number of columns: ", dataset_shape[2]))

#Checking for duplicates
sum(duplicated(data))

#Removing Duplicates
df1 <- data[!duplicated(data), ]
df1

dim(df1)

#Resetting the index after removing duplicates
rownames(df1) <- NULL

df1

#Checking for data types
str(df1)

#Data Exploration
summary(df1)

#Correlation Analysis
corr_mat <- cor(select_if(df1, is.numeric))
corr_mat

#Visualaizing Correlation Matrix
#corrplot(corr_mat, method = "color", tl.col = "black", tl.cex = 0.8)

ggcorrplot(corr_mat, lab = TRUE, lab_size = 3, colors = c("red", "white", "blue")) +
  ggtitle("Correlation Matrix") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))


#Counts of Categorical Values
categorical_columns <- select(df1, where(is.character))

sorted_counts <- sapply(categorical_columns, function(col) {
  counts <- table(col)
  sorted_counts <- sort(counts, decreasing = TRUE)
  return(sorted_counts)
})
sorted_counts

#Box plots to detect outliers
data_box <- df1 %>%
  pivot_longer(cols = c(salary, salary_in_usd), names_to = "variable", values_to = "value")

# Create box plots with adjusted y-axis limits
ggplot(data_box, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(y = "Values", x = "Variables") +
  scale_y_continuous(limits = c(0, 10000000)) +  # Adjust these limits as needed
  theme_minimal()

# Apply logarithmic transformation
ggplot(data_box, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(y = "Values", x = "Variables") +
  scale_y_log10() + 
  theme_minimal()

summary(df1)

#Normalization
min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply Min-Max Normalization to 'salary' and 'salary_in_usd'
df1$salary_normalized <- min_max_normalize(df1$salary)
df1$salary_in_usd_normalized <- min_max_normalize(df1$salary_in_usd)


#Box plots to detect outliers
data_box <- df1 %>%
  pivot_longer(cols = c(salary_normalized, salary_in_usd_normalized), names_to = "variable", values_to = "value")

# Create box plots with adjusted y-axis limits
ggplot(data_box, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(y = "Values", x = "Variables") +
  scale_y_continuous(limits = c(0, 1)) +  # Adjust these limits as needed
  theme_minimal()

#Applying some adjustments
df1 <- df1 %>%
  mutate(experience_level = case_when(
    experience_level == 'MI' ~ 'Mid-Level',
    experience_level == 'SE' ~ 'Senior',
    experience_level == 'EX' ~ 'Executive',
    experience_level == 'EN' ~ 'Entry-Level',
    TRUE ~ experience_level  # Keep original value if no match
  ))
df1

df1 <- df1 %>%
  mutate(employment_type = case_when(
    employment_type == 'FT' ~ 'Full-Time',
    employment_type == 'PT' ~ 'Part-Time',
    employment_type == 'CT' ~ 'Contract',
    employment_type == 'FL' ~ 'Freelance',
    TRUE ~ employment_type  # Keep original value if no match
  ))
df1

df1 <- df1 %>%
  mutate(company_size = case_when(
    company_size == 'M' ~ 'Medium',
    company_size == 'S' ~ 'Small',
    company_size == 'L' ~ 'Large',
    TRUE ~ company_size  # Keep original value if no match
  ))
df1


#Data Visualization

ggplot(df1, aes(x = salary)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  ggtitle("Salary Distribution") +
  xlab("Salary") +
  ylab("Frequency")

#First Chart
# Group by job title and calculate average salary
average_salaries <- df1 %>%
  group_by(job_title) %>%
  summarize(average_salary = mean(salary, na.rm = TRUE)) %>%
  arrange(desc(average_salary)) %>%
  head(20)  # Select the top 10 job titles with the highest average salaries

# Create a bar plot of the top 10 job titles with the highest average salaries
ggplot(average_salaries, aes(x = reorder(job_title, average_salary), y = average_salary, fill = job_title)) +
  geom_bar(stat = "identity") +  # Use identity to plot the actual values
  coord_flip() +  # Flip coordinates to make labels easier to read
  labs(title = "Top 20 Job Titles by Average Salary",
       x = "Job Title",
       y = "Average Salary") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend if not needed

#Second Chart
# Summarize the maximum salary for each job title
job_title_salaries <- df1 %>%
  group_by(job_title, experience_level) %>%
  summarize(max_salary = max(salary_normalized, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(max_salary)) %>%
  top_n(10, wt = max_salary)  # Select the top 10 job titles with the largest salaries

# Create a grouped bar plot for the top 10 job titles
ggplot(job_title_salaries, aes(x = reorder(job_title, max_salary), y = max_salary, fill = experience_level)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use 'dodge' to group bars side by side
  labs(title = "Top 10 Salaries by Job Title and Experience Level",
       x = "Job Title",
       y = "Salary") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        legend.title = element_blank()) 

# Third Chart
aggregated_salaries <- df1 %>%
  group_by(salary_currency) %>%
  summarize(total_salary = sum(salary_in_usd_normalized, na.rm = TRUE),  # Use sum or mean depending on your needs
            average_salary = mean(salary_in_usd_normalized, na.rm = TRUE)) %>%
  arrange(desc(total_salary))  # Order by total salary if needed

# Create a bar plot for total salary by currency
ggplot(aggregated_salaries, aes(x = reorder(salary_currency, total_salary), y = total_salary, fill = salary_currency)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Salary by Currency",
       x = "Salary Currency",
       y = "Total Salary (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#Fourth Chart
# Group by job title and calculate average salary
average_salaries <- df1 %>%
  group_by(company_size) %>%
  summarize(average_salary = mean(salary_normalized, na.rm = TRUE)) %>%
  arrange(desc(average_salary)) %>%
  head(20)  # Select the top 10 job titles with the highest average salaries

# Create a bar plot of the top 10 job titles with the highest average salaries
ggplot(average_salaries, aes(x = reorder(company_size, average_salary), y = average_salary)) +
  scale_fill_gradient(low = "blue", high = "red") + 
  geom_bar(stat = "identity") +  # Use identity to plot the actual values
  coord_flip() +  # Flip coordinates to make labels easier to read
  labs(title = "Company Sizes by Average Salary",
       x = "Company Size",
       y = "Average Salary") +
  theme_minimal() +
  theme(legend.position = "none")  

# Fifth Chart
# Count the occurrences of each job title
job_title_counts <- df1 %>%
  group_by(job_title) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)  # Select the top 10 job titles with the highest counts

# Create a bar plot of the top 10 job titles
ggplot(job_title_counts, aes(x = reorder(job_title, count), y = count, fill = job_title)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Job Titles by Count",
       x = "Job Title",
       y = "Count") +
  coord_flip() +  # Flip coordinates to make labels easier to read
  scale_fill_brewer(palette = "Set3") +  # Apply a color palette
  theme_minimal() +
  theme(legend.position = "none",  # Remove the legend if not needed
        axis.text.x = element_text(angle = 45, hjust = 1))  

#Sixth Chart
# Count occurrences of each job title by year
job_title_counts_by_year <- df1 %>%
  group_by(work_year, job_title) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(work_year, desc(count))

# Get top 5 job titles by count for each year
top_5_job_titles_by_year <- job_title_counts_by_year %>%
  group_by(work_year) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  arrange(work_year, desc(count))  # Ensure correct ordering

# Create a bar plot of the top 5 job titles by year
ggplot(top_5_job_titles_by_year, aes(x = reorder(job_title, count), y = count, fill = job_title)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ work_year) +  # Create separate plots for each year
  labs(title = "Top 5 Job Titles by Year",
       x = "Job Title",
       y = "Count") +
  coord_flip() +  # Flip coordinates to make labels easier to read
  scale_fill_brewer(palette = "Set3") +  # Apply a color palette
  theme_minimal() +
  theme(legend.position = "none",  # Remove the legend if not needed
        axis.text.x = element_text(angle = 45, hjust = 1))

#Seventh Chart
# Group by employment type and calculate the sum of salaries
sum_salaries_by_employment_type <- df1 %>%
  group_by(employment_type) %>%
  summarize(total_salary = mean(salary_normalized, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(total_salary))  # Order by sum of salaries in descending order

# Create a bar plot of the summed salaries by employment type
ggplot(sum_salaries_by_employment_type, aes(x = reorder(employment_type, total_salary), y = total_salary, fill = employment_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Salaries by Employment Type",
       x = "Employment Type",
       y = "Total Salary") +
  coord_flip() +  
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal() +
  theme(legend.position = "none",  
        axis.text.x = element_text(angle = 45, hjust = 1)) 

# Last Chart
# Count occurrences of each employee residence
residence_counts <- df1 %>%
  count(experience_level) %>%
  arrange(desc(n)) %>%
  mutate(percentage = n / sum(n) * 100)  # Calculate percentage

# Create a pie chart of employee residence counts with percentages
ggplot(residence_counts, aes(x = "", y = n, fill = experience_level)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +  
  labs(title = "Counts of Employee Residence",
       x = NULL,
       y = NULL) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 5) +  # Add percentage labels
  theme_minimal() +
  theme(axis.text.x = element_blank(),  
        axis.ticks = element_blank())  

#Saving File to Csv File
write.csv(df1, file = "Ai-Ml.csv", row.names = FALSE)
