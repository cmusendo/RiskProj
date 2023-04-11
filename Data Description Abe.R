# Load ggplot2 library
library(ggplot2)
library(readxl)

# Read in the Excel file and remove extra spaces from column names
df <- read_excel("C:/Users/apexb/Downloads/Taxpayer Returns Statistics (Copy).Anonymised.xlsx")


# To get a summary of the dataset 
summary(df)

# To Check the Structure of the dataset
str(df)

# To calculate some basic Statics 
mean(df$Amount)
sd(df$Amount)

# To create a histogram of the Amount Variable

hist(df$Amount)

library(ggplot2)

# Convert Pstng Date to a date format
df$`Pstng Date` <- as.Date(df$`Pstng Date`, format = "%d.%m.%Y")

# Subset the data frame to include only the first 5 records
df_subset <- head(df, 500)

# Create scatterplot with y-axis limits
ggplot(df_subset, aes(x=`Pstng Date`, y=Amount)) +
  geom_point() +
  scale_x_date(date_breaks = "100 days", date_labels = "%d-%m-%Y") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  ylim(min(df_subset$Amount), max(df_subset$Amount))


df$`Doc. Date` <- as.Date(df$`Doc. Date`, format = "%d.%m.%Y")
df$`Due Date` <- as.Date(df$`Due Date`, format = "%d.%m.%Y")

# Create new variable indicating whether payment was made before or after due date
df$compliance <- ifelse(df$`Doc. Date` <= df$`Due Date`, "Compliant", "Non-Compliant")

# Create bar chart of compliance
ggplot(df, aes(x=compliance)) +
  geom_bar() +
  labs(title="Compliance of Taxpayers", x="Compliance Status", y="Number of Payments")

library(reshape2)

# Convert Doc Date and Due Date to date format
df$`Doc. Date` <- as.Date(df$`Doc. Date`, format = "%d.%m.%Y")
df$`Due Date` <- as.Date(df$`Due Date`, format = "%d.%m.%Y")

# Subset the data frame to include only the records with Period Key 1701
df_subset <- subset(df, df$`Period key...6` == 1701)

# Create a new variable to indicate compliance status
df_subset$compliance <- ifelse(df_subset$`Doc. Date` <= df_subset$`Due Date`, "Compliant", "Non-Compliant")

# Subset the data frame to include only the records for January
df_subset_jan <- subset(df_subset, format(`Doc. Date`, "%m") == "01")

# Create scatterplot with color based on compliance status
ggplot(df_subset_jan, aes(x=`Doc. Date`, y=`Due Date`, color=compliance)) +
  geom_point() +
  scale_x_date(date_breaks = "1 day", date_labels = "%d-%m-%Y") +
  scale_y_date(date_breaks = "1 day", date_labels = "%d-%m-%Y") +
  labs(color="Compliance Status")
This should create a scatter plot showing compliance and non-compliance of tax payers for the month of January, with the x-axis representing the Doc Date and the y-axis representing the Due Date. The points will be colored based on the compliance status.





