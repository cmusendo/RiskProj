library(readxl)

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

#Loading the Data from the excel sheet
pacman::p_load(pacman, party, psych, rio, tidyverse)
tax_data <- read_excel("Taxpayer Data Consolidated.xlsx")

summary(tax_data)

head(tax_data)
#DATA CLEANING FOR THE DATE

# Convert "Return Date" column to Date format
tax_data$`Return. Date` <- as.Date(tax_data$`Return. Date`, format = "%d.%m.%Y")

# Convert "Due Date" column to Date format
tax_data$`Due Date`<- as.Date(tax_data$`Due Date`, format = "%d.%m.%Y")

tax_data$`Date Paid`<- as.Date(tax_data$`Date Paid`, format = "%d.%m.%Y")

head(tax_data)







hist(tax_data$`Amount Due`, 
     main = "Distribution of Amount Due", 
     xlab = "Amount Due", 
     ylab = "Frequency",
     breaks = 60)

# Create a subset of the data for 5 different taxpayers
plot_data <- subset(tax_data, tax_data$`ID#` %in% c("BP278"))

head(plot_data)
# Create a scatterplot
plot(plot_data$`Amount Due`, plot_data$`Amount Paid`, 
     xlab = "Amount Due", ylab = "Amount Paid", 
     main = "Scatterplot of Amount Due vs. Amount Paid for 5 taxpayers")



library(ggplot2)

# Create a new column "Compliance" that indicates whether the taxpayer is compliant or not
tax_data$Compliance <- ifelse(tax_data$`Date Paid` <= tax_data$`Due Date`, "Compliant", "Non-Compliant")

# Create a pie chart of taxpayer compliance
ggplot(data = tax_data, aes(x = "", fill = Compliance)) +
  geom_bar(width = 1) +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title = "Taxpayer Compliance")

tax_data$Amount_Paid <- tax_data$`Amount Due`- tax_data$`Amount Paid`

summary(tax_data$Taxhead)
# Use the table function to get a frequency table of the Taxhead column
table(tax_data$Taxhead)

# Create a bar graph of the Taxhead column
ggplot(tax_data, aes(x = Taxhead)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Distribution of Tax Heads", x = "Tax Head", y = "Count")

# Create a distribution plot of amount paid
ggplot(data = tax_data, aes(x = Amount_Paid)) +
  geom_density(fill = "lightblue", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(Amount_Paid)), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Amount Paid", x = "Amount Paid", y = "Density")


# Create a histogram of "Amount Paid"
hist(tax_data$`Amount Paid`, breaks = 10, xlab = "Amount Paid", ylab = "Frequency")

#Data Cleaning 
#Removing Null values
sum(is.na(tax_data))
tax_data <- na.omit(tax_data)

# Remove duplicate rows from the data frame
tax_data <- distinct(tax_data)

# Check the data types of each column
str(tax_data)

#  box plot of the Amount Paid column to check for outliers
ggplot(tax_data, aes(y = `Amount Paid`)) +
  geom_boxplot()
head(tax_data)

# Create a new column "Compliance" that indicates whether the taxpayer is compliant or not
tax_data$Compliance <- ifelse(tax_data$`Date Paid` <= tax_data$`Due Date`, "Compliant", "Non-Compliant")

# Create a pie chart of taxpayer compliance
ggplot(data = tax_data, aes(x = "", fill = Compliance)) +
  geom_bar(width = 1) +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title = "Taxpayer Compliance")

tax_data$Amount_Paid <- tax_data$`Amount Due`- tax_data$`Amount Paid`

summary(tax_data$Taxhead)
# Use the table function to get a frequency table of the Taxhead column
table(tax_data$Taxhead)

# Create a bar graph of the Taxhead column
ggplot(tax_data, aes(x = Taxhead)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Distribution of Tax Heads", x = "Tax Head", y = "Count")


ggplot(tax_data, aes(x = tax_data$`Amount Due`, y = tax_data$`Amount Paid`)) +
  geom_point() +
  geom_smooth(method = "lm")


# Create a new column "Year" that extracts the year from the "Return. Date" column
tax_data$Year <- format(as.Date(tax_data$`Return. Date`, format = "%d.%m.%Y"), "%Y")

# Create a summary data frame that shows the total count of compliant and non-compliant taxpayers by year
compliance_summary <- aggregate(Compliance ~ Year + Compliance, data = tax_data, FUN = length)
head(compliance_summary) 
# Create a line graph that shows the compliance levels by year
compliance_summary$Year <- as.factor(compliance_summary$Year)

# Create line graph
ggplot(data = compliance_summary, aes(x = Year, y = Compliance, group = 1)) +
  geom_line() +
  labs(x = "Year", y = "Compliance Level", title = "Compliance Levels by Year")




# Filter data for records where return date > due date and amount due = 0
filtered_data <- tax_data %>% filter(`Return. Date` > `Due Date` & `Amount Due` == 0)

# Plot compliance levels by return date
ggplot(data = filtered_data, aes(x = `Return. Date`, fill = Compliance)) +
  geom_bar() +
  labs(x = "Return Date", y = "Count", title = "Compliance Levels by Return Date") +
  theme(legend.position = "bottom")

# Plot compliance levels by due date
ggplot(data = filtered_data, aes(x = `Due Date`, fill = Compliance)) +
  geom_bar() +
  labs(x = "Due Date", y = "Count", title = "Compliance Levels by Due Date") +
  theme(legend.position = "bottom")

# Plot compliance levels by return date and due date
ggplot(data = filtered_data, aes(x = `Return. Date`, y = `Due Date`, color = Compliance)) +
  geom_point() +
  labs(x = "Return Date", y = "Due Date", title = "Compliance Levels by Return Date and Due Date")

