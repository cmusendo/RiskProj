library(readxl)
library(DataExplorer)


df <- read_excel("~/INFORMATICS PART FINAL/Final Year Project/Taxpayer Data/Taxpayer Returns Statistics (Copy).Anonymised.xlsx")


head(df)

summary(df)

str(df)


nrow(df)
ncol(df)

table(df$`ID#`)

DueDate <- as.Date(df$`Due Date`, format = "%d.%m.%y")
Doc.Date <- as.Date(df$`Doc. Date`, format = "%d.%m.%y")

DueDateNumeric <- as.numeric(DueDate)
DocDateNumeric <- as.numeric(Doc.Date)

diffence <-  DueDate + DocDateNumeric

plot(DocDateNumeric, DueDateNumeric, col= diffence , xlab=" Due Date" , ylab = "Doc Date" , main="Doc. Date vs Due Date")
lines(DueDateNumeric, col="blue")
lines(DocDateNumeric, col="orange")

legend("topright" , legend = c("Doc. Date", "Due Date"), col = c("red", "blue"), lty = 1)




