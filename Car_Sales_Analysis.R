# Importing the Libraries
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("reshape")
library(reshape)


# Loading the dataset
car_sales_data=read.csv("Car_sales.csv")
head(car_sales_data)

# Checking for missing values
head(is.na(car_sales_data))

# Removing missing Variable
car_data=na.omit(car_sales_data)

# Checking if there are still missing variables
sum(is.na(car_data))

# Checking the Data Structure
str(car_data)

# Checking the Latest Launch Column
class(car_data$Latest_Launch)
head(car_data$Latest_Launch)

# Changing to Date Format
install.packages("lubridate")
library(lubridate)
car_data$Latest_Launch <- mdy(car_data$Latest_Launch)
str(car_data)

# Grouping the same Manufacturer and then finding the count
Manufacturer_count <- car_data %>%
  group_by(Manufacturer) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(5)


# Print the Manufacturer count
print(Manufacturer_count)


# Graphical Representation count of Manufacturer using Bar Chart
ggplot(data = car_data,aes(x=Manufacturer,fill=Manufacturer)) + geom_bar()

# Find the top sales of the models in thousands
top_selling_models <- car_data %>%
  arrange(desc(Sales_in_thousands)) %>%
  select(Model, Sales_in_thousands)

# Find the top 5 sales of the models in thousands
top_5_selling_models <- car_data %>%
  arrange(desc(Sales_in_thousands)) %>%
  select(Model, Sales_in_thousands) %>%
  head(5)

# Print the top 5 selling models
print(top_5_selling_models)

# Visualize the top 5 selling models
ggplot(top_5_selling_models, aes(x = reorder(Model, Sales_in_thousands), y = Sales_in_thousands, fill = Model)) +
  geom_col() +
  labs(title = "Top 5 Selling Models", x = "Model", y = "Sales (thousands)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2")

# Install and load the dplyr and ggplot2 packages
install.packages(c("dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)

# Group data by Manufacturer and Model, and calculate total sales
top_selling_models <- car_data %>%
  group_by(Manufacturer, Model) %>%
  summarise(total_sales = sum(Sales_in_thousands))

# Visualize the top-selling models
ggplot(top_selling_models, aes(x = reorder(Model, total_sales), y = total_sales, fill = Manufacturer)) +
  geom_col() +
  labs(title = "Top-Selling Models", x = "Model", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "none")

# scatter plot to visualize the relationship between price and sales.


ggplot(car_data, aes(x = Price_in_thousands, y = Sales_in_thousands)) +
  geom_point() +
  labs(title = "Price vs Sales", x = "Price (thousands)", y = "Sales (thousands)")

# bar chart to help compare the sales performance of different price segments.


# Create price segments
car_data$Price_Segment <- cut(car_data$Price_in_thousands, 
                              breaks = c(0, 20, 40, 60, 80, 100), 
                              labels = c("0-20", "21-40", "41-60", "61-80", "81-100"))

# Group by price segment and calculate total sales
price_segment_sales <- car_data %>%
  group_by(Price_Segment) %>%
  summarise(total_sales = sum(Sales_in_thousands))

# Bar chart
ggplot(price_segment_sales, aes(x = Price_Segment, y = total_sales, fill = Price_Segment)) +
  geom_col() +
  labs(title = "Sales by Price Segment", x = "Price Segment", y = "Total Sales")




