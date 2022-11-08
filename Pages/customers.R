library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)

path_data = "/home/leaveit/Documents/Data_Sci_Dashboard/data"
if (getwd() != path_data){
  setwd(path_data)
}

customer = read.csv("customer_details.csv")
attach(customer)

# Pre-processing
if (any(is.na(customer))){
  print("Yes")
}
gender = unique(customer.gender)
count.city = table(customer.city)
count.state = table(customer.state)
count.country = table(customer.country)
max.customers.country = sort(table(customer.country), decreasing = T)

customer.region = gsub("4orth", "North", customer.region)
customer.region = gsub("So3th", "South", customer.region)
count.region = table(customer.region)


# Visualization functions
# Pie chart of gender proportion in all regions.
data = as.data.frame(table(customer.gender))
total.proportion.gender <-ggplot(data, aes(x="", y="", fill=customer.gender)) +
                        geom_bar(stat="identity", width=1, color="white") +
                        coord_polar("y", start=0) +
                        theme_void() 

# Pie chart of gender proportion according to a country.
countries = unique(customer.country)
country.proportion.gender = function(country){
  data = as.data.frame(table(customer[customer.country == country, ]$customer.gender))
  piepercent<- round(100*data$Freq/sum(data$Freq), 1)
  pie(data$Freq, labels = piepercent, main = paste("gender in", country),col = rainbow(length(data$Freq)))
  legend("topright", c("Female","Male","Others"), cex = 0.8, fill = rainbow(length(data$Freq)))
}

# Bar Plot of number of customers in a region.
num.customers.region <- customer %>% count(customer.region)
x = 1:16
x = as.numeric(x)
num.customers <- function(){
  barplot(num.customers.region$n ~ x, 
          ylim = c(0, max(num.customers.region$n)+1),
          xlab="Regions",
          ylab="Number of customers",
          main = "Histogram of number of customers in a region",
          col = "darkred",
          names.arg = num.customers.region$customer.region
  )
}



