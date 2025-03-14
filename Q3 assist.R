
library(readr)
#system("quarto check") ~verify installation
#install necessary packages:
#install.packages("shiny")


#print(names(subset_data))

#
#file.exists("subset_data.csv")

#file.exists("subset_data.csv")

#setwd("C:/Users/user/Downloads")  
#subset_data <- read_csv("subset_data.csv") 


write_csv(subset_data, "C:/Users/user/Downloads/subset_data.csv")
file.exists("C:/Users/user/Downloads/subset_data.csv")

subset_data <- read_csv("subset_data.csv") 


library(rsconnect)
rsconnect::deployApp('C:/Users/user/Downloads/app')
