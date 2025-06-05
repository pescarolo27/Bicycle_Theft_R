#load in libraries
library(tidyverse)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(stringr)
library(forcats)
library(lubridate)

#import data
bike_data <- read_csv("Cleaned_Bicycle_Thefts_Open_Data.csv")
  #31,833 observations, 8 variables
head(bike_data)

################################################################################################

#Q1 -- Which quarter has the highest & lowest number of stolen bikes?

q1_data <- bike_data %>%
  #create a "new_quarter" column to determine quarter regardless of year
  mutate(new_quarter = case_when(str_detect(quarter, "01-01") ~ "Q1",
                                 str_detect(quarter, "04-01") ~ "Q2",
                                 str_detect(quarter, "07-01") ~ "Q3",
                                 str_detect(quarter, "10-01") ~ "Q4")) %>%
  group_by(new_quarter) %>%
  summarize(num_stolen = n()) %>%
  arrange(num_stolen)
print(q1_data)

#assign quarters with most/least bike thefts
high <- q1_data$new_quarter[4]
  #OUTPUT: "Q3"
low <- q1_data$new_quarter[1]
  #OUTPUT: "Q1"


#make a plot
q1_plot_I <- ggplot(data=q1_data, aes(x=new_quarter, y=num_stolen)) + geom_col() +
  labs(title="Number of Stolen Bikes in Toronto per Quarter (2014-2023)", x="Quarter", 
       y="# Bikes Stolen") +
  scale_y_continuous(breaks=seq(2500,15000,by=2500)) +
  theme(plot.title=element_text(size=11), axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10))
q1_plot_I