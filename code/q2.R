#Q2 -- What are the most frequent locations (e.g., Residential, Commercial Areas) for bike thefts in Toronto? 
  #And what proportion it is (round to one decimal place)?

q2_data <- bike_data %>%
  group_by(location) %>%
  #calculate # observations per "location" & their proportion of the total # observations (31,833)
  #round these proportions to 1 decimal
  summarize(num = n(), perc = round((num/31833), 1)) %>%
  arrange(desc(num))
print(q2_data)
  #7 observations, 3 variables

#assign location with most bike thefts & its proportion of the total # observations
location <- q2_data$location[1]
  #OUTPUT: "Residential Structures"
percentage <- q2_data$perc[1]
  #OUTPUT: 0.5
print(location)
print(percentage)


#Make a plot
q2_plot_I <- ggplot(data=q2_data, aes(x=location, y=num)) + geom_col() +
  labs(title="Number of Stolen Bikes in Toronto per Location (2014-2023)", x="Location",
       y="# Bikes Stolen") +
  scale_y_continuous(breaks=seq(2500,15000,2500)) +
  theme(plot.title=element_text(size=11), axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9), axis.text.x=element_text(size=7,angle=45, hjust=1))
q2_plot_I