#Q3 -- In which region of Toronto is the median value of stolen bikes the highest?

q3_data <- bike_data %>%
  group_by(neighborhood) %>%
  #get total # thefts & median of "bike_cost"
  summarize(num_thefts = n(), med_bike_cost = median(bike_cost)) %>%
  #sort by median bike cost in descending order
  arrange(desc(med_bike_cost)) %>%
  #split the original "neighborhood" column into 2 to separate the neighborhood id from neighborhood
  mutate(neighborhood_id = str_extract(neighborhood, "([0-9]+)"),
         neighborhood = str_replace(neighborhood, "\\([0-9]+\\)", "")) %>%
  #alter the order of the columns
  select(neighborhood, neighborhood_id, num_thefts, med_bike_cost)
#print(q3_data)
  #140 observations, 4 variables

#assign the neighborhood/region with the highest median value of "bike_cost"
region <- q3_data$neighborhood[1]
  #OUTPUT: "Bridle Path-Sunnybrook-York Mills"
print(region)


#Make a plot with the top-5 neighborhoods
q3_data_sub <- q3_data %>%
    slice_max(order_by=med_bike_cost, n=5) %>%
  #some locations have same 'med_bike_cost'
  head(5)
q3_data_sub

q3_plot_I <- ggplot(data=q3_data_sub, aes(x=neighborhood, y=med_bike_cost)) + geom_col() +
  geom_text(aes(label=num_thefts), vjust=-0.3, size=3.5) +
  labs(title="Costs of Stolen Bikes in Toronto Neighborhoods (2014-2023)", x="Neighborhood",
       y="Median Bike Cost ($)") +
  scale_y_continuous(breaks=seq(200,1200,200)) +
  theme(plot.title=element_text(size=11), axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10), 
        axis.text.x=element_text(size=8, angle=25, hjust=0.8, vjust=0.9))
q3_plot_I