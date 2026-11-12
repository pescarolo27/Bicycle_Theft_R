# Insights into Bicycle Theft using Visualization (R)

**Background:** Known for its vibrant arts scene, diverse culture, stunning skyline, and bustling neighborhoods, Toronto is a city that never sleeps. However, as with any major urban center, it faces its share of challenges. One growing concern for Torontonians is the rising number of bicycle thefts.  
You have been invited to assist the Toronto Police Service by analyzing and visualizing data to uncover patterns in theft activity. Your findings and visual insights will provide crucial information that can help allocate resources more effectively and develop strategies to combat bike thefts, ensuring a safer city for all cyclists.

**The Data:** The dataset used for analyzing bike thefts is titled `Cleaned_Bicycle_Thefts_Open_Data.csv`. This dataset contains essential information regarding bicycle theft incidents in a given city including when and where they occur, the financial impact, and other spatial and temporal factors. By analyzing this data, valuable insights including patterns and trends can be obtained that can inform strategies to mitigate bike thefts and enhance urban planning.

This project was done in June, 2025. As an important note, this project is coded for a R environment. Files were attached both for code based in R as well as a project write-up done in Python containing code meant for R. The R files correspond to the three main analysis sections in the Python file (e.g. "q1.R" relates to Analysis I). Running the Python notebook will produce errors in the code cells & destroy the results that are currently contained in it.

---------------------------------------------------------------------------------------------------------------------------------------------------------

### Brief Summary
This project examined information on bike thefts reported in Toronto between 2014-2023. In particular, bike thefts were aggregated by quarter & location type, & costs of stolen bikes were assessed across the neighborhoods in the city. Over these ten years, 31,833 bike thefts were reported.  
No preprocessing of the data was necessary given that it did not contain any problematic inconsistencies.

#### Stolen Bikes by Quarter
The ten-year period was truncated into quarters to evaluate how bike thefts generally evolved over the course of a year. For clarity, the four quarters spanned three-month periods, specifically January 1st - March 31st, April 1st - June 30th, July 1st - September 30th, & October 1st - December 31st.

Over the first three quarters, the number of bike thefts roughly doubled; 2,890 in quarter one, 5,003 in quarter two, & 13,326 in quarter three. The fourth quarter reported the second highest number of thefts at 10,614. If these quantities were broken down to a daily basis per year, each quarter saw the following averages.
- About three bike thefts occurred on average per day in the first quarter between 2014-2023.
- About five bike thefts occurred on average per day in the second quarter between 2014-2023.
- Almost 15 bike thefts occurred on average per day in the third quarter between 2014-2023.
- Almost 12 bike thefts occurred on average per day in the fourth quarter between 2014-2023.

#### Stolen Bikes by Location Type
Location types were assessed to identify where bike thefts occur most often. Of the seven distinct location types defined, three account for more than 93% of the 31,833 reported bike thefts--Residential Structures (15,120), Commercial Areas (8,246), & Open/Public Spaces (6,424). No other location reported more than 715 thefts. The least popular area in terms of bike thefts, Group/Shared Homes, accounted for less than 0.4% of all of thefts in Toronto over these ten years.  
If these quantities were broken down to a daily basis per year, each of these three locations types reported the following averages.
- About two bike thefts occurred on average per day in Commercial Areas between 2014-2023.
- Nearly two bike thefts occurred on average per day in Open/Public Spaces between 2014-2023.
- About four bike thefts occurred on average per day from Residential Structures between 2014-2023.

#### Costs of Stolen Bikes by Neighborhood
Costs of stolen bikes across the 140 neighborhoods in Toronto were evaluated to determine where the most expensive bikes were stolen from. To get a better sense of the typical costs of these stolen bikes, the median was used.

The Bridle Path-Sunnybrook-York Mills neighborhood reported a median bike cost (of stolen bikes) of 1,186 Canadian dollars during this period, the only neighborhood with a higher typical cost greater than 990 CAD. Other neighborhoods whose stolen bikes were high in cost include Bedford Park-Nortown (990 CAD), Leaside-Bennington (870), & Edenbridge-Humber Valley (837). These four neighborhoods reported 47, 106, 234, & 40 stolen bikes over these ten years respectively.

#### Recommendations
On a yearly basis, bikes were stolen in the latter half of the year significantly more often, specifically in the third quarter (July through September). As such, police should be the most wary of bike theft & implement more enforcement strategies during these months. Conversely, they can relax their efforts relatively speaking in the first three months of the year which can allow police to spend more resources & time combating other types of crime & theft. This is not to say that they should disregard bike theft in the first two quarters of the year.

Of the bike thefts reported in Toronto between 2014-2023, almost 94% of them occurred in either Commercial Areas, Open/Public Spaces, or Residential Structures. In particular, almost half of these thefts were from Residential Structures. As such, police could spend more of their resources patrolling & enforcing these kinds of areas. Additionally, police could use stricter preventative measures in these locations; however, this might direct thieves to steal bikes in less-patrolled areas. Other measures beyond presence could be employed as well to assist bike owners who store their bikes in particular locations. For example, police could provide additional tools & security techniques to protect bikes in these areas.

When evaluated on a basis of cost, understanding where more valuable bikes are stolen from can help quantify where bike theft is most expensive in Toronto. In deciding how to distribute resources, cost & quantity of crime are important factors for law enforcement to consider. Generally, areas in which these factors are greater would demand more police presence & combative efforts because those areas are affected the most.  
With this in mind, the neighborhoods with the priciest bikes, including Bedford Park-Nortown, Bridle Path-Sunnybrook-York Mills, Edenbridge-Humber Valley, & Leaside-Bennington, should receive considerable amounts of police presence to help protect their bikes. This does not mean that other neighborhoods with less expensive bikes or less thefts should be disregarded, but more efforts, resources, & security measures should be allocated to these aforementioned areas in which bike theft is more costly for the residents & city of Toronto.

In totality, police can use these findings & trends in bike theft to guide their planning & enforcement efforts, but they can also use such information to combat bike theft in coordination with the city of Toronto. In other words, police can recommend strategies, tools, & other measures to the city that they cannot necessarily implement themselves to help thwart bike theft. For example, the city might better be able to develop & implement urban planning strategies & resources that can help aid bike owners more directly such as bike-tracking systems, locks, or stronger, more secure bike racks. While police can enforce the law & deter thieves from acting, having other systems in place that are not necessarily operated by law enforcement can help produce a more effective form of prevention & security.
