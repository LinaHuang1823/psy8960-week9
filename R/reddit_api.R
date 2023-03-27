#Script Settings and Resources
library(jsonlite)
library(tidyverse)
library(httr)

#Data Import and Cleaning
# Create list with all content from https://www.reddit.com/r/rstats/.json
rstats_list <- fromJSON("https://www.reddit.com/r/rstats/.json")
#create a variable rstats_original_tbl that contains all per-post data 
rstats_original_tbl <- fromJSON("https://www.reddit.com/r/rstats/.json", flatten = TRUE)$data$children
#create a variable that contains the post, upvotes, and comments variables for the 25 initial posts 
rstats_tbl <- fromJSON("https://www.reddit.com/r/rstats/.json", flatten = TRUE)$data$children %>%
  select(data.title, data.ups, data.num_comments) %>% #selects the data.title, data.ups, and data.num_comments variables for each post
  slice(1:25) %>% # select the 25 initial posts
  setNames(c("post", "upvotes", "comments"))  #renames the selected variables

#Visualization
# Create a scatterplot using ggplot with rstats_tbl as the data source
ggplot(rstats_tbl, aes(x = upvotes, y = comments)) + # Initialize the ggplot with the data and map upvotes to the x-axis and comments to the y-axis 
  geom_point() +  # Add a scatterplot layer (geom_point) to display each data point as a point
  labs(x = "Upvotes", y = "Comments", title = "Relationship between Upvotes and Comments") # Set the axis labels and the plot title

#Analysis
correlation_test <- cor.test(rstats_tbl$upvotes, rstats_tbl$comments) # Calculate correlation and p-value

#Publication
#The correlation between upvotes and comments was r(23) = .11, p = .61. This test was not statistically significant.
p_value <- round(correlation_test$p.value, 2) # Format p values
correlation <- round(correlation_test$estimate, 2) #correlation
df <- correlation_test$parameter  # extract df
significant <- ifelse(p_value < 0.05, "was", "was not") # Check for significance
# Print the result
print(sprintf("The correlation between upvotes and comments was r(%d) = .%g, p = .%g. This test %s statistically significant.", df, (correlation * 100) %% 100, (p_value * 100) %% 100, significant))