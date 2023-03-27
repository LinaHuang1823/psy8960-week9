#Script Settings and Resources
library(tidyverse)
library(rvest)

#Data Import and Cleaning
# Scrape data from Reddit
url <- "https://old.reddit.com/r/rstats/"
rstats_html <- read_html(url)

# Extract the required information
post_titles <- rstats_html %>%
  html_elements(".title a.title") %>%
  html_text()

upvotes <- rstats_html %>%
  html_elements(".midcol.unvoted .score.unvoted") %>%
  html_text() %>%
  str_replace("k", "000") %>%
  as.integer()

comments <- rstats_html %>%
  html_elements(".buttons .first a") %>%
  html_text() %>%
  str_extract("\\d+") %>%
  as.integer()

# Create rstats_tbl
rstats_tbl <- tibble(
  post = post_titles,
  upvotes = upvotes,
  comments = comments
)

# Print first 25 rows
print(head(rstats_tbl, 25))

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
