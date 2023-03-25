#Script Settings and Resources
library(jsonlite)
library(tidyverse)
library(rvest)

#Data Import and Cleaning
# Create list with all content from https://www.reddit.com/r/rstats/.json
rstats_list <- fromJSON("https://www.reddit.com/r/rstats/.json")
# Create variable with all per-post data
rstats_original_tbl <- as.data.frame(rstats_list$data$children) #need to edit later
# Fetch JSON data from Reddit
url <- "https://www.reddit.com/r/rstats/.json"
response <- GET(url, add_headers("User-Agent" = "R"))
# Check if the request is successful
if (http_status(response)$message == "OK") {
  # Parse JSON content
  content <- content(response, as = "text")
  json_data <- fromJSON(content, flatten = TRUE)
  
  # Create rstats_tbl
  rstats_tbl <- json_data$data$children %>%
    lapply(function(x) {
      tibble(
        post = x$data$title,
        upvotes = x$data$ups,
        comments = x$data$num_comments
      )
    }) %>%
    bind_rows() %>%
    head(25)
  
  # Print first 25 rows
  print(rstats_tbl)
} else {
  cat("Error fetching data from Reddit.")
}

#Visualization
ggplot(rstats_tbl, aes(x = upvotes, y = comments)) +
  geom_point() +
  labs(
    title = "Relationship between Upvotes and Comments",
    x = "Upvotes",
    y = "Comments"
  )
#Analysis

