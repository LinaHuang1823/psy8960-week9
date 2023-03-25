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
#Analysis