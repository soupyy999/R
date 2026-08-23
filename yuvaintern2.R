# Netflix Data Visualization using R

library(ggplot2)
library(dplyr)
library(tidyr)

# Load data
netflix <- read.csv("netflix_titles.csv", stringsAsFactors = FALSE)

# Basic overview
head(netflix)
str(netflix)
summary(netflix)

# Check missing values
colSums(is.na(netflix))

# Handle missing values
netflix$country[is.na(netflix$country)] <- "Unknown"
netflix$rating[is.na(netflix$rating)] <- "Unknown"
netflix$duration[is.na(netflix$duration)] <- "Unknown"
netflix$listed_in[is.na(netflix$listed_in)] <- "Unknown"

# Convert date_added and create year_added
netflix$date_added <- as.Date(
  trimws(netflix$date_added),
  format = "%B %d, %Y"
)

netflix$year_added <- as.numeric(
  format(netflix$date_added, "%Y")
)

# Movies dataset
movies <- netflix %>%
  filter(type == "Movie")

movies$duration_min <- as.numeric(
  gsub(" min", "", movies$duration)
)


# 1. Movies vs TV Shows

graph1 <- ggplot(netflix, aes(x = type)) +
  geom_bar() +
  labs(
    title = "Movies vs TV Shows on Netflix",
    x = "Content Type",
    y = "Number of Titles"
  ) +
  theme_minimal()

print(graph1)


# 2. Netflix Content Added by Year

yearly_content <- netflix %>%
  filter(!is.na(year_added)) %>%
  count(year_added)

graph2 <- ggplot(yearly_content,
                 aes(x = year_added, y = n)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Netflix Content Added by Year",
    x = "Year",
    y = "Number of Titles"
  ) +
  theme_minimal()

print(graph2)


# 3. Top 10 Countries

country_data <- netflix %>%
  separate_rows(country, sep = ",\\s*") %>%
  filter(country != "Unknown", country != "") %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 10)

graph3 <- ggplot(country_data,
                 aes(x = reorder(country, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Netflix Content",
    x = "Country",
    y = "Number of Titles"
  ) +
  theme_minimal()

print(graph3)


# 4. Movie Duration

graph4 <- ggplot(movies, aes(x = duration_min)) +
  geom_histogram(
    bins = 30,
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Movie Durations",
    x = "Duration (minutes)",
    y = "Number of Movies"
  ) +
  theme_minimal()

print(graph4)


# 5. Content Ratings

rating_data <- netflix %>%
  count(rating, sort = TRUE) %>%
  slice_head(n = 10)

graph5 <- ggplot(rating_data,
                 aes(x = reorder(rating, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top Content Ratings on Netflix",
    x = "Rating",
    y = "Number of Titles"
  ) +
  theme_minimal()

print(graph5)


# 6. Movie Duration vs Release Year

graph6 <- ggplot(
  movies,
  aes(x = release_year, y = duration_min)
) +
  geom_point(
    alpha = 0.4,
    na.rm = TRUE
  ) +
  labs(
    title = "Movie Duration vs Release Year",
    x = "Release Year",
    y = "Duration (minutes)"
  ) +
  theme_minimal()

print(graph6)


# 7. Movies and TV Shows Added by Year

type_year <- netflix %>%
  filter(!is.na(year_added)) %>%
  count(year_added, type)

graph7 <- ggplot(
  type_year,
  aes(
    x = year_added,
    y = n,
    group = type,
    linetype = type
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Movies and TV Shows Added by Year",
    x = "Year",
    y = "Number of Titles",
    linetype = "Content Type"
  ) +
  theme_minimal()

print(graph7)


# 8. Top 10 Genres

genre_data <- netflix %>%
  separate_rows(listed_in, sep = ",\\s*") %>%
  filter(listed_in != "Unknown", listed_in != "") %>%
  count(listed_in, sort = TRUE) %>%
  slice_head(n = 10)

graph8 <- ggplot(
  genre_data,
  aes(x = reorder(listed_in, n), y = n)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Netflix Genres",
    x = "Genre",
    y = "Number of Titles"
  ) +
  theme_minimal()

print(graph8)


# Some useful results

cat("Total number of titles:", nrow(netflix), "\n")

cat("\nMovies and TV Shows:\n")
print(table(netflix$type))

cat("\nTop 10 Countries:\n")
print(country_data)

cat("\nTop 10 Ratings:\n")
print(rating_data)

cat("\nTop 10 Genres:\n")
print(genre_data)