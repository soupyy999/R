library(ggplot2)

x <- c("Movie", "TV Show", "Movie", "Movie", "TV Show", "Movie")

ggplot(data.frame(x), aes(x = x)) +
  geom_bar()