library(tidyverse)

items <- read_delim("../data/BookCrossing/items_old.csv",
                    delim = ";",
                    escape_backslash = TRUE,
                    escape_double = F,
                    na = c("", "NA", "NULL"))

write_csv(items, "../data/BookCrossing/items.csv")


users <- read_delim("../data/BookCrossing/users_old.csv",
                    delim = ";",
                    escape_backslash = TRUE,
                    escape_double = F,
                    na = c("", "NA", "NULL"))

write_csv(users, "../data/BookCrossing/users.csv")


ratings <- read_delim("../data/BookCrossing/ratings_old.csv",
                    delim = ";",
                    escape_backslash = TRUE,
                    escape_double = F,
                    na = c("", "NA", "NULL")) %>%
		filter(rating > 0)

write_csv(ratings, "../data/BookCrossing/ratings.csv")
