# https://tidyr.tidyverse.org/articles/rectangle.html

library(tidyr)
library(dplyr)
library(repurrrsive)


# github users data -------------------------------------------------------


users <- tibble(user = gh_users)
names(users$user[[1]])

# all columns
users |>  unnest_wider(user)

# select columns
users |>
  hoist(
    user,
    followers = "followers",
    login = "login",
    url = "html_url"
  )



# github repos data -------------------------------------------------------

repos <- tibble(repo = gh_repos)
names(repos$repo[[1]])

repos <- repos |>
  unnest_longer(repo)

names(repos$repo[[1]])

repos |>
  hoist(
    repo,
    login = c("owner", "login"),   # get login from owner
    name = "name",
    homepage = "homepage",
    watchers = "watchers_count",
  ) # |>
  # filter(!is.na(homepage)) |>
  # arrange(desc(homepage))

# explanation of login = c("owner", "login")
repos %>%
  hoist(repo, owner = "owner") %>%
  unnest_wider(owner)



# game of thrones character data ------------------------------------------

chars <- tibble(char = got_chars)

names(chars$char[[1]])

chars2 <- chars |>
  unnest_wider(char)

chars2 |> select_if(is.list)

chars2 |>
  select(name, titles) |>
  unnest_longer(titles) |>
  group_by(name) |>
  summarize(titles = str_c(titles, collapse = "|")) |>
  filter(str_length(titles) > 0)
