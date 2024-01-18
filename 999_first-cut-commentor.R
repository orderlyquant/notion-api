library(tidyverse)
library(glue)
library(httr2)

source("code/00_notion_credentials.R")
source("code/01_functions.R")

# Page of "Read" links
read_url <- "https://www.notion.so/Read-81e647a9d50a430b89ab502c8ff74503"
read_tbl <- get_children_blocks(notion_url_to_id(read_url)) |>
  tibble() |>
  purrr::set_names("page") |>
  hoist(
    page,
    id = "id"
  )

tictoc::tic()
read_tbl2 <- read_tbl |>
  mutate(
    title = map_chr(id, get_page_title)
  ) |>
  mutate(
    comments = map(id, comments_with_text_for_page_id)
  )
tictoc::toc()

comments_tbl <- read_tbl2 |>
  select(title, comments) |>
  unnest(comments) |>
  select(title, comment_text, text)

# writexl::write_xlsx(comments_tbl, "all_comments.xlsx")




# # Open AI's App Store Moment
# oai_url <- "https://www.notion.so/link-OpenAI-s-App-Store-Moment-VCs-Love-Affair-with-AI-By-the-Numbers-837f2643412a42e98a91b3e0a203a708"
# oai_comments <- comments_with_text_for_page_id(notion_url_to_id(oai_url))
# oai_comments
#
#
# comments_with_text_for_page_id("1c2adb88-5fa8-4ec9-9f3c-2421a42b8190")

