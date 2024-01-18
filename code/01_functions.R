notion_url_to_id <- function(url) {
  url |>
    str_extract("([a-z0-9]{32})$")
}

get_page_title <- function(page_id) {

  req <- httr2::request(
    glue::glue("https://api.notion.com/v1/pages/{page_id}")
  ) |>
    httr2::req_headers(
      Authorization = glue::glue("{notion_secret}"),
      `Notion-Version` = "2022-06-28"
    )

  print(
    glue::glue(
      "{Sys.time()}",
      " - api request: get_page_title({page_id})"
    )
  )
  resp <- httr2::req_perform(req)

  resp_json <- resp |>
    httr2::resp_body_json()

  title_text <- resp_json[["properties"]][["title"]][["title"]] |>
    tibble(title = _) |>
    unnest_wider(title) |>
    summarize(
      text = str_c(plain_text, collapse = "") |> str_trim()
    ) |>
    pull(text)

  return(title_text)

}

get_children_blocks <- function(page_id) {
  req <- httr2::request(
    glue::glue("https://api.notion.com/v1/blocks/{page_id}/children?page_size=100")
  ) |>
    httr2::req_headers(
      Authorization = glue::glue("{notion_secret}"),
      `Notion-Version` = "2022-06-28"
    )

  print(
    glue::glue(
      "{Sys.time()}",
      " - api request: get_children_blocks({page_id})"
    )
  )
  resp <- httr2::req_perform(req)
  resp_json <- resp |> httr2::resp_body_json() |> pluck("results")

  return(resp_json)
}

get_comments_for_block <- function(id) {
  req <- httr2::request(
    glue::glue("https://api.notion.com/v1/comments?block_id={id}")
  ) |>
    httr2::req_headers(
      Authorization = glue::glue("{notion_secret}"),
      `Notion-Version` = "2022-06-28"
    )

  print(
    glue::glue(
      "{Sys.time()}",
      " - api request: get_comments_for_block({id})"
    )
  )
  resp <- httr2::req_perform(req) |>
    resp_body_json() |>
    pluck("results") |>
    map("rich_text")

  return(resp)
}

comments_with_text_for_page_id <- function(page_id) {

  page_tbl <- tibble(
    blocks = get_children_blocks(page_id)
  )

  if(nrow(page_tbl) == 0) {
    return(tibble())
  } else {
    page_tbl <- page_tbl |>
      hoist(
        blocks,
        id = "id",
        comment_type = "type"
      ) |>
      mutate(
        comment = map(id, get_comments_for_block)
      )

    page_tbl2 <- page_tbl |>
      unnest_longer(comment)

    if(nrow(page_tbl2) == 0) {
      return(tibble())
    } else {
      page_tbl2 <- page_tbl |>
        unnest_longer(comment) |>
        group_by(id) |>
        mutate(comment_id = row_number()) |>
        unnest_longer(comment) |>
        hoist(
          comment,
          type = "type",
          text = "plain_text"
        ) |>
        filter(type == "text") |>
        group_by(id, blocks, comment_type, type, comment_id) |>
        summarize(
          comment_text = str_c(text, collapse = "") |> str_trim()
        ) |>
        ungroup()

      return(
        page_tbl2 |> mutate(
          content = map2(comment_type, blocks, \(x, y) y |> pluck(x))
        ) |>
          select(id, comment_id, comment_text, content) |>
          unnest_wider(content) |>
          unnest_longer(rich_text) |>
          unnest_wider(rich_text) |>
          group_by(id, comment_id, comment_text) |>
          summarize(
            text = str_c(plain_text, collapse = "") |> str_trim()
          )
      )
    }
  }

}
