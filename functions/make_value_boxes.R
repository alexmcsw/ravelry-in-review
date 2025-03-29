make_value_boxes <- function(year) {
  completed_projects <<- df |>
  filter(status_name == "Finished") |> 
  filter(if_any(c(completed), str_detect, pattern = year)) |>
  count() |>
  as.numeric()

  gift_projects <<- df |>
  filter(!(made_for %in% c("me", "Me", "me!", "ME", "", NA))) |>
  filter(if_any(c(completed), str_detect, pattern = year)) |>
  count() |>
  as.numeric()

  started_projects <<- df |>
  filter(if_any(c(started, created_at), str_detect, pattern = year)) |>
  count() |>
  as.numeric()

  df_year <<- df |> filter(if_any(c(completed, created_at, started), str_detect, pattern = year))

  meterage <<- df_year |>
  select(contains("packs.total_meters")) |>
  mutate(across(everything(), as.numeric)) |>
  sum(na.rm = TRUE) |>
  as.numeric() / 1000 |> round(digits = 2)
}