favourited_project <- function(df) {
  plot <- ggplot() + geom_image(aes(x = 1,  y= 1, image = sample(images, 1)), size = 0.7) + theme_void()

  table <- most_favourited |> 
      select(name, favorites_count, links.self.href, comments_count) |>
      gt() |>
      fmt_url(
      columns = links.self.href,
      label = "Ravelry",
      show_underline = FALSE
    ) |>
      cols_add(heart = "Heart", comment = "comment") |>
      fmt_icon(heart, fill_color = "red", stroke_color = "red") |>
      fmt_icon(comment, fill_color = "blue", stroke_color = "blue") |>
      cols_merge(
          columns = c(name, links.self.href),
          pattern = "{1} ({2})"
      )  |>
      cols_merge(
          columns = c(favorites_count, heart, comments_count, comment),
          pattern = "{2} {1} {4} {3}"
      ) |>
      cols_width(name ~ pct(75), favorites_count ~ pct(25)) |>
      cols_label(name = "Project", favorites_count = "") |>
      opt_stylize(color = "pink") |>
      tab_options(
        table.width = pct(90),
        container.overflow.y = FALSE
      )
    


  plot

  table
}