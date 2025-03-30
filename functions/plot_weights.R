plot_weights <- function() {

set.seed(Sys.Date())

weights <- c("Lace", "Light Fingering", "Fingering", "Sport", "DK", "Worsted", "Aran", "Bulky")

df_weights <- df_year |>
  select("packs.yarn_weight.name", "packs.yarn_weight.name1") |>
  mutate(
    weight = case_when(
      is.na(packs.yarn_weight.name) ~ packs.yarn_weight.name1,
      .default = packs.yarn_weight.name
    ),
    .keep = "unused"
  ) |>
group_by(weight) |>
summarize(count = n()) |>
filter(!is.na(weight)) |>
mutate(weight = weight |> factor(levels = weights)) |>
rbind(
  data.frame(
      weight = weights,
      count = 0
  )
) |>
distinct(weight, .keep_all = TRUE) |>
arrange(weight)

df_weights |> ggplot(aes(x=as.factor(weight), y=count, alpha = 0.9)) +
geom_bar(stat="identity", aes(fill = weight)) +
theme_minimal() +
theme(
  legend.position = "None",
  axis.title.x=element_blank(),
  axis.title.y=element_blank(),
  axis.text.x = element_text(angle = 30, hjust=1, size = 15),
  axis.text.y = element_text(size = 20)
) + 
scale_fill_manual(values = sample(palette, 8)) +
xlab("weight")
}