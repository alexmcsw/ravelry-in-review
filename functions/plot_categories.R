plot_categories <- function() {
    df_category <- df_year |> mutate(
        category = case_when(
            grepl(accessory, tolower(pattern_name)) ~ "Bags",
            grepl(cardigan, tolower(pattern_name)) ~ "Cardigans",
            grepl(hats, tolower(pattern_name)) ~ "Hats",
            grepl(mitts, tolower(pattern_name)) ~ "Mittens",
            grepl(scarf, tolower(pattern_name)) ~ "Scarves",
            grepl(shirt, tolower(pattern_name)) ~ "Tops",
            grepl(socks, tolower(pattern_name)) ~ "Socks",
            grepl(sweater, tolower(pattern_name)) ~ "Sweaters",
            grepl(vest, tolower(pattern_name)) ~ "Vests",
            .default = "Other"
        ),
        category = case_when(
            category == "Other" & grepl(accessory, tolower(name)) ~ "Bags",
            category == "Other" & grepl(cardigan, tolower(name)) ~ "Cardigans",
            category == "Other" & grepl(hats, tolower(name)) ~ "Hats",
            category == "Other" & grepl(mitts, tolower(name)) ~ "Mittens",
            category == "Other" & grepl(scarf, tolower(name)) ~ "Scarves",
            category == "Other" & grepl(shirt, tolower(name)) ~ "Tops",
            category == "Other" & grepl(socks, tolower(name)) ~ "Socks",
            category == "Other" & grepl(sweater, tolower(name)) ~ "Sweaters",
            category == "Other" & grepl(vest, tolower(name)) ~ "Vests",
            .default = category
        ),
        # a bit of manual entry here
        # lesson is to be more specific in my rav projects!!
        category = case_when(
        grepl("tulip|delphinus|vair|cloudbow", tolower(name)) ~ "Sweaters",
        grepl("sulina|kostner|silvatica|snaraness|gossamer", tolower(name)) ~ "Scarves",
        grepl("springfrost|coaticook|reverie", tolower(name)) ~ "Socks",
        grepl("willapund", tolower(name)) ~ "Vests",
        grepl("llelua", tolower(name)) ~ "Hats",
        grepl("cyanotype", tolower(name)) ~ "Mittens",
        .default = category
        )
        ) |>
        group_by(category) |>
        summarize(count = n())

        df_category |> ggplot(aes(x = category, y = count, colour = category)) +
        geom_segment(aes(x = category, xend = category, y = 0, yend = count, alpha = 0.9), linewidth = 2) +
        geom_point(size = 15) +
        geom_text(
        size = 10,
        label = sample(c(
            "\U1F352", #cherry
            "\U1F370", #cake
            "\U1F353", #strawb
            "\U1F337", #tulip
            "\U1F36D", #lolly
            "\U1F351", #peach
            "\U1F33C", #blossom
            "\U1F407", #bunny
            "\U1F41D", #bee
            "\U1F433" #whale
        ),
        nrow(df_category)
        ),
        colour = "white",
        nudge_y = 0.2
        ) +
        theme_minimal() +
        theme(
        legend.position = "None",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size = 15),
        axis.text.y = element_text(size = 20)
        ) + 
        scale_colour_manual(values = sample(palette), nrow(df_category)) +
        coord_cartesian()
}