###############################################################################
# PLEASE NOTE:
# This legacy code created the first version of this plot, posted to social 
# media on 2024-11-15. After complete rework of the underlying data and a move 
# to a bivariate map, this code is kept as reference for the initial post.
# For the latest version of the plot, please checkout the complete code in 
# the Quarto document in this folder.
###############################################################################


library("tidyverse")
library("rnaturalearth")
library("scico")
library("marquee")
library("arrow")


# obtain map data from {rnaturalearth}
world_map <- ne_countries(scale = "small", returnclass = "sf")

# load ISO code data, which was generated in python beforehand:
countries <- arrow::read_parquet("countries.parquet")

# after some data wrangling a DataFrame countries_matches was created, 
# which was then joined to the map data for plotting:
world_map_data <- world_map |> 
  left_join(countries_matches, by = c("iso_a3_eh" = "alpha_3"))


ggplot(world_map_data) +
  aes(fill = match_label) +
  geom_sf() +
  scale_fill_scico_d(palette = "managua", name = "") +
  labs(
    title = "How ISO codes match the country names",
    subtitle = "Many ISO codes match the starting letters of the English name of a nation. Fewer ISO codes match the countries' acronyms or their permutation. However, most codes (139) do not match in any specific way.",
    caption = "Visualization by christiangebhard.com\nISO code data from the {ISOcodes} package via TidyTuesday for week 46 (2024).\n#30DayMapChallenge 2024, Day 14 'A World Map'"
  ) +
  coord_sf(crs = "+proj=robin +lon_0=0w") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Archivo", size = 13),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = marquee::element_marquee(
      hjust = 0.5,
      width = unit(14, "cm"), # needs manually fixed width
      style = marquee::classic_style(align = "center")
    ),
    plot.caption = element_text(size = 8)
  )
  
ggsave("ISOcodes.png", width = 10, height = 8, dpi = 300, bg = "white")
