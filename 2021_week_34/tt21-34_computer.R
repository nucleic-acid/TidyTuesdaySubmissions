## Load required packages ----

library("tidyverse")
library("ggrepel")
library("ggdark")
library("showtext")

## Setup custom fonts ----

font <- c("StarNext", "TNGcast")
path <- system.file(paste0("fonts/", font, ".ttf"), package = "trekfont")
for (i in 1:2) font_add(font[i], path[i])
font_add_google("Open Sans")
font_families()

showtext_auto()

## Read the computer interactions data ----

tuesdata <- tidytuesdayR::tt_load(2021, week = 34)
computer <- tuesdata$computer

glimpse(computer)


# count how often a character located someone else
searches_by_people <- computer %>%
  # ignore interactions by the computer and ignore the Wake Word "Computer" itself
  filter(sub_domain == "Locate", !str_detect(char, pattern = "[Cc]omputer"), type != "Wake Word")


searches_by_people_count <- searches_by_people %>%
  count(char, sort = TRUE) %>%
  mutate(
    char = str_to_lower(char),

    # use last name for later joining
    char = ifelse(char == "beverly", "crusher", char),
    char = ifelse(char == "geordi", "la forge", char)
  )

## Count how often characters are being located ----

# Define People of interest (this is not a complete cast list, but the result of skimming ~90 entries)
people <- str_to_lower(c(
  "data", "picard", "captain", "riker", "pulaski", "Goss", "Tam Elbrun", "Barclay",
  "Dalen Quaice", "Hill and Selar", "Worf", "La Forge", "Vash", "Diana", "Troi", "Crusher", "Ensign Ro",
  "Alexander Rozhenko", "Uhnari", "Morag"
))

# Create a Regex pattern by collapsing the vector with the "or" operator
people_pattern <- paste0(people, collapse = "|")


people_searched <- searches_by_people %>%
  mutate(
    # make the interactions strings to lower case
    interaction_lower = str_to_lower(interaction),

    # reduce the interactions strings to the searched person
    # e.g. from "computer, locate commander riker" --> "riker" is extracted.
    # Caution: This is not the best / generalizable way, but a rather hacky approach
    # due to limited time. It works for this use case / dataset.
    person_of_interest = str_extract(interaction_lower, pattern = people_pattern)
  ) %>%
  select(interaction, person_of_interest) %>%
  filter(!is.na(person_of_interest)) %>%
  count(person_of_interest, sort = TRUE) %>%
  mutate(person_of_interest = ifelse(person_of_interest == "captain", "picard", person_of_interest))

## Join the two dataframes ----

whereabouts <- searches_by_people_count %>%
  full_join(people_searched, by = c("char" = "person_of_interest")) %>%
  rename(searching = n.x, searched = n.y) %>%
  mutate(char = str_to_title(char)) %>%
  replace_na(list(searching = 0L, searched = 0L)) %>%
  filter(searched > 1) # limit to freuquently located characters

## Create graph ----

whereabouts %>%
  ggplot(aes(searching, searched)) +
  geom_point() +
  geom_label_repel(
    aes(label = char),
    box.padding = 0.5,
    label.padding = 0.5,
    max.time = 1,
    max.iter = 100000
  ) +
  labs(
    title = "Where is Captain Picard?",
    subtitle = "How often did Characters ask the computer to locate someone\nvs. how often are they being located via the computer.\n",
    x = "Times searching someone",
    y = "Times being searched",
    caption = "\n@c_gebhard | #TidyTuesday Week 34 (2021)\nData source: http://www.speechinteraction.org/TNG/"
  ) +
  dark_theme_minimal() +
  theme(
    plot.title = element_text(
      family = "StarNext",
      face = "bold",
      size = rel(2.5),
      hjust = 0,
      vjust = 10
    ),
    plot.subtitle = element_text(
      family = "Open Sans",
      size = rel(1.3),
      hjust = 0
    ),
    plot.caption = element_text(
      size = rel(1.1),
      face = "italic",
      hjust = 1
    ),
    plot.caption.position = "plot",
    plot.margin = margin(1.5, 0.4, 0.4, 0.4, unit = "cm"),
    axis.title = element_text(
      face = "bold",
      size = rel(1.3)
    ),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), angle = 90),
    axis.text = element_text(
      size = rel(1.3)
    )
  )

## Store the plot ----
ggsave("2021_week_34/tt21-34_picard.png", dpi = 96, height = 6, width = 10)
