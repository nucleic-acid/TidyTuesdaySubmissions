
## ---- Load required packages
library("tidyverse")
library("rmarkdown")
library("countrycode")
library("ggrepel")
library("png")

# ---- Load plotting theme
source("resources/jolly_theme.R")


# ---- Read the olympic data
athlete_events <- read_csv("2021_week_31/data/athlete_events.csv",
  col_types = cols(
    ID = col_character(),
    Name = col_character(),
    Sex = col_factor(levels = c("M", "F")),
    Age = col_integer(),
    Height = col_double(),
    Weight = col_double(),
    Team = col_character(),
    NOC = col_character(),
    Games = col_character(),
    Year = col_integer(),
    Season = col_factor(levels = c("Summer", "Winter")),
    City = col_character(),
    Sport = col_character(),
    Event = col_character(),
    Medal = col_factor(levels = c("Gold", "Silver", "Bronze"))
  )
)

# read in the NOC regions data
noc_regions <- read_csv("2021_week_31/data/noc_regions.csv")

# enrich the NOC data with the corresponding continent
noc_regions$continent <- countrycode(
  sourcevar = noc_regions$region,
  origin = "country.name",
  destination = "continent"
)

# manually correct the last missing continent data
noc_regions <- noc_regions %>%
  mutate(
    continent = ifelse(NOC %in% c("FSM", "TUV"), "Oceania", continent),
    continent = ifelse(NOC == "BOL", "Americas", continent),
    continent = ifelse(NOC == "KOS", "Europe", continent),
    continent = ifelse(is.na(continent), "Other", continent),
    # in the athletes_events data the NOC code for Singapore is SGP, not SIN:
    NOC = ifelse(NOC == "SIN", "SGP", NOC)
  )


## ---- Read the Gapminder data
library("readxl")
gapminder <- read_excel("2021_week_31/data/income_per_person_gdppercapita_ppp_inflation_adjusted.xlsx",
  sheet = "income_per_person_gdppercapita_"
)

# add the IOC country codes
gapminder$IOC <- countrycode(
  sourcevar = gapminder$country,
  origin = "country.name",
  destination = "ioc"
)
# derive continent by country code
gapminder$Continent <- countrycode(
  sourcevar = gapminder$country,
  origin = "country.name",
  destination = "continent"
)
# read the population data
pop <- read_csv("2021_week_31/data/gapminder_population_total.csv",
  col_names = TRUE
)


# reduce data: filter for years with Olympic Games
OG_years <- athlete_events %>%
  distinct(Year) %>%
  pull()

gapminder_long <- gapminder %>%
  pivot_longer(-c(country, IOC, Continent),
    names_to = "Year",
    values_to = "GDPpc",
    names_transform = list(Year = as.integer)
  ) %>%
  filter(Year %in% OG_years)

pop_long <- pop %>%
  pivot_longer(-country,
    names_to = "Year",
    values_to = "population",
    names_transform = list(Year = as.integer)
  ) %>%
  filter(Year %in% OG_years)


## ---- Combine the datasets
athlete_counts <- athlete_events %>%
  distinct(NOC, Year, ID, .keep_all = TRUE) %>%
  group_by(NOC, Year) %>%
  summarise(ath_count = n())

gap_medal_counts <- athlete_events %>%
  filter(!is.na(Medal)) %>%
  group_by(NOC, Year) %>%
  summarise(med_count = n())

gap_med_ath <- gapminder_long %>%
  inner_join(athlete_counts,
    by = c("IOC" = "NOC", "Year" = "Year"),
    suffix = c("_gap", "_ath")
  ) %>%
  inner_join(gap_medal_counts,
    by = c("IOC" = "NOC", "Year" = "Year"),
    suffix = c("_gap", "_ath")
  ) %>%
  inner_join(pop_long,
    by = c("Year", "country")
  ) %>%
  mutate(ath_frac = ath_count / population) %>%
  rename(Medals = med_count)


# ---- Plot the data

# filter for the 2016 Rio Olympics for plotting
gap_med_ath_2016 <- gap_med_ath %>%
  filter(Year == 2016)


# Neither medal count nor athlete count are normally distributed.
# Hence, Spearman's rank correlation is used:
cor.test(gap_med_ath_2016$ath_count, gap_med_ath_2016$Medals, method = "spearman")

# read the 2016 logo
rio <- readPNG("2021_week_31/Rio2016.png", T)


## Plot 1 ----
gap_med_ath_2016 %>%
  ggplot(aes(ath_count, Medals, color = Continent, group = Continent)) +
  geom_point(aes(size = GDPpc), alpha = 0.4) +
  geom_text_repel(
    aes(label = IOC),
    size = 3.5,
    max.time = 3,
    max.iter = 1000000,
    max.overlaps = 25,
    key_glyph = "blank"
  ) +
  annotate(
    geom = "text",
    label = "Spearman's ρ = 0.820\np < 2.2e-16",
    x = 23, y = 23,
    size = 5, hjust = 1,
    colour = "black"
  ) +
  scale_color_manual("Continent",
    values = c(jolly_yellow, jolly_green, jolly_red, jolly_blue, jolly_petrol)
  ) +
  scale_x_log10(breaks = c(10, 50, 100, 500), minor_breaks = NULL) +
  scale_y_log10(breaks = c(10, 50, 100, 300), minor_breaks = NULL) +
  scale_size(range = c(1, 18), name = "GDP p. c. ($, fixed 2011 prices)") +
  labs(
    title = "More athletes win more medals",
    subtitle = "Countries sending more athletes to the 2016 Games won more\nmedals. The size of the bubbles represents the GDP per capita,\ninspired by the famous Gapminder graph.",
    x = "Number of participating Athletes",
    y = "Total number of Medals won",
    caption = "@c_gebhard | Data sources:\nFree data from World Bank via gapminder.org, CC-BY license\n'120 years of Olympic history: athletes and results' by rgriffin via kaggle.com, CC0-license\nLogo: National Olympic Committee, Public domain, via Wikimedia Commons"
  ) +
  jolly_theme() +
  theme(
    legend.direction = "horizontal", legend.box = "vertical",
    legend.text = element_text(size = 12)
  ) +
  guides(color = guide_legend(override.aes = list(size = 6))) +
  patchwork::inset_element(rio, 0.8, 0.9, 1, 1,
    clip = F, align_to = "full"
  )

ggsave("2021_week_31/MoreAthletesMoreMedals.png", dpi = 96, height = 10, width = 8)


## Plot 2 ----

cor.test(gap_med_ath_2016$ath_frac, gap_med_ath_2016$GDPpc, method = "spearman")


gap_med_ath %>%
  filter(Year == 2016) %>%
  ggplot(aes(GDPpc, ath_frac, color = Continent, group = Continent)) +
  geom_point(aes(size = Medals), alpha = 0.4) +
  geom_text_repel(
    aes(label = IOC),
    size = 3.5,
    max.time = 3,
    max.iter = 1000000,
    max.overlaps = 25,
    key_glyph = "blank"
  ) +
  annotate(
    geom = "text",
    label = "Spearman's ρ = 0.504\np = 0.0000014",
    x = 5000, y = 0.00001,
    size = 5, hjust = 1,
    colour = "black"
  ) +
  scale_color_manual("Continent",
    values = c(jolly_yellow, jolly_green, jolly_red, jolly_blue, dark_slate)
  ) +
  scale_x_continuous(breaks = c(1000, 5000, 10000, 50000), minor_breaks = NULL) +
  scale_y_continuous(breaks = c(0.000001, 0.00001, 0.0001, 0.001), minor_breaks = NULL) +
  scale_size(range = c(1, 18), name = "Medal count") +
  coord_trans(x = "log10", y = "log10") +
  labs(
    title = "Wealty countries send more athletes",
    subtitle = "Countries with a higher GDP p.c. sent more athletes 'p. c.' to the\n2016 Games. The size of the bubbles represents the number of\nmedals, those atheletes won this year.",
    x = "GDP p. c. ($, fixed 2011 prices)",
    y = "'Athletes p. c.'\n(athletes divided by the countries' popualations",
    caption = "@c_gebhard | Data sources:\nFree data from World Bank via gapminder.org, CC-BY license\n'120 years of Olympic history: athletes and results' by rgriffin via kaggle.com, CC0-license\nLogo: National Olympic Committee, Public domain, via Wikimedia Commons"
  ) +
  jolly_theme() +
  theme(
    legend.direction = "horizontal", legend.box = "vertical",
    legend.text = element_text(size = 12)
  ) +
  guides(color = guide_legend(override.aes = list(size = 6))) +
  patchwork::inset_element(rio, 0.8, 0.9, 1, 1,
    clip = F, align_to = "full"
  )

ggsave("2021_week_31/MoreGDPMoreAthletes.png", dpi = 96, height = 10, width = 8)
