library(tidyverse)

lung <- tibble(
  year   = floor(time(ldeaths)),
  month  = cycle(ldeaths),
  male   = as.numeric(mdeaths),
  female = as.numeric(fdeaths)
)

# Gjør tabellen lang: kjønn blir en kolonne
lung_long <- lung |>
  pivot_longer(cols = c(male, female),
               names_to = "sex",
               values_to = "deaths") |>
  mutate(sex = recode(sex, male = "Menn", female = "Kvinner"))

# Totalt antall dødsfall per år og kjønn
yearly <- lung_long |>
  group_by(year, sex) |>
  summarise(total = sum(deaths), .groups = "drop")

# Plott
ggplot(yearly, aes(x = factor(year), y = total, fill = sex)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Menn" = "#2C7BB6", "Kvinner" = "#D7191C")) +
  labs(
    title = "Dør flere menn enn kvinner av lungesykdommer i Storbritannia?",
    subtitle = "Dødsfall av bronkitt, emfysem og astma per år, 1974-1979",
    x = "År",
    y = "Antall dødsfall",
    fill = "Kjønn",
    caption = "Kilde: datasettet UKLungDeaths (R, pakken datasets)"
  ) +
  theme_minimal(base_size = 14)

ggsave("lung_deaths_by_sex.png", width = 8, height = 5, dpi = 300)


