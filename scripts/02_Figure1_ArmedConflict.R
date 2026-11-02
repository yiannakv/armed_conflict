library(ggplot2)

#to generate the linegraph figure, we will first source the original data we had
df_figure <- readRDS('df_clean.rds')

df_figure$iso <- as.factor(df_figure$iso)
#we will first create a new column calculating the difference in maternal mortality for 
#those that increase! 

df_diff <- df_figure |>
  dplyr::select(iso, year, maternal_mortality) |>
  dplyr::filter(year < 2018) |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(diff_matmor = maternal_mortality - maternal_mortality[1L])

#now select those that have a positive diff_matmor > 0 
countries_keep <- df_diff |>
  dplyr::filter(year == 2017, diff_matmor > 0) |>
  pull(iso)


#we are looking to see if the overall change is greaterthan 0 
df_diff_select <- df_diff |>
  filter(iso %in% countries_keep)


#create a ggplot line graph, group_by ISO so that each colour is assigned 
figure1 <- ggplot(df_diff_select, aes(x = year, y = maternal_mortality,
                                      colour = iso, group = iso)) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  scale_x_continuous(breaks = seq(2000, 2017, by = 2), expand = c(0.01, 0.01)) +
  scale_y_log10() +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  labs(
    title = "Maternal Mortality Trends in Countries with an Overall Increase",
    subtitle = "Countries with higher maternal mortality in 2017 than in 2000",
    x = "Year",
    y = "Maternal mortality (log10 scale)",
    colour = "Country") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(colour = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )
   
print(figure1)
