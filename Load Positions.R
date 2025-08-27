library(ggplot2)
library(rearrr)
m <- matrix(c(4.88,  8.43,
  9.14,  1.54,
  8.53, -4.95,
  2.17, -9.58,
  -4.92, -8.45,
  -9.17, -1.51,
  -8.50,  4.97,
  -2.14,  9.59),byrow = T,ncol = 2) |>
  as.data.frame() |>
  rename(x = V1, y = V2) |>
  mutate(square = c("sq1","sq2","sq3","sq4","sq5","sq6","sq7","sq8"))

x <- angle(m,x_col = "x",y_col = "y",origin = c(0,0)) |>
  arrange(.degrees) |>
  mutate(diff_degree = .degrees - lag(.degrees))


x %>%
  ggplot(aes(x = x, y = y, color = .degrees)) +
  geom_segment(aes(x = 0, xend = 10, y = 0, yend = 10), color = "magenta") +
  geom_point() +
  theme_minimal()


plot(m$x,m$y)  


og_combos_df_map <- og_combos_df |>
  pivot_longer(cols = everything(),
               names_to = "square",
               values_to = "value") |>
  mutate(combo_n = rep(1:40, each = 8))

positions <- left_join(og_combos_df_map,m)

positions[positions$value == 0,]$x <- NA 
positions[positions$value == 0,]$y <- NA 




ggplot(positions, aes(x = x, y = y))+
  geom_point() +
  geom_point(aes(x = 0, y = 0, color = "red"))+
  facet_wrap(~combo_n)
  
