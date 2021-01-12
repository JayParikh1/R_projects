library(tidyverse)
forest_fires <- read_csv("forestfires.csv")
dim(forest_fires)
month_order <- c("jan", "feb", "mar", "apr", "may", "jun",
                 "jul", "aug", "sep", "oct", "nov", "dec")
day_order <- c("sun", "mon", "tue", "wed", "thu", "fri", "sat")
forest_fires <- forest_fires %>% 
  mutate(
    month = factor(month, levels = month_order),
    day = factor(day, levels = day_order)
  )

forest_fires_month <- forest_fires %>% group_by(month) %>% summarise(total_fires = n())
forest_fires_month

forest_fires_month %>% ggplot(aes(x = month, y = total_fires)) + geom_col() + labs(title = "# of forest fires by month", y = "fire count", x = "month")

forest_fires_day <- forest_fires %>% group_by(day) %>% summarise(total_fires = n())
forest_fires_day

forest_fires_day %>% ggplot(aes(x = day, y = total_fires)) + geom_col() + labs(title = "# of forest fires by day", y = "fire count", x = "day")
# aug and sep is when the most fires occur by month, while weekend days is when the most fires occur by day
forest_fires_long <- forest_fires %>% 
  pivot_longer(
    cols = c("FFMC", "DMC", "DC", 
             "ISI", "temp", "RH", 
             "wind", "rain"),
    names_to = "data_col",
    values_to = "value"
  )
forest_fires_long %>% 
  ggplot(aes(x = month, y = value)) +
  geom_boxplot() +
  facet_wrap(vars(data_col), scale = "free_y") +
  labs(
    title = "Variable changes over month",
    x = "Month",
    y = "Variable value"
  )
# seems like there is higher DC, DMC, ISI, and temp in aug and sep

forest_fires_area <- forest_fires_long %>% ggplot(aes(x = value, y = area)) + geom_point() + facet_wrap(vars(data_col), scale = "free_x") +
  labs(
    title = "Relationships between other variables and area burned",
    x = "Month",
    y = "Area burned (hectare)"
  )

forest_fires_area
