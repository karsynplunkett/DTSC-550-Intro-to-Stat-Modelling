library(tidyverse)
library(openintro)
data(nycflights)
names(nycflights)
?nycflights
glimpse(nycflights)
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram()
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 15)
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 150)
lax_flights <- nycflights %>%
  filter(dest == "LAX")
ggplot(data = lax_flights, aes(x = dep_delay)) +
  geom_histogram()
lax_flights %>%
  summarise(mean_dd   = mean(dep_delay), 
            median_dd = median(dep_delay), 
            n         = n())
sfo_feb_flights <- nycflights %>%
  filter(dest == "SFO", month == 2)
sfo_feb_flights %>%
  group_by(origin) %>%
  summarise(median_dd = median(dep_delay), iqr_dd = IQR(dep_delay), n_flights = n())
ggplot(data =sfo_feb_flights , aes(x = arr_delay)) +
  geom_histogram()
nycflights %>%
  group_by(month) %>%
  summarise(mean_dd = mean(dep_delay)) %>%
  arrange(desc(mean_dd))
sfo_feb_flights %>%
  group_by(origin) %>%
  summarise(median_dd = median(dep_delay), iqr_dd = IQR(dep_delay), n_flights = n())
sfo_feb_flights %>%
  group_by(carrier) %>%
  summarise(median_dd = median(dep_delay), iqr_dd = IQR(arr_delay), n_flights = n())
nycflights %>%
  group_by(month) %>%
  summarise(mean_dd = mean(dep_delay)) %>%
  arrange(desc(mean_dd))
nycflights <- nycflights %>%
  mutate(dep_type = ifelse(dep_delay < 5, "on time", "delayed"))
nycflights %>%
  group_by(origin) %>%
  summarise(ot_dep_rate = sum(dep_type == "on time") / n()) %>%
  arrange(desc(ot_dep_rate))
ggplot(data = nycflights, aes(x = origin, fill = dep_type)) +
  geom_bar()
nycflights <- nycflights %>%
  mutate(avg_speed = distance/(air_time/60))
ggplot(data = nycflights, aes(x = avg_speed, y=distance)) +
  geom_point()
dept_flights <- nycflights %>%
  filter(carrier == "AL"|carrier == "DL"|carrier == "UA")
ggplot(data = dept_flights, aes(x = dep_delay, y=arr_delay)) +
  geom_point()
