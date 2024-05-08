# Libraries and files----

library(tidyverse)
library(ggplot2)

maristova <-  read.csv2("Weather/maristova.csv") |> 
  mutate(date = as.Date(date, "%d.%m.%Y"), 
         year = year(date), 
         month = month(date)) |> 
  mutate(precipitation = as.numeric(sub(",", ".", precipitation, fixed = TRUE))) |> 
  mutate(snow_depth = as.numeric(sub(",", ".", snow_depth, fixed = TRUE)))

aabjoersbraaten <-  read.csv("Weather/aabjoersbraaten.csv", sep = ";") |> 
  mutate(date = as.Date(date, "%d.%m.%Y"), 
         year = year(date), 
         month = month(date)) |> 
  mutate(temperature_average = as.numeric(sub(",", ".", temperature_average, fixed = TRUE))) |> 
mutate(precipitation = as.numeric(sub(",", ".", precipitation, fixed = TRUE))) |> 
  mutate(snow_depth = as.numeric(sub(",", ".", snow_depth, fixed = TRUE)))

filefjell <-  read.csv("Weather/filefjell.csv", sep = ";") |> 
  mutate(date = as.Date(date, "%d.%m.%Y"), 
         year = year(date), 
         month = month(date)) |> 
  filter(date > "2010-01-01") |> 
  mutate(temperature_average = as.numeric(sub(",", ".", temperature_average, fixed = TRUE))) |> 
  mutate(precipitation = as.numeric(sub(",", ".", precipitation, fixed = TRUE))) |> 
  mutate(snow_depth = as.numeric(sub(",", ".", snow_depth, fixed = TRUE)))


# Some plots----

maristova |> 
  ggplot(aes(x = date, y = precipitation)) + 
  geom_line() + 
  theme_bw()
maristova |> 
  ggplot(aes(x = date, y = snow_depth)) + 
  geom_line() + 
  theme_bw()

aabjoersbraaten |> 
  ggplot(aes(x = date, y = temperature_average)) + 
  geom_line() + 
  theme_bw()
aabjoersbraaten |> 
  ggplot(aes(x = date, y = precipitation)) + 
  geom_line() + 
  theme_bw()
aabjoersbraaten |> 
  ggplot(aes(x = date, y = snow_depth)) + 
  geom_line() + 
  theme_bw()

filefjell |> 
  ggplot(aes(x = date, y = temperature_average)) + 
  geom_line() + 
  theme_bw()
filefjell |> 
  ggplot(aes(x = date, y = precipitation)) + 
  geom_line() + 
  theme_bw()
filefjell |> 
  ggplot(aes(x = date, y = snow_depth)) + 
  geom_line() + 
  theme_bw()


# Precipitation----

maristova_year <- maristova |> 
  group_by(year) |> 
  summarise(precipitation_yearly = sum(precipitation, na.rm = TRUE))
maristova_year |> 
  ggplot(aes(x = year, y = precipitation_yearly)) + 
  geom_line() + 
  theme_bw()

maristova_month<- maristova |> 
  group_by(year, month) |> 
  summarise(precipitation_month = sum(precipitation, na.rm = TRUE)) |> 
  group_by(month) |> 
  summarise(precipitation_monthly = mean(precipitation_month, na.rm = TRUE))
maristova_month |> 
  ggplot(aes(x = month, y = precipitation_monthly)) + 
  geom_line() + 
  ylim(0, 100) + 
  theme_bw()



filefjell_p_year <- filefjell |> 
  group_by(year) |> 
  summarise(precipitation_yearly = sum(precipitation, na.rm = TRUE))
filefjell_p_year |> 
  ggplot(aes(x = year, y = precipitation_yearly)) + 
  geom_line() + 
  theme_bw()

filefjell_p_month <-  filefjell |> 
  group_by(year, month) |> 
  summarise(precipitation_month = sum(precipitation, na.rm = TRUE)) |> 
  group_by(month) |> 
  summarise(precipitation_monthly = mean(precipitation_month, na.rm = TRUE))
filefjell_p_month |> 
  ggplot(aes(x = month, y = precipitation_monthly)) + 
  geom_line() + 
  ylim(0, 100) +
  theme_bw()



# Temperature----

aabjoersbraaten_t_year <- aabjoersbraaten |> 
  group_by(year) |> 
  summarise(temperature_yearly = mean(temperature_average, na.rm = TRUE))
aabjoersbraaten_t_year |> 
  ggplot(aes(x = year, y = temperature_yearly)) + 
  geom_line() + 
  theme_bw()

aabjoersbraaten_t_month <-  aabjoersbraaten |> 
  group_by(year, month) |> 
  summarise(temperature_month = mean(temperature_average, na.rm = TRUE)) |> 
  group_by(month) |> 
  summarise(temperature_monthly = mean(temperature_month, na.rm = TRUE))
aabjoersbraaten_t_month |> 
  ggplot(aes(x = month, y = temperature_monthly)) + 
  geom_line() + 
  theme_bw()



filefjell_t_year <- filefjell |> 
  group_by(year) |> 
  summarise(temperature_yearly = mean(temperature_average, na.rm = TRUE))
filefjell_t_year |> 
  ggplot(aes(x = year, y = temperature_yearly)) + 
  geom_line() + 
  theme_bw()

filefjell_t_month <-  filefjell |> 
  group_by(year, month) |> 
  summarise(temperature_month = mean(temperature_average, na.rm = TRUE)) |> 
  group_by(month) |> 
  summarise(temperature_monthly = mean(temperature_month, na.rm = TRUE))
filefjell_t_month |> 
  ggplot(aes(x = month, y = temperature_monthly)) + 
  geom_line() + 
  theme_bw()
