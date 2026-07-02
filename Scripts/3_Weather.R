# Data----

library(tidyverse)
library(janitor)

temperature <- read_csv2("data_raw/kyrkestolene_temp.csv")
water <- read_csv2("data_raw/kyrkestolene_water.csv")
# weather <- read_csv2("data_raw/kyrkjestolane_weather.csv",
                     # na = c("", "NA", "-"))
# maristova <- read_csv2("data_raw/maristova_precip.csv")

temp_water <- temperature |>
  full_join(water, by = c("name", "station", "date"))

temp_water_tidy <- temp_water |>
  mutate(date = dmy(date),
         year = year(date),
         month = month(date),
         mean_temp = ifelse(mean_temp == "-", NA_real_, mean_temp),
         mean_temp = parse_double(mean_temp, locale = locale(decimal_mark = ",")),
         max_temp = ifelse(max_temp == "-", NA_real_, max_temp),
         max_temp = parse_double(max_temp, locale = locale(decimal_mark = ",")),
         min_temp = ifelse(min_temp == "-", NA_real_, min_temp),
         min_temp = parse_double(min_temp, locale = locale(decimal_mark = ",")),
         mean_mois = ifelse(mean_mois == "-", NA_real_, mean_mois),
         mean_mois = as.numeric(mean_mois),
         min_mois = ifelse(min_mois == "-", NA_real_, min_mois),
         min_mois = as.numeric(min_mois),
         precip = ifelse(precip == "-", NA_real_, precip),
         precip = parse_double(precip, locale = locale(decimal_mark = ",")),
         snow = ifelse(snow == "-", NA_real_, snow),
         snow = as.numeric(snow),
         snow_change = snow - lag(snow)) |>
  relocate(c(year, month), .after = station)

# weather_tidy <- weather |>
#   clean_names() |> # This gives a warning. But we are renaming the columns afterwards, so it's benign
#   rename(name = navn, station = stasjon, date = tid_norsk_normaltid, mean_temp = middeltemperatur_dogn, precip = nedbor_dogn) |>
#   mutate(date = dmy(date),
#          year = year(date),
#          month = month(date)) |> 
#   relocate(c(year, month), .after = station)

# maristova_tidy <- maristova|>
#   clean_names() |>  rename(name = navn, station = stasjon, date = tid_norsk_normaltid, precip = nedbor_dogn) |>
#   mutate(date = dmy(date),
#          year = year(date),
#          month = month(date)) |> 
#   relocate(c(year, month), .after = station)



# Temperature----

temp_water_tidy |>
  filter(!is.na(mean_temp)) |>
  ggplot() +
  geom_point(aes(x = date, y = mean_temp))

temp_water_tidy |>
  filter(year %in% 2011:2024) |>
  summarise(.by = year, annual_temp = mean(mean_temp, na.rm = TRUE)) |>
  ggplot() +
  geom_point(aes(x = year, y = annual_temp))


# For average we use 2011-2024: years with full data and before the study

temp_water_tidy |> 
  filter(year %in% 2011:2024 & !is.na(mean_temp)) |> 
  summarise(.by = year, annual_temp = mean(mean_temp))
# Ranging from -0.7 to 1.6

temp_water_tidy |> 
  filter(year %in% 2011:2024 & !is.na(mean_temp)) |> 
  summarise(.by = year, annual_temp = mean(mean_temp)) |> 
  summarise(mean_annual_temp = mean(annual_temp))
# 0.36


### Possible extreme events

temp_water_tidy |> 
  filter(!is.na(mean_temp)) |> 
  filter(!(month %in% c(4:11))) |> 
  summarise(.by = year, winter_hot = sum(mean_temp > 0))

temp_water_tidy |>
  filter(!is.na(max_temp)) |>
  ggplot() +
  geom_point(aes(x = date, y = max_temp))

temp_water_tidy |>
  filter(!is.na(min_temp)) |>
  ggplot() +
  geom_point(aes(x = date, y = min_temp))

temp_water_tidy |> 
  filter(year %in% 2011:2024, !is.na(mean_temp)) |> 
  summarise(.by = year, temp_15 = sum(mean_temp > 15)) |>
  ggplot() +
  geom_point(aes(x = year, y = temp_15))

temp_water_tidy |> 
  filter(year %in% 2011:2024, !is.na(max_temp)) |> 
  summarise(.by = year, hightemp_20 = sum(max_temp > 20)) |>
  ggplot() +
  geom_point(aes(x = year, y = hightemp_20))
# 2018: 10 days average above 15 degrees, 36 maximum temperatures above 20




# Precipitation----

temp_water_tidy |> 
  filter(year %in% 2011:2024 & !is.na(precip)) |> 
  summarise(.by = year, annual_precip = sum(precip)) |>
  ggplot() +
  geom_point(aes(x = year, y = annual_precip))
# Ranging from 436 to 877

temp_water_tidy |> 
  filter(year %in% 2011:2024 & !is.na(precip)) |> 
  summarise(.by = year, annual_precip = sum(precip)) |> 
  summarise(mean_annual_precip = mean(annual_precip))
# 664

# maristova_tidy |> 
#   filter(year %in% 2011:2024 & !is.na(precip)) |> 
#   summarise(.by = year, annual_precip = sum(precip))
# # Ranging from 673 to 1178
# 
# maristova_tidy |> 
#   filter(year %in% 2011:2024 & !is.na(precip)) |> 
#   summarise(.by = year, annual_precip = sum(precip)) |> 
#   summarise(mean_annual_precip = mean(annual_precip))
# # 850


### Testing----

test_weather <- readxl::read_excel(path = "data_raw/table.xlsx") |> clean_names()
test_weather |> 
  mutate(date = dmy(tid_norsk_normaltid),
         year = year(date)) |> 
  summarise(.by = c(stasjon, year), mean = mean(middeltemperatur_dogn)) |>
  print(n = Inf)

test_weather_annual <- test_weather |> 
  mutate(date = dmy(tid_norsk_normaltid),
         year = year(date)) |> 
  filter(stasjon %in% c("SN54110", "SN54120", "SN54130")) |> 
  filter(!(stasjon == "SN54110" & date < "2008-10-01")) |> 
  filter(year != 2026) |> 
  summarise(.by = year, mean_t = mean(middeltemperatur_dogn, na.rm = TRUE)) 

test_weather_annual |> 
  ggplot() + 
  geom_point(aes(x = year, y = mean_t))

glmmTMB(mean_t ~ year,
        data = test_weather_annual) |> summary()




# PrecipitatInf# Precipitation----

weather_tidy |>
  ggplot() +
  geom_point(aes(x = date, y = precip))

weather_tidy |> 
  filter(!is.na(mean_mois)) |> 
  summarise(.by = year, lowmois = sum(mean_mois < 60))
weather_tidy |> 
  filter(!is.na(min_mois)) |> 
  summarise(.by = year, lowmois = sum(min_mois < 30))
# 2018: 15 days average below 60%, 21 days below 30



# Moisture----

weather_tidy |>
  ggplot() +
  geom_point(aes(x = date, y = mean_mois))

weather_tidy |>
  ggplot() +
  geom_point(aes(x = date, y = min_mois))

weather_tidy |> 
  filter(!is.na(mean_mois)) |> 
  summarise(.by = year, lowmois = sum(mean_mois < 60))
weather_tidy |> 
  filter(!is.na(min_mois)) |> 
  summarise(.by = year, lowmois = sum(min_mois < 30))
# 2018: 15 days average below 60%, 21 days below 30

weather_tidy |> 
  filter(year == 2018 & min_mois < 30)

weather_tidy |> 
  filter(year == 2018, month > 4, month < 9) |> 
  ggplot() +
  geom_line(aes(x = date, y = max_temp), colour = "red", linewidth = 1) +
  geom_line(aes(x = date, y = min_mois / 5), colour = "blue", linewidth = 1) +
  scale_y_continuous(name = "max_temp",
                     sec.axis = sec_axis(~.*5, name = "min_mois"))

# Snow----

weather_tidy

weather_tidy |>
  ggplot() +
  geom_point(aes(x = date, y = snow))

weather_tidy |> 
  filter(!is.na(snow)) |> 
  filter(!(month %in% c(4:11))) |> 
  summarise(.by = year, winter_hot = sum(snow < 10))

weather_tidy |> 
  filter(!is.na(snow)) |> 
  filter(year == 2022, !(month %in% c(4:11)), snow < 10) |> 
  ggplot() +
  geom_point(aes(x = date, y = snow))


## Rain on snow----

rain_on_snow <- weather_tidy |> 
  filter(!(month %in% c(4:11)), mean_temp > 1, precip > 10)

weather_tidy |> 
  filter(!is.na(snow_change)) |> 
  summarise(.by = year, melting = sum(snow_change < -5))

weather_tidy |> 
  filter(snow > 30, mean_temp > 1, precip > 10)
