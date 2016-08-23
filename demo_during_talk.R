library(dplyr)
library(tidyr)
library(nycflights13)
library(ggplot2)

glimpse(flights)

#Subsetting
flights$carrier
flights[,5]
flight_small <- select(flights, carrier, flight, origin)
flight_small

select(flights, matches(".time"))
select(flights, contains("time"))

flights[, -5]
select(flights, -contains("time"))

flights_special <- filter(flights, dest == "DEN" &
                            between(dep_time, 1000, 1400) &
                            month %in% c(6, 7, 8))

flights_special

#in base R
dest_is_DEN = flights$dest == "DEN"
flights[dest_is_DEN,]

#Sampling
sample_n(flights, 5, replace = TRUE)
sample_frac(flights, 0.1)


# Mutating ------------------------------------------------------------

flights_km <- mutate(flights, distance = distance * 1.6) #convert to km
flights_cf <- mutate(flights, carrier_flight = paste(carrier, flight))
flights_max <- mutate(flights, max_delay = max(c(dep_delay, arr_delay),
                                               na.rm = T)/60)

flights_lower <- mutate_if(flights, is.character, tolower)



# Grouping ----------------------------------------------------------------
flights_by_month = group_by(flights, month)
flights_max <- mutate(flights, max_day = max(day))
flights_max2 <- mutate(flights_by_month, max_day = max(day))


# Summarising -------------------------------------------------------------



flights_by_carrier  <- group_by(flights, carrier)
summarise(flights_by_carrier, Delay_Avg = mean(arr_delay, na.rm = T))

flights_by_date <- group_by(flights, month, day)
summarise(flights_by_date, Delay_Avg = mean(arr_delay, na.rm = T),
          Distance_Avg = mean(distance),
          Distance_SD = sd(distance),
          Most_Common_Carrier = names(sort(table(carrier)))[1])


# Combining ---------------------------------------------------------------

airlines
flights_with_airlines <- left_join(flights, airlines, by = "carrier")
select(flights_with_airlines, carrier, name)


# Piping ------------------------------------------------------------------

flights_sum <- flights %>%
  left_join(airlines, by = "carrier") %>%
  filter(carrier %in% c("WN", "UA", "VX", "DL")) %>%
  group_by(name) %>%
  summarise(Avg_Distance = mean(distance, na.rm = T), 
            Avg_Delay = mean(arr_delay, na.rm = T))

flights_long_delay <- flights %>%
  group_by(carrier, month) %>%
  arrange(arr_delay) %>%
  top_n(5) %>%
  summarise(Avg_Delay = mean(arr_delay, na.rm = T))