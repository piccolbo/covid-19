library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

nyt_us_states =
  read_csv(
    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv",
    guess_max = 1e6
  ) %>%
  mutate(city = NA_character_,
         county = NA_character_,
         country = "United States") %>%
  select (-fips) %>%
  pivot_longer(
    cols = c("cases", "deaths"),
    names_to = "type",
    values_to = "value"
  )

nyt_us_counties =
  read_csv(
    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
    guess_max = 1e6
  ) %>%
  mutate(city = NA_character_, country = "United States") %>%
  select (-fips) %>%
  pivot_longer(
    cols = c("cases", "deaths"),
    names_to = "type",
    values_to = "value"
  )


# return a unique item dropping NAs or NA if it's the only one
# naggregate = function(x) {
#   ux = unique(discard(x, is.na))
#   if (length(ux) == 0)
#     NA
#   else
#     ux
# }
#

lockdown =
  c(
    Alabama = "2020-04-04",
    Alaska = "2020-03-28",
    Arizona = "2020-03-31",
    California = "2020-03-19",
    Colorado = "2020-03-26",
    Connecticut = "2020-03-23",
    Delaware = "2020-03-24",
    Florida = "2020-04-01",
    Georgia = "2020-04-03",
    Hawaii = "2020-03-25",
    Illinois = "2020-03-21",
    Indiana = "2020-03-25",
    Kansas = "2020-03-19",
    Kentucky = "2020-03-26",
    Louisiana = "2020-03-23",
    Maine = "2020-04-01",
    Maryland = "2020-03-30",
    Massachusetts = "2020-03-24",
    Michigan = "2020-03-24",
    Minnesota = "2020-03-27",
    Mississippi = "2020-04-05",
    Missouri = "2020-04-06",
    Montana = "2020-03-10",
    Nevada = "2020-03-21",
    `New Hampshire` = "2020-03-27",
    `New Jersey` = "2020-03-21",
    `New Mexico` = "2020-03-24",
    `New York` = "2020-03-22",
    `North Carolina` = "2020-03-30",
    Ohio = "2020-03-23",
    Oregon = "2020-03-23",
    Pennsylvania = "2020-04-01",
    Tennessee = "2020-03-31",
    Texas = "2020-04-02",
    Vermont = "2020-03-25",
    Washington = "2020-03-23",
    `West Virgina` = "2020-03-24",
    Wisconsin = "2020-03-24"
  )
