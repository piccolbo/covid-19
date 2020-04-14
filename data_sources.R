library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

jhu_types = c(cases = "confirmed", deaths = "deaths")

jhu = bind_rows(lapply(jhu_types , function(type)
  read_csv(
    paste0(
      "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_",
      type ,
      "_global.csv"
    ),
    guess_max = 1e6
  )),
  .id = "type") %>%
  pivot_longer(cols = ends_with("/20"),
               names_to = "date",
               values_to = "value") %>%
  mutate(date = mdy(paste0(date, "20"))) %>%
  rename(state = `Province/State`, country = `Country/Region`) %>%
  mutate(country = ifelse(country == "US", "United States", country))

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

# pop =
#   read_csv("ts.csv.zip", guess_max = 1e6) %>%
#   filter(!grepl(pattern = ",", x = county)) %>% #kill funky last day and combo counties
#   tidyr::separate(col = "county", into = "county", sep = " County") %>% # remove useless County from County names
#   tidyr::separate(col = "county", into = "county", sep = " Parish") %>%
#   group_by(city, county, state, country) %>%
#   summarise(population = naggregate(population))
# save(pop, file = "pop.rdata")
load("pop.rdata")



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

jhu = left_join(jhu, pop)
nyt_us_counties = left_join(nyt_us_counties, pop)
nyt_us_states = left_join(left_join(nyt_us_states, pop),
                          data.frame(state = names(lockdown), lockdate = ymd(lockdown)))


tf = tempfile(fileext = ".csv.zip")
download.file(url = "https://coronadatascraper.com/timeseries-tidy.csv.zip", destfile = tf)
cds = read_csv(file = tf, guess_max = 1e6) %>%
  filter(!grepl(pattern = ",", x = county)) %>% #kill funky combo counties
  tidyr::separate(col = "county", into = "county", sep = " County") %>% # remove useless County from County names
  tidyr::separate(col = "county", into = "county", sep = " Parish") # same with Parish
unlink(tf)

mwna = function(x, y=x){y[min(1,min(which(!is.na(x))))] }
corona =
  bind_rows(jhu = jhu %>% filter(country != "United States" |
                             is.na(state)),
            nyt_counties = nyt_us_counties,
            nyt_states = nyt_us_states,
            cds = cds,
            .id = "source") %>%
  tidyr::separate(col = "county", into = "county", sep = " Parish") %>%
  group_by(city, county, state, country, date, type) %>%
  summarize(value = median(value), population = median(population), lockdate = median(lockdate)) %>%
  ungroup
  # summarize(source = mwna(source, value), value = mwna(value), population = mwna(population), lockdate = mwna(lockdate)) %>% View

save(corona, file = "data-sources.Rdata")

