library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

#data read and one-time processing

df2named_vector = function(keys, values, data = NULL) {
  if (!is.null(data)) {
    keys = data[[keys]]
    values = data[[values]]
  }
  v = values
  names(v) = keys
  v
}

three_letter2country = function(x) {
  map = df2named_vector("Alpha_3", "Name", ISOcodes::ISO_3166_1)
  unlist(ifelse(is.na(map[x]), x, map[x]))
}

expand_country_names = function(data) {
  mutate(data, country = three_letter2country(country))
}

monotonize = function(x) {
  c(x[1], pmax(head(x, -1), tail(x, -1)))
}


tf = tempfile(fileext = ".csv.zip")
download.file(url = "https://coronadatascraper.com/timeseries-tidy.csv.zip", destfile = tf)


atlas = read_csv(
    file = tf,
    col_types = cols(
    city = col_character(),
    county = col_character(),
    state = col_character(),
    country = col_character(),
    population = col_double(),
    lat = col_double(),
    long = col_double(),
    aggregate = col_character(),
    tz = col_character(),
    date = col_date(format = ""),
    type = col_character(),
    value = col_double()
  )
) %>%
  dplyr::select(city, county, state, country, population, date, type, value) %>% #just what we need
  expand_country_names %>% # readable country names
  filter (date != max(date) &
            !grepl(pattern = ",", x = county)) %>% #kill funky last day and combo counties
  tidyr::separate(col = "county", into = "county", sep = " County") %>% # remove useless County from County names
  tidyr::separate(col = "county", into = "county", sep = " Parish") %>%
  group_by(city, county, state, country, type) %>% arrange(date) %>%
  mutate(value = monotonize(value)) %>% ungroup #correct declining cumulative counts

unlink(tf)

# return a unique item dropping NAs or NA if it's the only one
naggregate = function(x) {
  ux = unique(discard(x, is.na))
  if (length(ux) == 0)
    NA
  else
    ux
}

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


pop =
  atlas %>%
  filter(country == "United States") %>%
  group_by(city, county, state, country) %>%
  summarise(population = naggregate(population))


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

nyt_us_counties = left_join(nyt_us_counties, pop)
nyt_us_states = left_join(left_join(nyt_us_states, pop),
                          data.frame(state = names(lockdown), lockdate = ymd(lockdown)))



corona =
  bind_rows(atlas %>% filter(country != "United States" |
                               is.na(state)),
            nyt_us_counties,
            nyt_us_states)




# filtering


is_city = function(x) {
  !is.na(x$city)
}

is_county = function(x) {
  !is_city(x) & !is.na(x$county)
}

is_state = function(x) {
  !is_city(x) & !is_county(x) & !is.na(x$state)
}

is_country = function(x) {
  !is_city(x) & !is_county(x) & !is_state(x) & !is.na(x$country)
}

has_subregions = list(
  country =  corona[is_state(corona),]$country %>% unique,
  state = discard(corona[is_county(corona),]$state %>% unique, is.na),
  county = discard(corona[is_city(corona),]$county %>% unique, is.na)
)
all_dates = unique(corona$date)
# all_types = unique(corona$type)
all_types = c("cases", "deaths", "recovered", "tested")



filter_regions = function(data, regions, level) {
  filter_(data,
          paste(level, " %in% regions"))
}


all_levels = c("city", "county", "state", "country")

one_level_up = function(level) {
  all_levels[which(all_levels == level) + 1]
}

one_level_down = function(level) {
  all_levels[which(all_levels == level) - 1]
}

filter_data2 = function(data,
                        type,
                        countries,
                        states,
                        counties,
                        cities,
                        date_range) {
  bind_rows(
    filter_data(
      data,
      level = "country",
      type = type,
      top_region = NULL,
      regions = countries,
      date_range = date_range
    ),
    filter_data(
      data,
      level = "state",
      type = type,
      top_region = NULL,
      regions = states,
      date_range = date_range
    ),
    filter_data(
      data,
      level = "county",
      type = type,
      top_region = NULL,
      regions = counties,
      date_range = date_range
    ),
    filter_data(
      data,
      level = "city",
      type = type,
      top_region = NULL,
      regions = cities,
      date_range = date_range
    )
  )
}


filter_data = function(data,
                       level = all_levels,
                       type = c(all_types),
                       top_region,
                       regions,
                       date_range) {
  level = match.arg(level)
  top_level = one_level_up(level)
  typ = match.arg(type)
  if (level == "country") {
    top_region = NULL
  }
  else {
    if (!belong_to_level(top_region, data, one_level_up(level))) {
      top_region = NULL
    }
  }
  if (!belong_to_level(regions, data, level)) {
    regions = NULL
  }

  level_filter = list(
    city = is_city,
    county = is_county,
    state = is_state,
    country = is_country
  )[[level]]
  data = filter(data, level_filter(data), type == typ)
  if (!is.null(top_region)) {
    data = filter_(data, paste(top_level,  " == top_region"))
  }
  if (!is.null(regions)) {
    data = filter_regions(data, regions, level)
  }
  if (!is.null(date_range))
    data = filter(data, date >= date_range[1] - 1, #1 extra for diffs
                  date <= date_range[2])
  data
}


#stats

set_region = function(data,
                      level = c("city", "county", "state", "country")) {
  level = match.arg(level)
  data$region = data[[level]]
  data
}

decimal_trunc = function(x) {
  as.numeric(format(x, digits = 2))
}

cdiff = function(x, bottom) {
  #this is wrong
  stopifnot(length(unique(bottom)) == 1)
  bottom = unique(bottom)
  pmax(bottom, c(bottom, diff(x)))
}

safe_approx = function(x, y, newx) {
  if (length(x) < 2)
    list(x = x, y = y)
  else
    approx(x, y, newx)
}

fixed_length_smooth = function(x, y) {
  if (length(x) < 3) {
    list(x = x, y = y)
  }
  else {
    # start = min(which(y > log(0.1))) - 1
    # if (length(x) - start < 10) {
    if (TRUE) {
      # disable lowess for now, very strange artifacts
      ss = supsmu(x, y)
      sx = ss$x
      sy = ss$y
    }
    else{
      if (start > 0) {
        startx = head(x, start)
        x = tail(x, -start)
        starty = head(y, start)
        y = tail(y, -start)
      }
      else {
        startx = numeric()
        starty = numeric()
      }
      ss = lowess(x = x,
                  y = y,
                  f = 10 / length(x))
      x = c(startx, x)
      sx = c(startx, ss$x)
      sy = c(starty, ss$y)
    }
    safe_approx(sx, sy, x)
  }
}

logsmooth = function(x, bottom) {
  if (all(is.na(x))) {
    rep_len(NA_real_, length(x))
  }
  else {
    y = pmax(bottom, x)
    # pmax(bottom, fixed_length_smooth(1:length(y), y)$y)
    pmax(bottom, exp(fixed_length_smooth(1:length(y), log(y))$y))
  }
}

safe_log2 = function(x, bottom)
{
  log2(pmax(x, bottom))
}

spy = function(x, f = identity) {
  print(do.call(paste, as.list(f(x))))
  x
}


more_columns = function(data, prevalence) {
  mutate(
    data,
    value = value / (if (prevalence)
      population/1E5
      else
        1),
    bottom = .1 / if (prevalence)
      population
    else
      1
  ) %>%
    rename(cumulative = value) %>%
    group_by(region) %>%
    arrange(date) %>%
    mutate(smoothed.cumulative = logsmooth(cumulative, bottom)) %>%
    mutate(
      increase = cdiff(cumulative, bottom),
      smoothed.increase = cdiff(smoothed.cumulative, bottom),
      ratio = smoothed.increase/smoothed.cumulative
    ) %>%
    mutate(
      log2.cumulative = safe_log2(cumulative, bottom),
      log2.smoothed.cumulative = safe_log2(smoothed.cumulative, bottom),
      log2.increase = safe_log2(increase, bottom),
      log2.smoothed.increase = safe_log2(smoothed.increase, bottom)
    ) %>%
    filter(date > min(date))
}

high_cases_regions = function(data, n) {
  (
    data %>%
      filter(date == max(date)) %>%
      group_by(region) %>%
      arrange(-smoothed.increase) %>%
      head(n)
  )$region
}

belong_to_level = function(location, data, level) {
  all(location %in% data[[level]])
}

process_data = function(data,
                        level = c("city", "county", "state", "country"),
                        type = c(all_types),
                        top_region,
                        regions,
                        date_range,
                        prevalence) {
  level = match.arg(level)
  type = match.arg(type)
  message(paste(
    c(level, type, top_region, regions),
    sep = " :: ",
    collapse = " : "
  ))

  data = filter_data(
    data,
    level = level,
    type = type,
    top_region = top_region,
    regions = regions,
    date_range = date_range
  ) %>%  set_region(level = level)
  data = more_columns(data, prevalence)
  data
}

trend_calc = function(data) {
  n_days_ago = tail(sort(unique(data$date)), 3)[1]
  last_day = max(data$date)

  data %>%
    filter(date >= n_days_ago, date <= last_day) %>%
    group_by(region) %>%
    arrange(date) %>%
    summarize(
      model1 = if (all(is.na(log2.smoothed.increase)))
        list(NULL)
      else
        list(lm(formula = log2.smoothed.increase ~ date)),
      # model2 = list(lm(formula = log2value ~ poly(date, 2))),
      log2.latest.increase = last(log2.smoothed.increase),
      latest.cumulative = decimal_trunc(last(cumulative)),
      population = last(population)
    ) %>%
    mutate(log2.growth.rate = unlist(sapply(model1, function(x)
      if (is.null(x))
        NA_real_
      else
        x$coeff[2]))) %>%
    mutate(
      growth.rate = 2 ** log2.growth.rate,
      doubling.time = decimal_trunc(1 / log2.growth.rate)
      ) %>%
    mutate(
      daily.growth.percent = trunc((growth.rate - 1) * 100),
      latest.increase = decimal_trunc(2 ** log2.latest.increase)
    ) %>%
    dplyr::select(-starts_with("model"),-starts_with("log2"),-growth.rate,-population)
}
