library(readr)
library(dplyr)

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
  unlist(map[x])
}

expand_country_names = function(data) {
  mutate(data, country = three_letter2country(country))
}


corona = readr::read_csv(
  "https://coronadatascraper.com/timeseries-tidy.csv",
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
) %>%   expand_country_names



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

all_dates = unique(corona$date)
all_types = unique(corona$type)




filter_regions = function(data, regions, level) {
  filter_(data,
          paste(level, " %in% regions"))
}


all_levels = c("city", "county", "state", "country")

one_level_up = function(level) {
  all_levels[which(all_levels == level) + 1]
}

filter_data = function(data,
                       level = all_levels,
                       type = c(all_types),
                       top_region = NULL,
                       regions = NULL,
                       date_range = NULL) {
  level = match.arg(level)
  top_level = one_level_up(level)
  typ = match.arg(type)
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
  rename(data, region = level)
}


decimal_trunc = function(x)
  trunc(x * 100) / 100

bottom = .1

cdiff = function(x) {
  #this is wrong
  pmax(0, c(bottom, diff(x)))
}

safe_approx = function(x, y, newx) {
  if (length(x) < 2)
    list(x = x, y = y)
  else
    approx(x, y, newx)
}

fixed_length_smooth = function(x, y) {
  ss = supsmu(x, y)
  safe_approx(ss$x, ss$y, x)
}

logsmooth = function(x) {
  y = pmax(bottom, x)
  pmax(bottom, exp(fixed_length_smooth(1:length(y), log(y))$y))
}

safe_log2 = function(x)
  log2(pmax(x, bottom))

spy = function(x, f) {
  print(do.call(paste, as.list(f(x))))
  x
}


more_columns = function(data) {
  group_by(data, region) %>%
    arrange(date) %>%
    rename(cumulative.value = value) %>%
    mutate(smoothed.cumulative.value = logsmooth(cumulative.value)) %>%
    mutate(
      increase = cdiff(cumulative.value),
      smoothed.increase = cdiff(smoothed.cumulative.value)
    ) %>%
    mutate(
      log2.cumulative.value = safe_log2(cumulative.value),
      log2.smoothed.cumulative.value = safe_log2(smoothed.cumulative.value),
      log2.increase = safe_log2(increase),
      log2.smoothed.increase = safe_log2(smoothed.increase)
    ) %>%
    filter(date > min(date))
}

high_cases_regions = function(data) {
  (
    data %>%
      filter(date == max(date)) %>%
      group_by(region) %>%
      arrange(-smoothed.increase) %>%
      head(12)
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
                        date_range) {
  level = match.arg(level)
  type = match.arg(type)
  message(paste(
    c(level, type, top_region, regions),
    sep = " :: ",
    collapse = " : "
  ))
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

  data = filter_data(
    data,
    level = level,
    type = type,
    top_region = top_region,
    regions = regions,
    date_range = date_range
  ) %>%  set_region(level = level)
  data = more_columns(data)
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
      model1 = list(lm(formula = log2.smoothed.increase ~ date)),
      # model2 = list(lm(formula = log2value ~ poly(date, 2))),
      log2.latest.value = last(log2.smoothed.increase),
      latest.cumulative.value = last(cumulative.value),
      population = last(population)
    ) %>%
    mutate(
      log2.growth.rate = sapply(model1, function(x)
        x$coeff[2]),
      growth.rate = 2 ** log2.growth.rate,
      doubling.time = decimal_trunc(1 / log2.growth.rate)#,
      #concavity = sapply(model2, function(x)        x$coeff[3])
      # pval = sapply(model2, function(x) (x %>% summary %>% coefficients)[3,4])
    ) %>%
    mutate(in.15.days = sapply(model1, function(x)
      trunc(2 ** predict(
        x, newdata = data.frame(date = last_day + 15)
      )))) %>%
    mutate(
      daily.growth.percent = trunc((growth.rate - 1) * 100),
      latest.value = trunc(2 ** log2.latest.value),
      latest.value.per.hundred.thousand =
        decimal_trunc(latest.value * 1E5 / population)
    ) %>%
    dplyr::select(-starts_with("model"),-starts_with("log2"),-growth.rate,-population)
}
