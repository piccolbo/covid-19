library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)


# df2named_vector = function(keys, values, data = NULL) {
#   if (!is.null(data)) {
#     keys = data[[keys]]
#     values = data[[values]]
#   }
#   v = values
#   names(v) = keys
#   v
# }
#
# three_letter2country = function(x) {
#   map = df2named_vector("Alpha_3", "Name", ISOcodes::ISO_3166_1)
#   unlist(ifelse(is.na(map[x]), x, map[x]))
# }
#
# expand_country_names = function(data) {
#   mutate(data, country = three_letter2country(country))
# }
#
# monotonize = function(x) {
#   c(x[1], pmax(head(x,-1), tail(x,-1)))
# }






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
  country =  corona[is_state(corona), ]$country %>% unique,
  state = discard(corona[is_county(corona), ]$state %>% unique, is.na),
  county = discard(corona[is_city(corona), ]$county %>% unique, is.na)
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


filter_data = function(data,
                       level = all_levels,
                       type = c(all_types),
                       top_region,
                       regions,
                       date_range) {
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
  data$region = data[[level]]
  data
}

decimal_trunc = function(x) {
  round(x * 100) / 100
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
      ss = supsmu(x, y)
      sx = ss$x
      sy = ss$y
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
      population / 1E5
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
      ratio = smoothed.increase / smoothed.cumulative
    ) %>%
    mutate(
      log2.cumulative = safe_log2(cumulative, bottom),
      log2.smoothed.cumulative = safe_log2(smoothed.cumulative, bottom),
      log2.increase = safe_log2(increase, bottom),
      log2.smoothed.increase = safe_log2(smoothed.increase, bottom)
    ) %>%
    filter(date > min(date))
}

high_cases_regions = function(data, level, type, top_region, prevalence, n) {
  (
    process_data(
      data = data,
      level = level,
      type = type,
      top_region = top_region,
      regions = NULL,
      date_range = NULL,
      prevalence = prevalence
    ) %>%
      filter(date == max(date)) %>%
      group_by(region) %>%
      arrange(-smoothed.increase) %>%
      head(n)
  )$region
}

# belong_to_level = function(location, data, level) {
#   all(location %in% data[[level]])
# }

regions_at_level = function(data, level) {
  unique(
    filter_data(
      data = data,
      level = level,
      type = "cases",
      top_region = NULL,
      regions = NULL,
      date_range = NULL
    )[[level]]
  )
}

subregions_of = function(data, level, region) {
  unique(
    filter_data(
      data = data,
      level = level,
      top_region = region,
      type = "cases",
      regions = NULL,
      date_range = NULL
    )[[level]]
  )
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
    dplyr::select(-starts_with("model"),
                  -starts_with("log2"),
                  -growth.rate,
                  -population)
}

