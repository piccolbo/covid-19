library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

load("data-sources.Rdata")



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
  country =  corona[is_state(corona), ]$country %>% unique %>% sort,
  state = discard(corona[is_county(corona), ]$state %>% unique %>% sort, is.na),
  county = discard(corona[is_city(corona), ]$county %>% unique %>% sort, is.na)
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
  typ = type# if(!is.null(type)) match.arg(type) else NULL
  level_filter = list(
    city = is_city,
    county = is_county,
    state = is_state,
    country = is_country
  )[[level]]
  data = filter(data, level_filter(data))
  if(!is.null(typ)) {
    data = filter(data, typ == type)
  }
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
  as.numeric(format(x = x, digits = 2, scientific = FALSE))
}

cdiff = function(x) {
        c(NA, diff(x))
 }

fixed_length_smooth = function(x, y) {
  if (length(x) < 3) {
    list(x = x, y = y)
  }
  else {
    ss = supsmu(x, log(y + 1))
    sx = ss$x
    sy = ss$y
    ret = approx(sx, sy, x)
    ret$y = exp(ret$y)-1
    ret
  }
}

fixed_length_smooth_1 = function(x, y) {
  if (length(x) < 3) {
    list(x = x, y = y)
  }
  else {
    ss = supsmu(x, y)
    sx = ss$x
    sy = ss$y
    ret = approx(sx, sy, x)
    ret
  }
}

pos_smooth = function(x) {
  if (all(is.na(x)))
    rep_len(NA_real_, length(x))
  else
    pmax(0,fixed_length_smooth(1:length(x), x)$y)
}

spy = function(x, f = identity) {
  print(do.call(paste, as.list(f(x))))
  x
}

very_small = function(population, prevalence) {
  if(prevalence) 1E5/population else 1
}

active = function(new) {
  reduce(lapply(0:14, function(l) lag(new, l)), `+`)/15
}

more_columns = function(data, prevalence) {
  mutate(
    data,
    value = value / (if (prevalence)
      population / 1E5
      else
        1)
  ) %>%
    rename(cumulative = value) %>%
    group_by(region) %>%
    arrange(date) %>%
    mutate(smoothed.cumulative = pos_smooth(cumulative)) %>%
    mutate(
      new = cdiff(cumulative),
      smoothed.new = cdiff(smoothed.cumulative),
      active = active(smoothed.new),
      ratio = ifelse(
        active > very_small(population, prevalence),
        (smoothed.new + very_small(population, prevalence)) /
          (active + very_small(population, prevalence)),
        NA
      )
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
      arrange(-smoothed.new) %>%
      head(n)
  )$region
}


regions_at_level = function(data, level) {
  filter_data(
    data = data,
    level = level,
    type = "cases",
    top_region = NULL,
    regions = NULL,
    date_range = NULL
  )[[level]] %>%
    unique %>%
    sort
}

subregions_of = function(data, level, region) {
  filter_data(
    data = data,
    level = level,
    top_region = region,
    type = "cases",
    regions = NULL,
    date_range = NULL
  )[[level]] %>%
    unique %>%
    sort
}

process_data = function(data,
                        level = c("city", "county", "state", "country"),
                        type = c(all_types),
                        top_region,
                        regions,
                        date_range,
                        prevalence) {
  level = match.arg(level)
  message(paste(
    c(level, type, top_region, regions, date_range[1], date_range[2], prevalence),
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
      model1 = if (all(is.na(log2.smoothed.new)))
        list(NULL)
      else
        list(lm(formula = log2.smoothed.new ~ date)),
      log2.latest.new = last(log2.smoothed.new),
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
      latest.new = decimal_trunc(2 ** log2.latest.new)
    ) %>%
    dplyr::select(-starts_with("model"),
                  -starts_with("log2"),
                  -growth.rate,
                  -population)
}

