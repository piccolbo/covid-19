#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(directlabels)
library(devtools)
library(tidyr)
library(httr)
library(DT)
library(wbstats)


corona = readr::read_csv("https://coronadatascraper.com/timeseries-tidy.csv",
                         guess_max = 100000)


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

options(DT.fillContainer = FALSE)
options(DT.autoHideNavigation = FALSE)

ui <- fluidPage(# Application title
  titlePanel(
    paste(
      "Daily cases per country or state, confirmed, updated",
      as.character(max(all_dates))
    )
  ),

  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "date_range",
        "Date range",
        start = max(all_dates) - 15,
        end = max(all_dates),
        min = min(all_dates),
        max = max(all_dates)
      ),
      radioButtons(
        "type",
        "Type",
        choices = unique(corona$type),
        selected = "cases",
        inline = TRUE
      ),
      radioButtons(
        "world_or_US",
        "Show countries or US States",
        choices = c("Countries", "US States"),
        selected = "Countries",
        inline = TRUE
      ),
      radioButtons(
        "density",
        "Convert to prevalence (cases per 100K people)",
        choices = c("no", "yes"),
        inline = TRUE
      ),
      uiOutput("region_selector"),
      radioButtons(
        "smoothing",
        "Use smoothing (simpler graphs)",
        choices = c("yes", "no"),
        inline = TRUE
      )
    ),

    mainPanel(tabsetPanel(
      tabPanel("Plot", plotOutput("cases")),
      tabPanel("Days to doubling", dataTableOutput("growth")),
      # tabPanel("Data", dataTableOutput("data")),
      tabPanel(
        "Methods",
        "Data from coronascraper Smoothed with https://stat.ethz.ch/R-manual/R-devel/library/stats/html/supsmu.html after a log transform (because the smoother is locally linear, and in log scale these trends appear close to linear) on the cumulative counts. Trends are computed with a linear model applied to the last 5 days of smoothed data in log scale (on the daily count, which I may reconsider). \"Days to double\" is a rough estimate of how many days it takes for cases to double, negative numbers corresponding to decrease or halving time. \"Days to 1M\" likewise (per day). These choices are reasonable given a visual inspection of the data and the little I know about epidemiology but have not been validated and assume unchanged policies and attitudes in the affected countries, which is hopefully the wrong assumption, plus negligible levels of immunity in the population, which is correct but is bound to change in the near future. I do have a background in science, but this has been hastly produced and not peer-reviewed. This analysis is meant to support the view that we are in an exponential phase of disesase spread in most countries, that is increase from one day to the next as a percentage is roughly constant. No health care system, let alone a system for tracking and isolating cases, can work more than a few days when cases double every 2 or 3 days as we are seeing in several countries as of early March. Only mobilizing a large fraction of the population can work (self-quarantine, social distancing, remote work, canceling gatherings, school closings, travel restrictions etc.). Countries that show a declining number of new cases (for example China, Hong Kong, South Korea as of early March) have applied these population-wide measures. Major caveat is that confirmed cases per day may be capped by detection capacity for large outbreaks or in countries run by incompetent people and by censorship. In that case check the trends on number of deaths, which are harder to conceal -- but causes of death may be attributed incorrectly. Code: https://github.com/piccolbo/covid-19 Feedback: covid19@piccolboni.info"
      )
    ))
  ))

decimal_trunc = function(x)
  trunc(x * 100) / 100


diff_smooth = function(x, smoothing) {
  bottom = .1
  x = pmax(bottom, x)
  if (smoothing) {
    pmax(bottom,
         c(bottom,
           diff(exp(
             x = supsmu(1:length(x), log(x))$y
           ))))
  }
  else
    c(bottom, diff(x))
}

spy = function(x, f) {
  print(do.call(paste, as.list(f(x))))
  x
}

state_abb_map = function(x) {
  v = state.name
  names(v) = state.abb
  y = v[x]
  if (is.na(y))
    1
  else
    y
}


process_data = function(options, smoothing = NULL) {
  smoothing = (if (is.null(smoothing))
    options$smoothing == "yes"
    else
      smoothing)
  world = options$world_or_US == "Countries"
  data = corona %>%
    filter(type  == options$type)


  data =  if (world) {
    filter(data, is_country(data)) %>%
      rename(region = "country")

  }
  else{
    filter(data, is_state(data) & country == "USA") %>%
      rename(region = "state")
  }


  data =
    group_by(data, region) %>%
    arrange(date) %>%
    mutate(total.value = value, value = diff_smooth(value, smoothing)) %>%
    mutate(log2value = log2(ifelse(value > 0, value, 0.1)))

  data
}

high_cases_regions = function(data) {
  (data %>%
     filter(date == max(date)) %>%
     group_by(region) %>%
     arrange(-value) %>%
     head(12))$region
}

filter_regions = function(data, regions) {
  filter(data,
         region %in% regions)
}

trend_calc = function(data) {
  n_days_ago = tail(sort(unique(data$date)), 3)[1]
  last_day = max(data$date)

  data %>%
    filter(date >= n_days_ago, date <= last_day) %>%
    group_by(region) %>%
    arrange(date) %>%
    summarize(
      model1 = list(lm(formula = log2value ~ date)),
      # model2 = list(lm(formula = log2value ~ poly(date, 2))),
      log2.latest.value = last(log2value),
      latest.total.value = last(total.value),
      population = last(population)
    ) %>%
    mutate(
      log2.growth.rate = sapply(model1, function(x)
        x$coeff[2]),
      growth.rate = 2 ** log2.growth.rate,
      days.to.double = decimal_trunc(1 / log2.growth.rate)#,
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


server <- function(input, output, session) {
  reset_selection = FALSE
  output$cases <- renderPlot({
    data = process_data(input) %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2]) %>%
      filter_regions(input$regions)
    plot = ggplot(
      data = data,
      mapping = aes(
        x = date,
        y = value / (if (input$density == "yes")
          population / 1E5
          else
            1),
        label = region,
        color = region
      )
    ) +
      geom_line() +
      geom_dl(method = "angled.boxes") +
      scale_y_log10(labels = identity) +
      ylab(paste("cases", (if (input$density == "yes")
        "per hundred thousands"
        else
          ""))) +
      theme(legend.position = "none")
    plot
  }, height = 800)
  output$growth = renderDataTable({
    trend_calc(process_data(input, smoothing = TRUE))
  }, options = list(order = list(list(5, 'desc'))))
  output$data = renderDataTable({
    process_data(input)
  })
  output$region_selector = renderUI({
    data = process_data(input)
    all_regions = sort(unique(data$region))
    selectizeInput(
      "regions",
      "Region (biggest current outbreaks shown, click for more)",
      choices = all_regions,
      selected = if (is.null(input$regions) ||
                     !all(input$regions %in% all_regions)) {
        high_cases_regions(data)
      } else {
        input$regions
      },
      multiple = TRUE
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
