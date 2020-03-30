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
                         guess_max = 1000000)

# corona = readr::read_csv("timeseries-tidy.csv.zip",
#                          guess_max = 1000000)

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
        start = max(all_dates) - 21,
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
      tabPanel("Time series", plotOutput("timeseries")),
      tabPanel("Growth vs Size", plotOutput("growthvssize")),
      tabPanel("Stats and projections", dataTableOutput("growth")),
      tabPanel("Data", dataTableOutput("data")),
      tabPanel(
        "Methods and credits",
        "Data from https://coronadatascraper.com/ Smoothed with https://stat.ethz.ch/R-manual/R-devel/library/stats/html/supsmu.html after a log transform (because the smoother is locally linear, and in log scale these trends appear locally linear) on the cumulative counts. Trends are computed with a linear model applied to the last 3 days of smoothed data in log scale (on the daily count, which I may reconsider). \"Doubling time\" is an estimate of how many days it takes for cases to double, negative numbers corresponding to decrease or halving time. Cases in 2 weeks is not a prediction, is to get an idea of what the current trend means, because most people don't get exponential growth. These choices are reasonable given a visual inspection of the data and the little I know about epidemiology but have not been validated and assume unchanged policies and attitudes in the affected countries, which is hopefully the wrong assumption, plus negligible levels of immunity in the population, which is correct but is bound to change in the future. I do have a background in science, but this has been hastly produced and not peer-reviewed. I which epidemiologists assisting governemnts communicated with the public in a way that makes this work irrelevant, but they don't at least in the US. You get soundbites at best. This analysis is meant to support the view that we are in an exponential phase of disesase spread in most countries and states, that is increase from one day to the next as a percentage is roughly constant. No health care system, let alone a system for tracking and isolating cases, can work more than a few weeks when cases double every 2 or 3 days as we are seeing in several countries as of early March. Only mobilizing a large fraction of the population can work (self-quarantine, social distancing, remote work, canceling gatherings, school closings, travel restrictions etc.). Countries that show a declining number of new cases (for example China, Hong Kong, South Korea as of early March) have applied these population-wide measures. Major caveat is that confirmed cases per day are capped by detection capacity for large outbreaks or in countries run by incompetent people and by censorship. In that case check the trends on number of deaths, which are harder to conceal -- but causes of death may be attributed incorrectly.  Code: https://github.com/piccolbo/covid-19 Feedback: covid19@piccolboni.info"
      )
    ))
  ))

decimal_trunc = function(x)
  trunc(x * 100) / 100

bottom = .1

cdiff = function(x) {
  pmax(0, c(bottom, diff(x)))
}

safe_approx = function(x,y, newx){
  if (length(x) < 2)
    list(x=x, y=y)
  else
    approx(x,y,newx)
}

fixed_length_smooth = function(x,y){
  ss = supsmu(x,y)
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


process_data = function(world, type) {
  tp = type
  data = corona %>%
    mutate(country = three_letter2country(country)) %>%
    filter(type  == tp)

  data = if (world) {
    rename(data, region = "country")
  }
  else{
    filter(data, is_state(data) & country == "United States") %>%
      rename(region = "state")
  }


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
    )


}

high_cases_regions = function(data) {
  (data %>%
     filter(date == max(date)) %>%
     group_by(region) %>%
     arrange(-smoothed.increase) %>%
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
    dplyr::select(-starts_with("model"),
                  -starts_with("log2"),
                  -growth.rate,
                  -population)
}


server <- function(input, output, session) {
  reset_selection = FALSE
  output$timeseries = renderPlot({
    data = process_data(input$world_or_US == "Countries", input$type) %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2]) %>%
      filter_regions(input$regions)
    plot = ggplot(
      data = data,
      mapping = aes(
        x = date,
        y = (if (input$smoothing == "yes")
          smoothed.increase
          else
            increase) / (if (input$density == "yes")
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
      ylab(paste(input$type, (if (input$density == "yes")
        "per hundred thousands"
        else
          ""))) +
      theme(legend.position = "none")
    plot
  }, height = 800)
  output$growthvssize = renderPlot({
    data = process_data(input$world_or_US == "Countries", input$type) %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2]) %>%
      filter_regions(input$regions)
    plot = ggplot(
      data = data,
      mapping = aes(
        x = cumulative.value / (if (input$density == "yes")
          population / 1E5
          else
            1),
        y = decimal_trunc(smoothed.increase / smoothed.cumulative.value),
        label = region,
        color = region
      )
    ) +
      geom_line() +
      geom_point() +
      geom_dl(method = "angled.boxes") +
      scale_x_log10(labels = identity) +
      scale_y_log10(labels = identity) +
      xlab(paste(input$type, (if (input$density == "yes")
        "per hundred thousands"
        else
          ""))) +
      ylab("growth rate")+
      theme(legend.position = "none")
    plot
  }, height = 800)
  output$growth = renderDataTable({
    trend_calc(process_data(input$world_or_US == "Countries", input$type))
  }, options = list(order = list(list(5, 'desc'))))
  output$data = renderDataTable({
    process_data(input$world_or_US == "Countries", input$type)
  })
  output$region_selector = renderUI({
    data = process_data(input$world_or_US == "Countries", input$type)
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
