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



corona_wide = bind_rows(
  .id = "type",
  confirmed = readr::read_csv(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
  ),
  deaths = readr::read_csv(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
  ),
  recovered = readr::read_csv(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
  )
)

all_countries = sort(unique(corona_wide$`Country/Region`))

all_dates = purrr::discard(lubridate::mdy(colnames(corona_wide)), .p = is.na)

options(DT.fillContainer = FALSE)
options(DT.autoHideNavigation = FALSE)

ui <- fluidPage(# Application title
  titlePanel(
    paste(
      "Daily cases per country, confirmed, updated",
      as.character(max(all_dates))
    )
  ),

  # Sidebar with a slider input for number of bins
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
        choices = unique(corona_wide$type),
        selected = "confirmed",
        inline = TRUE
      ),
      radioButtons(
        "density",
        "Convert to density(cases per 1M people)",
        choices = c("yes", "no"),
        inline = TRUE
      ),
      uiOutput("country_selector"),
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
        "Data from https://github.com/CSSEGISandData/COVID-19  Smoothed with https://stat.ethz.ch/R-manual/R-devel/library/stats/html/supsmu.html after a log transform (because the smoother is locally linear, and in log scale these trends appear close to linear) on the cumulative counts. Trends are computed with a linear model applied to the last 5 days of smoothed data in log scale (on the daily count, which I may reconsider). \"Days to double\" is a rough estimate of how many days it takes for cases to double, negative numbers corresponding to decrease or halving time. \"Days to 1M\" likewise (per day). These choices are reasonable given a visual inspection of the data and the little I know about epidemiology but have not been validated and assume unchanged policies and attitudes in the affected countries, which is hopefully the wrong assumption, plus negligible levels of immunity in the population, which is correct but is bound to change in the near future. I do have a background in science, but this has been hastly produced and not peer-reviewed. This analysis is meant to support the view that we are in an exponential phase of disesase spread in most countries, that is increase from one day to the next as a percentage is roughly constant. No health care system, let alone a system for tracking and isolating cases, can work more than a few days when cases double every 2 or 3 days as we are seeing in several countries as of early March. Only mobilizing a large fraction of the population can work (self-quarantine, social distancing, remote work, canceling gatherings, school closings, travel restrictions etc.). Countries that show a declining number of new cases (for example China, Hong Kong, South Korea as of early March) have applied these population-wide measures. Major caveat is that confirmed cases per day may be capped by detection capacity for large outbreaks or in countries run by incompetent people and by censorship. In that case check the trends on number of deaths, which are harder to conceal -- but causes of death may be attributed incorrectly. Code: https://github.com/piccolbo/covid-19 Feedback: covid19@piccolboni.info"
      )
    ))
  ))

decimal_trunc = function(x)
  trunc(x * 100) / 100


diff_smooth = function(x, smoothing) {
  bottom = 1
  x = pmax(bottom, x)
  if (smoothing) {
    pmax(bottom, c(bottom, diff(exp(
      supsmu(1:length(x), log(x))$y
    ))))
  }
  else
    c(bottom, diff(x))
}

spy = function(x, f) {
  message(do.call(paste, as.list(f(x))))
  x
}

process_data = function(options, smoothing = NULL) {
  tryCatch (
    unsafe_process_data(options, smoothing),
    error = function(e) {
      corona_wide = select(corona_wide, -NCOL(corona_wide))
    }
  )
}

unsafe_process_data = function(options, smoothing) {
  smoothing = (if (is.null(smoothing))
    options$smoothing == "yes"
    else
      smoothing)
  data = corona_wide %>%
    filter(type  == options$type) %>%
    pivot_longer(cols = ends_with("/20")) %>%
    group_by(`Country/Region`, name) %>% summarise(cases = sum(value)) %>%
    mutate(date = lubridate::mdy(name)) %>% select(-name) %>%
    arrange(date) %>%
    mutate(total.cases = cases, cases = diff_smooth(cases, smoothing)) %>%
    rename(Country.Region = "Country/Region") %>%
    mutate(log2cases = log2(ifelse(cases > 0, cases, 0.1)))

  country_translate = function(x) {
    name_translation = c(
      "US" = "United States",
      "Slovakia" = "Slovak Republic",
      "Russia" = "Russian Federation",
      "Korea, South" = "Korea, Rep.",
      "Iran" = "Iran, Islamic Rep.",
      "Egypt" = "Egypt, Arab Rep.",
      "Czechia" = "Czech Republic",
      "Brunei" = "Brunei Darussalam"
    )
    trn = names(name_translation)
    names(trn) = name_translation
    if (is.na(trn[x]))
      x
    else
      trn[x]
  }


  pop_data = wb(indicator = "SP.POP.TOTL", mrv = 1) %>%
    mutate(country = unlist(purrr::map(.x = country, .f = country_translate)))
  data = left_join(
    data,
    pop_data %>% select(country, population = value),
    by = c("Country.Region" = "country")
  )
  n_days_ago = tail(sort(unique(data$date)), 6)[1]
  last_day = max(data$date)
  growth = data %>%
    filter(date >= n_days_ago, date < last_day) %>%
    group_by(Country.Region) %>%
    arrange(date) %>%
    summarize(
      log2.growth.rate = (lm(formula = log2cases ~ date)$coeff[2]),
      log2.latest.cases = last(log2cases)
    ) %>%
    mutate(
      growth.rate = 2 ** log2.growth.rate,
      days.to.double = decimal_trunc(1 / log2.growth.rate)
    ) %>%
    mutate(days.to.1M = {
      d = (log2(10E6) - log2.latest.cases) / log2.growth.rate
      ifelse(d > 0, trunc(d), Inf)
    }) %>%
    mutate(daily.growth.percent = trunc((growth.rate - 1)*100)) %>%
    dplyr::select(Country.Region,
                  days.to.double,
                  days.to.1M,
                  daily.growth.percent)
  high_cases_countries = (
    data %>%
      filter(date == max(date)) %>%
      group_by(Country.Region) %>%
      arrange(-cases) %>%
      head(12)
  )$Country.Region

  list(data = filter(
    data,
    Country.Region %in% (if (!is.null(options$country))
      options$country
      else
        high_cases_countries)
  ),
  growth = growth)
}

server <- function(input, output, session) {
  output$cases <- renderPlot({
    pd = process_data(input)
    data = pd$data    %>%  filter(date >= input$date_range[1],
                                  date <= input$date_range[2])
    countries = sort(unique(data$Country.Region))
    plot = ggplot(
      data = data,
      mapping = aes(
        x = date,
        y = cases/(if(input$density=="yes") population/1E5 else 1),
        label = Country.Region,
        color = Country.Region
      )
    ) +
      geom_line() +
      geom_dl(method = "angled.boxes") +
      scale_y_log10(labels = identity) +
      ylab(paste("cases",( if(input$density=="yes") "per hundred thousands" else  ""))) +
      theme(legend.position = "none")
    plot
  }, height = 800)
  output$growth = renderDataTable({
    process_data(input, smoothing = TRUE)$growth
  }, options = list(order = list(list(4, 'desc'))))
  output$data = renderDataTable({
    process_data(input)$data
  })
  output$country_selector = renderUI({
    pd = process_data(input)
    data = pd$data
    selectizeInput(
      "country",
      "Country (biggest current outbreaks shown, click for more)",
      choices = all_countries,
      selected = if (is.null(input$country)) {
        sort(unique(data$Country.Region))
      } else {
        input$country
      },
      multiple = TRUE
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
