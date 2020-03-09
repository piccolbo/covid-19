#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#devtools::install_github("RamiKrispin/coronavirus")
#library(coronavirus)
library(ggplot2)
library(dplyr)
library(directlabels)
library(devtools)
#devtools::install_github("tidyverse/tidyr")
library(tidyr)
library(httr)
library(DT)


unlink('coronavirus.rda')
con = file('coronavirus.rda', 'wb')
writeBin(content(
  GET(url = 'https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/data/coronavirus.rda')
), con)
close(con)
load('coronavirus.rda')
unlink('coronavirus.rda')
all_countries = sort(unique(coronavirus$Country.Region))

options(DT.fillContainer = FALSE)
options(DT.autoHideNavigation = FALSE)

ui <- fluidPage(# Application title
  titlePanel(
    paste(
      "Daily cases per country, confirmed, updated:",
      as.character(max(coronavirus$date))
    )
  ),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "min_cases",
        "Minimum number of cases in one day for a country to be displayed",
        min = 1,
        max = 100,
        value = 10
      ),
      dateRangeInput(
        "date_range",
        "Date range",
        start = min(coronavirus$date),
        end = max(coronavirus$date),
        min = min(coronavirus$date),
        max = max(coronavirus$date)
      ),
      radioButtons(
        "type",
        "Type",
        choices = unique(coronavirus$type),
        selected = "confirmed",
        inline = TRUE
      ),
      uiOutput("country_selector"),
      radioButtons(
        "smoothing",
        "Use smoothing (simpler graphs)",
        choices = c("yes", "no"),
        # selectd = "yes",
        inline = TRUE
      )
    ),

    mainPanel(tabsetPanel(
      tabPanel("Plot", plotOutput("cases")),
      tabPanel("Days to doubling", dataTableOutput("growth")),
      tabPanel("Data", dataTableOutput("data")),
      tabPanel(
        "Methods",
        "Data from https://github.com/CSSEGISandData/COVID-19 via https://github.com/RamiKrispin/coronavirus. Smoothed with https://stat.ethz.ch/R-manual/R-devel/library/stats/html/supsmu.html. Trends are computed with a linear model applied to the last 5 days of smoothed data in log scale. \"Days to double\" is a rough estimate of how many days it takes for cases to double, negative numbers corresponding to decrease or halving time. These choices are reasonable given a visual inspection of the data and the little I know about epidemiologu but have not been validated and assume unchanged policies and attitudes in the affected countries, which is hopefully the wrong assumption, plus negligible levels of immunity in the population, which is correct but is bound to change in the near future. This analysis is meant to support the view that we are in an exponential phase of disesase spread in most countries, that is increase from one day to the next as a percentage are roughly constant. No health care system, let alone a system for tracking and isolating cases, can work more than a few days when cases double every 2 or 3 days as we are seeing in several countries as of early March. Only mobilizing a large fraction of the population can work (self-quarantine, social distancing, remote work, canceling gatherings, school closings, travel restrictions etc.). Countries that show a declining number of new cases (for example China, Hong Kong, South Korea as of early March) have applied these population-wide measures. One caveat is that confirmed cases per day may be capped by detection capacity and censorship. In that case check the trends on number of deaths, which are harder to conceal -- but causes of death may be attributed incorrectly. Feedback: covid19@piccolboni.info"
      )
    ))
  ))

decimal_trunc = function(x)
  trunc(x * 100) / 100

process_data = function(options, smoothing = NULL) {
  data = coronavirus %>%
    group_by(Country.Region, type, date) %>%
    summarise(cases = as.double(sum(cases))) %>%
    filter(cases > 0,
           max(cases) > options$min_cases,
           type == options$type) %>%
    arrange(date)
  high_cases_countries = sort(unique(data$Country.Region))
  data = data %>%
    pivot_wider(
      names_from = Country.Region,
      values_from = cases,
      values_fill = 0.1
    )
  if (options$smoothing == "yes" ||
      (!is.null(smoothing) && smoothing))
  {
    data =
      mutate_if(
        data,
        .predicate = is.numeric,
        .funs = function(y) {
          supsmu(1:length(y), y)$y
        }
      )
  }
  data =  pivot_longer(data,
                      cols = high_cases_countries ,
                      names_to = "Country.Region",
                      values_to = "cases")
  data = mutate(data, logcases = log2(cases + 0.1)) %>%
    filter(date >= options$date_range[1],
           date <= options$date_range[2])
  n_days_ago = tail(sort(unique(data$date)), 5)[1]
  growth = data %>%
    filter(date >= n_days_ago) %>%
    group_by(Country.Region) %>%
    summarize(growth.rate = (lm(formula = logcases ~ date)$coeff[2])) %>%
    mutate(days.to.double = decimal_trunc(1 / growth.rate),
           growth.rate = decimal_trunc(growth.rate))
  top_countries = (data %>%
    group_by(Country.Region) %>%
    filter(cases == max(cases)) %>%
    arrange(-cases) %>%
    head(5))$Country.Region
  list(
    data = filter(data, Country.Region %in% ( if (!is.null(options$country))
      options$country
      else
        top_countries)),
    growth = growth
  )
}

server <- function(input, output, session) {
  output$cases <- renderPlot({
    pd = process_data(input)
    data = pd$data
    countries = sort(unique(data$Country.Region))
     plot = ggplot(
      data = data,
      mapping = aes(
        x = date,
        y = cases,
        label = Country.Region,
        color = Country.Region
      )
    ) +
      geom_line() +
      geom_dl(method = "angled.boxes") +
      scale_y_log10(labels = identity) +
      theme(legend.position = "none")
    # if (length(input$country) > 6) {
    #   plot = plot + facet_wrap(facets = ~ wrap)
    # }
    plot
  }, height = 800)
  output$growth = renderDataTable({
    process_data(input, smoothing = TRUE)$growth
  }, options = list(order = list(list(2, 'desc'))))
  output$data = renderDataTable({
    process_data(input)$data
  })
  output$country_selector = renderUI({
    pd = process_data(input)
    data = pd$data
    selectizeInput(
      "country",
      "Country",
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
