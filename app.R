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

default = function(xx) {
  if (length(xx) > 1) {
    message(xx[[1]])
    if (!is.null(xx[[1]]) && xx[[1]]) {
      xx[[1]]
    }
    else {
      default(xx[-1])
    }
  }
  else {
    xx[[1]]
  }
}

unlink('coronavirus.rda')
con = file('coronavirus.rda', 'wb')
writeBin(content(
  GET(url = 'https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/data/coronavirus.rda')
), con)
close(con)
load('coronavirus.rda')
unlink('coronavirus.rda')

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
      # tabPanel("Data", dataTableOutput("data")),
      tabPanel("Methods", "Data from https://github.com/CSSEGISandData/COVID-19 via https://github.com/RamiKrispin/coronavirus. Smoothed with https://stat.ethz.ch/R-manual/R-devel/library/stats/html/supsmu.html. Trends are computed with a linear model applied to the last 5 days of smoothed data in log scale. \"Days to double\" is a rough estimate of how many days it takes for cases to double. These choices are reasonable given a visual inspection of the data but have not been validated and assume unchanged policies and attitudes in the affected countries, which is hopefully the wrong assumption. This analysis is meant to support the view that we are in an exponential phase of disesase spread in most countries, that is increase from one day to the next as a percentage are roughly constant. No health care system, let alone a system for tracking and isolating cases, can work more than a few days when cases double every 2 or 3 days as we are seeing in several countries as of early March. Only mobilizing a large fraction of the population can work (self-quarantine, social distancing, remote work, canceling gatherings, school closings, travel restrictions etc.). Countries that show a declining number of new cases (for example China, Hong Kong, South Korea as of early March) have applied these population-wide measures. One caveat is that confirmed cases per day may be capped by detection capacity and censorship. In that case check the trends on number of deaths, which are harder to conceal -- but causes of death may be attributed incorrectly")
    ))
  ))

process_data = function(options, smoothing = NULL) {
  data = coronavirus %>%
    group_by(Country.Region, type, date) %>%
    summarise(cases = as.double(sum(cases))) %>%
    filter(
      cases > 0,
      max(cases) > options$min_cases,
      type == options$type,
      date >= options$date_range[1],
      date <= options$date_range[2]
    ) %>%
    arrange(date)
  countries = sort(unique(data$Country.Region))
  data = data %>%
    pivot_wider(
      names_from = Country.Region,
      values_from = cases,
      values_fill = 0.1
    )
  if (options$smoothing == "yes" || (!is.null(smoothing) && smoothing))
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
  data =   pivot_longer(data,
                        cols = countries ,
                        names_to = "Country.Region",
                        values_to = "cases")
  data = mutate(data, logcases = log2(cases + 0.1))
  n_days_ago = tail(sort(unique(data$date)), 5)[1]
  growth = data %>%
    filter(date >= n_days_ago) %>%
    group_by(Country.Region) %>%
    summarize(growth.rate = (lm(formula = logcases ~ date)$coeff[2])) %>%
    mutate(days.to.double = 1 / growth.rate)
  list(
    data = filter(data, Country.Region %in% options$country),
    countries = countries,
    growth = growth
  )
}

server <- function(input, output, session) {
  countries = sort(unique(coronavirus$Country.Region))
  output$cases <- renderPlot({
    pd = process_data(input)
    data = pd$data
    countries = pd$countries

    data$wrap = paste("Group", as.integer(as.factor(data$Country.Region)) %%
                        (length(input$country) %/% 6))
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
      scale_y_log10() +
      theme(legend.position = "none")
    if (length(input$country) > 6) {
      plot = plot + facet_wrap(facets = ~ wrap)
    }
    plot
  }, height = 800)
  output$growth = renderDataTable({
    process_data(input, smoothing=TRUE)$growth
  }, options = list(order = list(list(2, 'desc'))))
  output$data = renderDataTable({
    process_data(input)$data
  })
  output$country_selector = renderUI({
    pd = process_data(input)
    data = pd$data
    countries = pd$countries
    selectizeInput(
      "country",
      "Country",
      choices = countries,
      selected = if (is.null(input$country)) {
        countries
      } else {
        input$country
      },
      multiple = TRUE
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
