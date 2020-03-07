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
library(viridis)

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
      sliderInput(
        "span",
        "Amount of smoothing to apply (0 for no smoothing)",
        min = 0,
        max = 1,
        value = 0.5
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(plotOutput("distPlot"))
  ))

process_data = function(input) {
  data = coronavirus %>%
    group_by(Country.Region, type, date) %>%
    summarise(cases = as.double(sum(cases))) %>%
    filter(
      cases > 0,
      max(cases) > input$min_cases,
      type == input$type,
      date >= input$date_range[1],
      date <= input$date_range[2]
    ) %>%
    arrange(date)
  data = pivot_longer(
    data = pivot_wider(
      data = data,
      names_from = Country.Region,
      values_from = cases,
      values_fill = 0.1
    ),
    cols = unique(data$Country.Region) ,
    names_to = "Country.Region",
    values_to = "cases"
  )
  countries = sort(unique(data$Country.Region))
  list(data = filter(data, Country.Region %in% input$country),
       countries = countries)
}
server <- function(input, output, session) {
  countries = sort(unique(coronavirus$Country.Region))
  output$distPlot <- renderPlot({
    pd = process_data(input)
    data = pd$data
    countries = pd$countries
    if (input$span == 0) {
      main_geom = geom_line()
    }
    else {
      main_geom = geom_smooth(se = FALSE, span = input$span)
    }

    data$wrap = paste("Group", as.integer(as.factor(data$Country.Region))%%(length(countries)%/%6))
    ggplot(
      data = data,
      mapping = aes(
        x = date,
        y = cases,
        label = Country.Region,
        color = Country.Region
      )
    ) +
      main_geom +
      geom_dl(method = "angled.boxes") +
      scale_y_log10() +
      facet_wrap(facets = ~wrap) +
      theme(legend.position="none")
  }, height = 800)
  output$country_selector = renderUI({
    pd = process_data(input)
    data = pd$data
    countries = pd$countries
    selectizeInput(
      "country",
      "Country",
      choices = countries,
      selected = if(is.null(input$country)){ countries} else {input$country},
      multiple = TRUE
    )
  })

}

# Run the application
shinyApp(ui = ui, server = server)
