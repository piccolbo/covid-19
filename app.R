#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


source("data_processing.R")
source("plots.R")
library(ggplot2)
library(directlabels)
library(DT)

options(DT.fillContainer = FALSE)
options(DT.autoHideNavigation = FALSE)

ui <- fluidPage(# Application title
  titlePanel(
    paste(
      "Daily new cases (deaths, recovered, tested) per country or state,  updated as of",
      as.character(max(all_dates))
    )
  ),

  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "date_range",
        "Date range (default last three weeks)",
        start = max(all_dates) - 21,
        end = max(all_dates),
        min = min(all_dates),
        max = max(all_dates)
      ),
      radioButtons(
        "type",
        "Type",
        choices = all_types,
        selected = "cases",
        inline = TRUE
      ),
      radioButtons(
        "level",
        "Choose admin unit level to visualize (US terminology used for all countries)",
        choices = c("country", "state", "county"),
        selected = "country",
        inline = TRUE
      ),
      checkboxInput(
        "prevalence",
        "Convert to prevalence (cases per 100K people)",
        value = TRUE
      ),
      uiOutput("top_region_choice"),
      uiOutput("region_selector"),
      checkboxInput("smoothing",
                    "Use smoothing (simpler graphs)",
                    value = TRUE)
    ),

    mainPanel(
      tabsetPanel(
        id = "mainPanel-tabsetPanel",
        tabPanel(
          "Time series",
          plotOutput("timeseries", height = "800px")#,
          # downloadButton("download_timeseries")
        ),
        tabPanel("Growth vs Size", plotOutput("growthvssize", height = "800px")),
        tabPanel("Stats and projections", dataTableOutput("stats")),
        tabPanel("Data", value = "data", dataTableOutput("data")),
        tabPanel(
          "Methods and credits",
          "Data from https://coronadatascraper.com/ Smoothed with https://stat.ethz.ch/R-manual/R-devel/library/stats/html/supsmu.html after a log transform (because the smoother is locally linear, and in log scale these trends appear locally linear) on the cumulative counts. Trends are computed with a linear model applied to the last 3 days of smoothed data in log scale (on the daily count, which I may reconsider). \"Doubling time\" is an estimate of how many days it takes for cases to double, negative numbers corresponding to decrease or halving time. Cases in 2 weeks is not a prediction, is to get an idea of what the current trend means, because most people don't get exponential growth. These choices are reasonable given a visual inspection of the data and the little I know about epidemiology but have not been validated and assume unchanged policies and attitudes in the affected countries, which is hopefully the wrong assumption, plus negligible levels of immunity in the population, which is correct but is bound to change in the future. I do have a background in science, but this has been hastly produced and not peer-reviewed. I which epidemiologists assisting governemnts communicated with the public in a way that makes this work irrelevant, but they don't at least in the US. You get soundbites at best. This analysis is meant to support the view that we are in an exponential phase of disesase spread in most countries and states, that is increase from one day to the next as a percentage is roughly constant. No health care system, let alone a system for tracking and isolating cases, can work more than a few weeks when cases double every 2 or 3 days as we are seeing in several countries as of early March. Only mobilizing a large fraction of the population can work (self-quarantine, social distancing, remote work, canceling gatherings, school closings, travel restrictions etc.). Countries that show a declining number of new cases (for example China, Hong Kong, South Korea as of early March) have applied these population-wide measures. Major caveat is that confirmed cases per day are capped by detection capacity for large outbreaks or in countries run by incompetent people and by censorship. In that case check the trends on number of deaths, which are harder to conceal -- but causes of death may be attributed incorrectly.  Code: https://github.com/piccolbo/covid-19 Feedback: covid19@piccolboni.info"
        )
      )
    )
  ))

safe_select_regions = function(regions, data) {
  if (is.null(regions) ||
      !all(regions %in% data$region)) {
    high_cases_regions(data)
  } else {
    regions
  }
}


belongs_to_level = function(region, data, level) {
  if (is.null(region)) {
    NULL
  }
  else{
    if (region %in% unique(data[[level]]))
      region
    else
      NULL
  }
}

server <- function(input, output, session) {
  observeEvent(session$clientData$url_hostname, {
    message(session$clientData$url_hostname)
    if (session$clientData$url_hostname != "127.0.0.1") {
      message("hide tab")
      hideTab(inputId = "mainPanel-tabsetPanel", target = "data")
    }
  })


  output$top_region_choice = renderUI({
    message("output$top_region_choice")
    if (input$level != "country") {
      data = process_data(
        data = corona,
        level = one_level_up(input$level),
        top_region = NULL,
        regions = NULL,
        type = input$type,
        date_range = input$date_range
      )
      all_regions = sort(unique(data$region))
      selectizeInput(
        "top_region",
        "Focus on this specific region",
        choices = all_regions,
        selected = safe_select_regions(input$region, data)[1],
        multiple = FALSE
      )
    }
  })
  output$region_selector = renderUI({
    message("output$region_selector")
    data = process_data(
      data = corona,
      level = input$level,
      type = input$type,
      top_region = belongs_to_level(input$top_region, corona, one_level_up(input$level)),
      regions = NULL,
      date_range = input$date_range
    )
    all_regions = sort(unique(data$region))
    selectizeInput(
      "regions",
      "Regions to show data from (biggest current outbreaks shown, click for more)",
      choices = all_regions,
      selected = safe_select_regions(sort(input$regions), data),
      multiple = TRUE
    )
  })


  output$timeseries = renderCachedPlot({
    message("output$timeseries")
    if (!is.null(input$regions)) {
      data = process_data(
        data = corona,
        level = input$level,
        type = input$type,
        top_region = input$top_region,
        regions = input$regions,
        date_range = input$date_range
      )
      print(
        plot_timeseries(
          data,
          type = input$type,
          prevalence = input$prevalence,
          smoothing = input$smoothing
        )
      )
    }
  },
  cacheKeyExpr = list(
    input$regions,
    input$smoothing,
    input$date_range,
    input$prevalence,
    input$type,
    strsplit(date(), " ")[[1]][1:3]
  ))
  output$download_timeseries = downloadHandler(
    filename = paste("timeseries", trunc(runif(1) * 1000), ".png"),
    content = function(file) {
      device = function(..., width, height) {
        grDevices::png(...,
                       width = width,
                       height = height)
      }
      data = process_data(
        data = corona,
        level = input$level,
        type = input$type,
        top_region = input$top_region,
        regions = input$regions,
        date_range = input$date_range
      )
      message(class(data))
      ggsave(
        file,
        plot = plot_timeseries(
          data = data,
          type = input$type,
          smoothing = input$smoothing,
          prevalence = input$prevalence
        ),
        device = device
      )
    }
  )
  output$growthvssize = renderCachedPlot({
    message("output$growthvssize")
    data = process_data(
      data = corona,
      level = input$level,
      type = input$type,
      top_region = input$top_region,
      regions = input$regions,
      date_range = NULL
    )
    print(plot_growthvssize(
      data,
      type = input$type,
      prevalence = input$prevalence
    ))
  },
  cacheKeyExpr = list(
    input$regions,
    input$smoothing,
    input$prevalence,
    input$type,
    strsplit(date(), " ")[[1]][1:3]
  ))
  output$stats = renderDataTable({
    message("output$stats")
    trend_calc(
      data = process_data(
        data = corona,
        level = input$level,
        type = input$type,
        top_region = input$top_region,
        regions = NULL,
        date_range = input$date_range
      )
    )
  }, options = list(order = list(list(5, 'desc'))))
  output$data = renderDataTable({
    message("output$data")
    data = process_data(
      data = corona,
      level = input$level,
      type = input$type,
      top_region = input$top_region,
      regions = input$regions,
      date_range = input$date_range
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
