source("data_sources.R")
source("data_processing.R")
source("plots.R")

library(ggplot2)
library(directlabels)
library(DT)

options(DT.fillContainer = FALSE)
options(DT.autoHideNavigation = FALSE)

ui <- fluidPage(
  titlePanel(
    paste(
      "Visualizations of new cases (deaths, recovered, tested) per country or state,  updated as of",
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
        "Type (all numbers affected by varying definitions, testing capacity, all likely to be less than or even a fraction of reality; these biases are not even consistent over time)",
        choices = all_types,
        selected = "cases",
        inline = TRUE
      ),
      checkboxInput(
        "prevalence",
        "Convert to prevalence (cases per 100K people)",
        value = FALSE
      ),
      radioButtons(
        "level",
        "Choose admin unit level to visualize (US terminology used for all countries)",
        choices = c("country", "state", "county"),
        selected = "country",
        inline = TRUE
      ),
      selectizeInput("top_region",
                     "",
                     choices = NULL,
                     multiple = FALSE),
      selectizeInput("regions",
                     "",
                     choices =  NULL,
                     multiple = TRUE),
      checkboxInput("smoothing",
                    "Use smoothing (simpler graphs)",
                    value = TRUE)
    ),

    mainPanel(
      tabsetPanel(
        id = "mainPanel-tabsetPanel",
        tabPanel("Time series", plotOutput("timeseries", height = "800px")),
        tabPanel("Growth vs Size", plotOutput("growthvssize", height = "800px")),
        tabPanel("Tables", dataTableOutput("stats")),
        tabPanel("Data", value = "data", dataTableOutput("data")),
        tabPanel(
          "Methods and credits",
          "Data from NYT for US,  https://coronadatascraper.com/ for rest of the  Smoothed with https://stat.ethz.ch/R-manual/R-devel/library/stats/html/supsmu.html after a log transform (because the smoother is locally linear, and in log scale these trends appear locally linear) on the cumulative counts. Trends are computed with a linear model applied to the last 3 days of smoothed data in log scale (on the daily count, which I may reconsider). \"Doubling time\" is an estimate of how many days it takes for cases to double, negative numbers corresponding to decrease or halving time. These choices are reasonable given a visual inspection of the data and the little I know about epidemiology but have not been validated and assume unchanged policies and attitudes in the affected countries, which is hopefully the wrong assumption, plus negligible levels of immunity in the population, which is correct but is bound to change in the future. I do have a background in science, but this has been hastly produced and not peer-reviewed. This analysis is meant to support the view that we are in an exponential phase of disesase spread in most countries and states, that is increase from one day to the next as a percentage is roughly constant. No health care system, let alone a system for tracking and isolating cases, can work more than a few weeks when cases double every 2 or 3 days as we are seeing in several countries as of early March. Only mobilizing a large fraction of the population can work (self-quarantine, social distancing, remote work, canceling gatherings, school closings, travel restrictions etc.). Countries that show a declining number of new cases (for example China, Hong Kong, South Korea as of early March) have applied these population-wide measures. Major caveat is that confirmed cases per day are capped by detection capacity for large outbreaks or in countries run by incompetent people and by censorship. In that case check the trends on number of deaths, which are harder to conceal -- but causes of death may be attributed incorrectly.  Code: https://github.com/piccolbo/covid-19 Feedback: covid19@piccolboni.info"
        )
      )
    )
  ))


server <- function(input, output, session) {
  observeEvent(session$clientData$url_hostname, {
    message(session$clientData$url_hostname)
    if (session$clientData$url_hostname != "127.0.0.1") {
      message("hide tab")
      hideTab(inputId = "mainPanel-tabsetPanel", target = "data")
    }
  })


  current_top_region = reactive({
    if (input$level == "country")
      NULL
    else {
      top_regions = has_subregions[[one_level_up(input$level)]]
      if (input$top_region %in% top_regions) {
        input$top_region
      }
      else
        "United States"
    }
  })

  current_regions = reactive({
    rr = regions_range()
    rr6 = head(rr, 6)
    if (is.null(input$regions)) {
      rr6
    }
    else {
      regions = intersect(input$regions, rr)
      if (length(regions) > 0) {
        input$regions
      }
      else
        rr6
    }
  })


  observeEvent(input$level,
               {
                 if (input$level == "country") {
                   NULL
                 }
                 else{
                   choices = has_subregions[[one_level_up(input$level)]]
                   selected = high_cases_regions(
                     data = process_data(
                       data = corona,
                       level = one_level_up(input$level),
                       type = input$type,
                       top_region = NULL,
                       regions = NULL,
                       date_range = NULL,
                       prevalence = input$prevalence
                     ),
                     1
                   )
                   updateSelectizeInput(
                     session = session,
                     inputId = "top_region",
                     label = "Focus on this specific region",
                     choices =  choices,
                     selected = selected,
                     server = TRUE
                   )
                 }
               })

  observeEvent({
    input$level
    input$top_region
  },
  {
    data = process_data(
      data = corona,
      level = input$level,
      type = input$type,
      top_region = current_top_region(),
      regions = NULL,
      date_range = NULL,
      prevalence = input$prevalence
    )
    choices = sort(unique(data[[input$level]]))
    selected = high_cases_regions(data = data, 12)
    updateSelectizeInput(
      session = session,
      inputId = "regions",
      label = "Regions to show data from (biggest current outbreaks shown, click for more)",
      choices = choices,
      selected = selected,
      server = TRUE
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
        date_range = input$date_range,
        input$prevalence
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
        date_range = input$date_range,
        input$prevalence
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
      date_range = NULL,
      input$prevalence
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
        date_range = NULL,
        input$prevalence
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
      date_range = input$date_range,
      input$prevalence
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
