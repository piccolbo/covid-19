source("data_sources.R")
source("data_processing.R")
source("plots.R")

library(ggplot2)
library(directlabels)
library(DT)

options(DT.fillContainer = FALSE)
options(DT.autoHideNavigation = FALSE)
options(shiny.reactlog = TRUE)

ui <- fluidPage(titlePanel(
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
    selectizeInput(
      "regions",
      label = "Regions to show data from (biggest current outbreaks shown, click for more)",
      choices =  regions_at_level(data = corona, level = "country"),
      selected = high_cases_regions(
        data = corona,
        level = "country",
        type = "cases",
        top_region = NULL,
        FALSE,
        n = 12
      ),
      multiple = TRUE
    ),
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
        "Data from NYT for US,  JHU for the rest of the world. Smoothed with https://stat.ethz.ch/R-manual/R-devel/library/stats/html/supsmu.html after a log transform (because the smoother is locally linear, and in log scale these trends appear locally linear) on the cumulative counts. Trends are computed with a linear model applied to the last 3 days of smoothed data in log scale (on the daily count, which I may reconsider). \"Doubling time\" is an estimate of how many days it takes for cases to double, negative numbers corresponding to decrease or halving time. These choices are reasonable given a visual inspection of the data and the little I know about epidemiology but have not been validated and assume unchanged policies and attitudes in the affected countries, which is hopefully the wrong assumption, plus negligible levels of immunity in the population, which is correct but is bound to change in the future. I do have a background in science, but this has been hastly produced and not peer-reviewed. This analysis is meant to support the view that we are in an exponential phase of disesase spread in most countries and states, that is increase from one day to the next as a percentage is roughly constant. No health care system, let alone a system for tracking and isolating cases, can work more than a few weeks when cases double every 2 or 3 days as we are seeing in several countries as of early March. Only mobilizing a large fraction of the population can work (self-quarantine, social distancing, remote work, canceling gatherings, school closings, travel restrictions etc.). Countries that show a declining number of new cases (for example China, Hong Kong, South Korea as of early March) have applied these population-wide measures. Major caveat is that confirmed cases per day are capped by detection capacity for large outbreaks or in countries run by incompetent people and by censorship. In that case check the trends on number of deaths, which are harder to conceal -- but causes of death may be attributed incorrectly.  Code: https://github.com/piccolbo/covid-19 Feedback: covid19@piccolboni.info"
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

  top_region_choices = reactive({
    if (input$level == "country")
      c()
    else {
      top_regions = has_subregions[[one_level_up(input$level)]]
    }
  })

  current_top_region = reactive({
    if (input$level == "country")
      NULL
    else {
      if (input$top_region %in% top_region_choices()) {
        input$top_region
      }
      else
        intersect(
          high_cases_regions(
            data = corona,
            level = one_level_up(input$level),
            type = input$type,
            top_region = NULL,
            prevalence = input$prevalence,
            n = 100
          ),
          top_region_choices()
        )[1]
    }
  })

  #top_region selector
  observeEvent(input$level,
               {
                 if (input$level == "country") {
                   updateSelectizeInput(
                     session = session,
                     inputId = "top_region",
                     label = "",
                     choices =  NULL,
                     selected = NULL,
                     server = TRUE
                   )
                 }
                 else{
                   choices = top_region_choices()
                   selected = current_top_region()
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

  region_choices = reactive({
    if (input$level == "country")
      regions_at_level(data = corona, level = "country")
    else
      subregions_of(data = corona,
                    level = input$level,
                    region = current_top_region())
  })

  current_regions = reactive({
    regions = input$regions
    #drop regions not in current range
    if(!all(regions %in% region_choices())||length(regions) == 0) #presumable change of level
      regions =   high_cases_regions(
        data = corona,
        level = input$level,
        type = input$type,
        top_region =
          current_top_region(),
        prevalence = input$prevalence,
        n = 12
      )
    regions
  })


  #regions selector
  observeEvent({
    list(input$level,
         current_top_region())
  },
  {
    updateSelectizeInput(
      session = session,
      inputId = "regions",
      label = "Regions to show data from (biggest current outbreaks shown, click for more)",
      choices = region_choices(),
      selected = current_regions(),
      server = TRUE
    )
  })

  output$timeseries = renderCachedPlot({
    message("output$timeseries")
    if (!is.null(current_regions())) {
      data = process_data(
        data = corona,
        level = input$level,
        type = input$type,
        top_region = current_top_region(),
        regions = current_regions(),
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
    input$level,
    input$type,
    current_regions(),
    current_top_region(),
    input$smoothing,
    input$date_range,
    input$prevalence,
    strsplit(date(), " ")[[1]][1:3]
  ))

  output$growthvssize = renderCachedPlot({
    message("output$growthvssize")
    data = process_data(
      data = corona,
      level = input$level,
      type = input$type,
      top_region = current_top_region(),
      regions = current_regions(),
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
    input$level,
    input$type,
    current_top_region(),
    current_regions(),
    input$prevalence,
    strsplit(date(), " ")[[1]][1:3]
  ))

  output$stats = renderDataTable({
    message("output$stats")
    trend_calc(
      data = process_data(
        data = corona,
        level = input$level,
        type = input$type,
        top_region = current_top_region(),
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
      top_region = current_top_region(),
      regions = current_regions(),
      date_range = input$date_range,
      input$prevalence
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
