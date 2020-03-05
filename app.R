#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(coronavirus)
library(ggplot2)
library(dplyr)
library(directlabels)
library(devtools)
#devtools::install_github("tidyverse/tidyr")
library(tidyr)
# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Daily cases per country, confirmed"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         sliderInput("min_cases",
                     "Minimum number of cases in one day for a country to be displayed",
                     min = 5,
                     max = 100,
                     value = 10),
         sliderInput("span",
                     "Amount of smoothing to apply",
                     min = 0,
                     max = 1,
                     value = 0.5)
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$distPlot <- renderPlot({
     data = coronavirus %>%
       group_by(Country.Region, type, date) %>%
       summarise(cases = sum(cases)) %>%
       filter(cases > 0, max(cases)>input$min_cases, type == "confirmed") %>%
       arrange(date)
     data = pivot_longer(data = pivot_wider(data = data, names_from =Country.Region, values_from = cases, values_fill = 0), cols = unique(data$Country.Region) , names_to = "Country.Region", values_to = "cases")
     if(input$span == 0) {
       main_geom = geom_line()}
     else {
       main_geom = geom_smooth(se = FALSE, span = input$span)}
     ggplot(
       data = data,
       mapping = aes(
         x=date,
         y=cases,
         label = Country.Region,
         color=Country.Region)) +
       main_geom +
       geom_dl(method = "last.points")+
       scale_y_log10()
     })
}

# Run the application
shinyApp(ui = ui, server = server)



