library(httr)
library(jsonlite)
library(shiny)
library(tidyverse)
library(scales)
library(rsconnect)

covid_dta <- data.frame(Date=as.Date(character()),
                          Country=character(),
                          Confirmed=integer(),
                          Deaths=integer(),
                          stringsAsFactors=FALSE)
countries <- read.csv("countrybasic.csv")[ ,c('Country')]
  
# api call below inspired by 
#https://www.programmableweb.com/news/how-to-access-any-restful-api-using-r-language/how-to/2017/07/21?page=2

for (country in countries) {
    covid_url <- "https://api.covid19api.com/dayone/country/"
    covid_url <- paste(covid_url, country, sep ="")
    #the below call possible due to the httr library
    get_covid <- GET(covid_url)
    get_covid_text <- content(get_covid, "text")
    #the below json conversion possible due to the jsonlite library
    get_covid_json <- fromJSON(get_covid_text, flatten = TRUE)
    get_covid_df <- as.data.frame(get_covid_json)
    get_covid_df[-c(1, 3:8, 11:12)]
    cdata <- get_covid_df[c("Date", "Country", "Confirmed", "Deaths")]
    cdata$Date <- as.Date(paste(cdata$Date))
    covid_dta <- rbind(covid_dta, cdata)
  }

long_df <- covid_dta %>% gather(Type,Value,3:4)
long_df <- na.omit(long_df)
long_df$Value <- as.integer(paste(long_df$Value))

#R shiny inspiration from https://shiny.rstudio.com/tutorial/
ui <- fluidPage(
sidebarPanel(
  selectInput(
    inputId = "country",
    label = "Select Country",
    long_df$Country
  ),
  selectInput(inputId = "type",
              label = "Data to Show:",
              choices=unique(long_df$Type),
              multiple = TRUE)),
mainPanel('COVID in Select Countries',
    plotOutput("chart")),
  position = 'left'
)

server <- function(input, output) {
  output$chart <- renderPlot({
    long_df %>% 
      filter(Country %in% input$country) %>% 
      filter(Type %in% input$type) %>% 
      ggplot(aes(Date, Value)) +
      # info on multiple y variables
      # https://stackoverflow.com/questions/55155912/plot-graph-with-multiple-y-axis-variable-in-r?
      #newreg=e0d0131c5d90481fb841170850c12449
      geom_line(aes(color = Type)) +
      scale_colour_manual(values = c("Deaths" = "red",
                                     "Confirmed" = "blue")) +
      scale_y_continuous(labels = comma,
                         expand = expansion(mult = c(0, 0.1)),
                         limits = c(0, NA)) +
      scale_x_date(expand = expansion(mult = 0))
  })
}

shinyApp(ui = ui, server = server)