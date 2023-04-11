library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(htmltools)
library(shiny)
library(shinydashboard)
library(knitr)


# Read the csv files
license_applications <- read.csv("data.csv")
zipcodes <- read.csv("zipcodes.csv")

# Rename and change types of columns
names(license_applications) <-
  tolower(gsub("[.]", "_", names(license_applications)))
names(zipcodes) <- tolower(gsub("[.]", "_", names(zipcodes)))
zipcodes <- rename(zipcodes, zip = zip_code, city = common_cities)
zipcodes$zip <- as.character(zipcodes$zip)

# Duplicate a subset of the data
df_orig <- license_applications %>%
  select(application_id:end_date,
         license_category,
         application_category,
         city:latitude)

zipcodes <- select(zipcodes, -type)

df <- df_orig

# Limit the analysis of only businesses in from NYC with an valid issued licence
df <- df %>%
  filter(state == "NY") %>%
  filter(status == "Issued") %>%
  mutate(contact_phone = case_when(contact_phone == "" ~ NA_character_,
                                   TRUE ~ contact_phone)) %>%
  mutate(end_date = mdy(end_date),
         start_date = mdy(start_date))

# Join tables (Why? To get accurate/uniform names for the county column)
df <- df %>%
  left_join(zipcodes, by = "zip") %>%
  select(-city.x) %>%
  rename(county = county, city = city.y) %>%
  filter(!duplicated(application_id))

# Create a function that will subset the table based on license category
getSubset <- function(category) {
  set <- df %>%
    filter(
      county %in% c(
        "Queens County",
        "Bronx County",
        "Kings County",
        "Richmond County",
        "New York County"
      )
    ) %>%
    filter(license_category == category) %>%
    mutate(year = year(end_date)) %>%
    group_by(county, year) %>%
    summarise(count = n()) %>%
    filter(year >= 2014)
  
  set
}

#------------------------------------------------------------------------------#

# This is the plot for the total licences issued over time for each 5 boroughs
# Prepare the data
p <- df %>%
  filter(
    county %in% c(
      "Queens County",
      "Bronx County",
      "Kings County",
      "Richmond County",
      "New York County"
    )
  ) %>%
  mutate(year = year(end_date)) %>%
  group_by(county, year) %>%
  summarise(count = n())

# Prepare the plot
p <- p %>% plot_ly(
  x = ~ county,
  y = ~ count,
  color = ~ county,
  frame = ~ year,
  hoverinfo = 'y',
  type = 'bar'
)

overview <- df %>%
  filter(
    county %in% c(
      "Queens County",
      "Bronx County",
      "Kings County",
      "Richmond County",
      "New York County"
    )
  ) %>%
  mutate(year = year(end_date)) %>%
  group_by(year) %>%
  summarise(count = n())

overview <- overview %>% plot_ly(
  x = ~ year,
  y = ~ count,
  hoverinfo = 'y',
  type = 'scatter',
  mode = 'lines+markers'
)

#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#

# Create the UI ----
ui <- dashboardPage(
  dashboardHeader(title = "NYC License Application"),
  dashboardSidebar(sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("th")),
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    )
  )),
  dashboardBody(tabItems(
    tabItem(
      tabName = "dashboard",
      fluidRow(
        column(width = 4,
               box(
                 width = 12,
                 selectInput(
                   "year",
                   label = "Year (MAP)",
                   choices = c("2022", "2021", "2020", "2019",
                               "2018", "2017", "2016", "2015", 
                               "2014"),
                   selected = "2022"
                 )
               ))
        ,
        column(width = 4,
               box(
                 width = 12,
                 selectInput(
                   "county",
                   label = "County (MAP)",
                   choices = c(
                     "New York County",
                     "Kings County",
                     "Queens County",
                     "Richmond County",
                     "Bronx County"
                   ),
                   selected = "New York County"
                 )
               )),
        column(width = 4,
               box(
                 width = 12,
                 selectInput(
                   "type",
                   label = "Choose the type of license (BOTH)",
                   choices = c(
                     "Home Improvement Contractor",
                     "General Vendor",
                     "Pedicab Driver",
                     "Laundries",
                     "Tobacco Retail Dealer",
                     "Home Improvement Salesperson",
                     "Secondhand Dealer - General",
                     "Electronics Store",
                     "Sightseeing Guide",
                     "Auctioneer",
                     "Games of Chance",
                     "Gaming Cafe",
                     "Sidewalk Cafe"
                     
                   ),
                   selected = "Home Improvement Contractor"
                 )
               ))
      ),
      fluidRow(column(
        width = 12,
        box(title = "Location of businesses by county and year",
            leafletOutput("map")),
        box(title = "Total number selected type of licences issued from 2014 - 2023",
            plotlyOutput("graph"))
      )),
      fluidRow(column(width = 12,
                      box(
                        title = "Total number of all types of licenses issued from 2000 - 2023",
                        width = 12,
                        plotlyOutput("overview")
                      )))
      
    )
  ))
)


# Create the server ----
server <- function(input, output) {
  output$graph <- renderPlotly({
    plot_ly(
      data = getSubset(input$type),
      x = ~ county,
      y = ~ count,
      color = ~ county,
      frame = ~ year,
      hoverinfo = 'y',
      type = 'bar'
    ) %>% 
      layout(showlegend = FALSE)
  })
  
  output$map <- renderLeaflet({
    data <- df %>%
      filter(
        license_category == input$type,
        year(end_date) == input$year,
        county == input$county
      )
    
    if(nrow(data) == 0) {
      data %>%
        leaflet() %>%
        addTiles() %>%
        setView(lng = -73.935242,
                lat = 40.730610 ,
                zoom = 10)
    } else {
      data %>%
        leaflet() %>%
        addTiles() %>%
        setView(lng = -73.935242,
                lat = 40.730610 ,
                zoom = 10) %>%
        addMarkers(~ longitude, ~ latitude, label = ~ htmlEscape(business_name))
    }
  })
  
  output$overview <- renderPlotly({
    df %>%
      filter(
        county %in% c(
          "Queens County",
          "Bronx County",
          "Kings County",
          "Richmond County",
          "New York County"
        )
      ) %>%
      mutate(year = year(end_date)) %>%
      group_by(year) %>%
      summarise(count = n()) %>%
      plot_ly(
        x = ~ year,
        y = ~ count,
        hoverinfo = 'y',
        type = 'scatter',
        mode = 'lines+markers'
      )
  })
  
  # output$markdown <- renderUI({
  #   #withMathJax(HTML(markdown::markdownToHTML(knit('Assignment1.Rmd'),  fragment.only=TRUE))
  #   #includeHTML("Assignment1.html")
  #   withMathJax(HTML(readLines(rmarkdown::render(input = "Assignment1.rmd",
  #                                                output_format = rmarkdown::html_fragment(),
  #                                                quiet = TRUE
  #   ))))
  #})
}

# Run the app ----
shinyApp(ui = ui, server = server)

