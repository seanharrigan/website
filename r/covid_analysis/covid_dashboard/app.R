library(shiny)
library(tidyverse)
library(spData) # For getting spatial data
library(sf) # For preserving spatial data
library(leaflet) # For making maps
library(DT) # For making tables
library(plotly)
library(RColorBrewer)
library(ggpubr)
library(mapcan)
library(patchwork)
library(gghighlight)
library(ggpubr)
library(mapcan)
library(plotly)
library(bslib)

# setwd('/Users/seanharrigan/Desktop/Coding/web/personal_site/r/covid_analysis/covid_dashboard/')

cov <- read_csv("covid19-canada.csv") %>% 
  rename(pr_english = prname, year = reporting_year) %>% 
  filter(pr_english != "Repatriated travellers") %>% 
  mutate(pr_english = recode(pr_english, Canada = 'All'))

vax <- read_csv('vaccination.csv') %>% 
  mutate(year = lubridate::year(week_end)) %>% 
  rename(pr_english = prename)

pr_geographic <- mapcan(boundaries = province,
                        type = standard)



#Define UI
ui <- fluidPage(

  theme = "styles.css",

  # Application title
  # Top image
  #HTML('<center><img src="https://i.ibb.co/1zSfPP2/ramen-3950790-640.png" width="100"></center>'),
  img(class = 'topimg', src = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/SARS-CoV-2_without_background.png/1200px-SARS-CoV-2_without_background.png"),

  # Application title
  h1("The COVID-19 pandemic at a glance:"),
  h3("Canadian data from 2020-2023"),

  fluidRow(
    fluidRow(

      column(6,
             # Province Menu
             selectInput("pr_english", "Province",
                         choices =
                         c("All", "Alberta", "British Columbia", "Manitoba",
                           "New Brunswick", "Newfoundland and Labrador", "Northwest Territories",
                           "Nova Scotia", "Nunavut", "Ontario", "Prince Edward Island", "Quebec",
                           "Saskatchewan", "Yukon")
                           #selected = 'All')
             )
          ),


      column(6,
             # Year menu
             selectInput("year", "Year",
                         c('All',
                           unique(cov$year) %>%
                             sort()
                         )

      )
    )
  ),

  fluidRow(
    column(6,
           column(12,
             # Bar Chart
             plotOutput("brandBar", height=200)
           ),
           column(12,
              # Bar Chart
              plotOutput("vaxPlot", height=200)
            )

    ),

    column(6,
           # Map
           plotOutput("map", height = 450)

    )

  ),

  fluidRow(
    # Table
    dataTableOutput("table")
  )

))




# Define server logic
server <- function(input, output) {

  
  # Create bar chart of infections
  output$brandBar <- renderPlot({
    
    # Filter data based on selected Province
    if (input$pr_english != "All") {
      cov <- filter(cov, pr_english != 'All')
    }
    
    cov <- cov %>% 
      mutate(colour = ifelse(pr_english == input$pr_english, 'blue', 'grey'), 
             opacity = ifelse(pr_english == input$pr_english, 0.85, 0.35))
    

    # Filter data based on selected Year
    if (input$year != "All") {
      cov <- filter(cov, year == input$year) %>% 
        droplevels()
    }
    
    validate (
      need(nrow(cov) > 0, "No data. Please make another selection." )
    )
    
    cov %>%  
      as_tibble() %>% 
      select(date, pr_english, cases = avgcases_last7, colour, opacity) %>% 
      group_by(date, pr_english, colour, opacity) %>% 
      summarise(cases = sum(cases)) %>% 
      ungroup() %>% 
      ggplot(aes(x = date, y = cases, color = pr_english, alpha = opacity)) +
      geom_line(size = 1) +
      gghighlight(opacity > 0.5, use_direct_label = FALSE) + 
      theme_minimal() + 
      labs(x = NULL, 
           y = 'Infections',
           color = NULL,
           alpha = NULL,
           title = 'COVID-19 Infections stratified by province') + 
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none", 
            axis.title.y = element_text(size = 10, 
                                        margin = margin(r = 10))) + 
      scale_x_date(breaks = "2 month", date_labels = "%b, %y",
                   minor_breaks = '1 month',
                   expand = c(0, 0)) 
    

  })
  
  
  #Create bar chart of vaccination
  output$vaxPlot <- renderPlot({
    
    # Filter data based on selected Province
    if (input$pr_english != "All") {
      vax1 <- filter(vax, pr_english == input$pr_english)
    } else {
      vax1 <- filter(vax, pr_english == 'Canada')
    }
    
    
    # Filter data based on selected Year
    if (input$year != "All") {
      vax2 <- filter(vax1, year == input$year) %>% 
        droplevels()
    } else {
      vax2 <- vax1
    }
    
    validate (
      need(nrow(vax2) > 0, "No data. Please make another selection." )
    )
    
    vax2 %>% 
      select(prov = pr_english, Partially = proptotal_partially, 
             Fully = proptotal_fully, date = week_end) %>% 
      #filter(prov ==  'Canada') %>% 
      mutate(Fully = as.numeric(ifelse(Fully == '<0.1', 0, Fully))) %>% 
      #filter(date >= '2021-01-01' & date <= '2021-12-31')  %>% 
      pivot_longer(c(Partially, Fully), names_to = 'vax', values_to = 'prop') %>% 
      ggplot(aes(x=date, y = prop, fill = vax)) +
      geom_area(alpha = 0.5) + 
      theme_minimal() + 
      labs(x = NULL, 
           y = '% Vaccinated',
           fill = NULL,
           title = NULL) + 
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.y= element_text(color = 'black', size = 10, margin = margin(r  = 10)),
            legend.position = c(0.2, 0.88),
            legend.direction="horizontal",
            legend.title = element_text(size = 9)) +
      scale_x_date(breaks = "2 month", date_labels = "%b, %y",
                   minor_breaks = '1 month',
                   expand = c(0, 0)) +
      guides(fill = guide_legend(title.position = "top")) 

    
  })
  
  
  # Create world map
  output$map <- renderPlot({

    # Filter data based on selected Province
    if (input$pr_english == "All") {
      
      if (input$year != "All") {
      
        cov1 <-
          filter(cov, year == input$year) %>%
          droplevels() %>% 
          select(pr_english, year, rate = ratedeaths_last14) %>% 
          filter(pr_english !='All') %>% 
          filter(pr_english != 'Repatriated travellers') %>% 
          droplevels() %>% 
          group_by(pr_english) %>% 
          summarise(rate = mean(rate, na.rm = T)) %>% 
          left_join(., pr_geographic, by = c('pr_english')) 
        
        cov1 %>%
          ggplot(aes(x = long, y = lat, group = group, fill = rate)) +
          geom_polygon() +
          coord_fixed() +
          theme_mapcan() + 
          guides(alpha = FALSE) + 
          scale_fill_viridis_c(name = "Mortality rate / 100K", 
                               limits = c(0, 4.05)) +
          guides(fill = guide_colourbar(title.position = "top")) + 
          theme(legend.position = c(0.7, 0.75),
                legend.direction="horizontal",
                legend.title = element_text(size = 9))

      } else  {

      #if (input$year == "All") {
        
        cov1 <-
          cov %>% 
          select(pr_english, year, rate = ratedeaths_last14) %>% 
          filter(pr_english !='All') %>% 
          filter(pr_english != 'Repatriated travellers') %>% 
          droplevels() %>% 
          group_by(pr_english) %>% 
          summarise(rate = mean(rate, na.rm = T)) %>% 
          left_join(., pr_geographic, by = c('pr_english')) 
        
        cov1 %>%
          ggplot(aes(x = long, y = lat, group = group, fill = rate)) +
          geom_polygon() +
          coord_fixed() +
          theme_mapcan() + 
          guides(alpha = FALSE) + 
          scale_fill_viridis_c(name = "Mortality rate / 100K", 
                               limits = c(0, 4.05)) +
          guides(fill = guide_colourbar(title.position = "top")) + 
          theme(legend.position = c(0.7, 0.75),
                legend.direction="horizontal",
                legend.title = element_text(size = 9))

      }
      
    } else {


      if (input$year != "All") {
        cov1 <- filter(cov, year == input$year) %>%
          select(pr_english, year, rate = ratedeaths_last14) %>% 
          filter(pr_english !='All') %>% 
          filter(pr_english != 'Repatriated travellers') %>% 
          droplevels() %>% 
          group_by(pr_english, year) %>% 
          summarise(rate = mean(rate, na.rm=T)) %>% 
          left_join(., pr_geographic, by = c('pr_english')) %>% 
          droplevels() %>%
          mutate(opacity = ifelse(pr_english == input$pr_english, 0.85, 0.35)) 
        
        cov1 %>%
          ggplot(aes(x = long, y = lat, group = group, fill = rate, alpha = opacity)) +
          geom_polygon() +
          coord_fixed() +
          theme_mapcan() + 
          guides(alpha = FALSE) + 
          scale_fill_viridis_c(name = "Mortality rate / 100K", 
                               limits = c(0, 4.05)) +
          # ggtitle("Canadian Population by Province") + 
          guides(fill = guide_colourbar(title.position = "top")) + 
          theme(legend.position = c(0.7, 0.75),
                legend.direction="horizontal",
                legend.title = element_text(size = 9))

        
      } else {
      
     # if (input$year == "All") {
        
        cov1 <- cov %>% 
          select(pr_english, year, rate = ratedeaths_last14) %>% 
          filter(pr_english !='All') %>% 
          filter(pr_english != 'Repatriated travellers') %>% 
          droplevels() %>% 
          group_by(pr_english) %>% 
          summarise(rate = mean(rate, na.rm=T)) %>% 
          left_join(., pr_geographic, by = c('pr_english')) %>% 
          mutate(opacity = ifelse(pr_english == input$pr_english, 0.85, 0.35)) 
        
        cov1 %>%
          ggplot(aes(x = long, y = lat, group = group, fill = rate, alpha = opacity)) +
                   geom_polygon() +
                   coord_fixed() +
                   theme_mapcan() +
                   guides(alpha = FALSE) +
                   scale_fill_viridis_c(name = "Mortality rate / 100K", 
                                        limits = c(0, 4.05)) +
                   # ggtitle("Canadian Population by Province") +
                   guides(fill = guide_colourbar(title.position = "top")) +
                   theme(legend.position = c(0.7, 0.75),
                         legend.direction="horizontal",
                         legend.title = element_text(size = 9),
                         plot.title = element_text(hjust = 0.5))

      }
      
    }

  
  })  
  
# Create data table
output$table <- renderDataTable({
  
  # Filter data based on selected Province
  if (input$pr_english != "All") {
    cov <- filter(cov, pr_english == input$pr_english)
  }
  
  # Filter data based on selected Country
  if (input$year != "All") {
    cov <- filter(cov, year == input$year)
  }
  
  # Hide table when user has filtered out all data
  validate (
    need(nrow(cov) > 0, "No data. Please make another selection.")
  )
  
  cov %>% select(Province = pr_english, Date = date, Year = year,
                `Average infections last 7 days` = avgcases_last7,
                 `Mortality Rate` = ratedeaths_last14) 
  
})
  
}

# Run the application 
shinyApp(ui = ui, server = server)



