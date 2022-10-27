
library(tidyverse)     # for data cleaning and plotting
library(lubridate)     # for date manipulation
library(openintro)     # for the abbr2state() function
library(maps)          # for map data
library(ggmap)         # for mapping points on maps
library(gplots)        # for col2hex() function
library(RColorBrewer)  # for color palettes
library(sf)            # for working with spatial data
library(leaflet)       # for highly customizable mapping
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(gganimate)     # for adding animation layers to ggplots
library(transformr)    # for "tweening" (gganimate)
library(gifski)        # need the library for creating gifs but don't need to load each time
library(ggimage)
library(readxl)
library(forcats)
library(patchwork)
library(tidytext)
library(shinyWidgets)
library(shinythemes)
library(rsconnect)
library(scales)

tsf_data_clean <- read_csv("tsf_data_clean.csv")
tsf_data_lbs <- read_csv("tsf_data_lbs.csv")

tsf_tidy<-
  tsf_data_clean %>% 
  select(!("Email Address":"Phone Number")) %>%
  select(!("First Name":"Age")) %>%
  select(!("Notes")) %>% 
  rename("timestamp"="Timestamp") %>% 
  rename("visited_this_year"="Have you visited us before?") %>%
  rename("first_visit_this_month"="Is this your first visit this month?") %>%
  rename("race_ethnicity"="Demographics Information") %>% 
  rename("household_size"="Household Size") %>% 
  rename("n_youth"="Children Under 18 Years") %>%
  rename("n_adult"="Adults 18 - 64 Years") %>% 
  rename("n_senior"="Seniors 65 & Older") %>% 
  rename("zip"="Zip Code") %>% 
  rename("location"="Mobile Foods Distribution Location") %>% 
  mutate(timestamp=as.Date(timestamp, format="%m/%d/%Y"))


food_lbs<-
  tsf_data_lbs %>% 
  mutate(timestamp=as.Date(date, format="%m/%d/%Y"))

ui <- fluidPage(theme=shinytheme("readable"),
  
  fluidRow(
    column(11, h1("Sanneh Foundation Nutritional Services")),
    column(1,
           mainPanel(img(src = "sanneh-logo.png", align="right", height = 127, width = 189)),
           div(style = "font-size: 10px; padding: 14px"))),
  
  
  fluidRow(
    column(3,
           wellPanel(
                dateRangeInput(inputId="timestamp", label="Date Range",
                            min="2020-04-01", max="2022-04-30",
                            sep=""),
                pickerInput(inputId="location", label="Food Distribution Location",
                            multiple=TRUE, options = list(`actions-box` = TRUE), choices=c(
                              "Afton View", "American Indian Family Center", "Casa de Esperanza",
                              "Cash Money Ryders", "Cedar Riverside Apartments", "Chaska", "Central Hi Rise", "Chaska",
                              "Commonbond (Skyline)", "Como Apartments", "Como/District 1",
                              "Como/District 10", "Conway", "Corcoran Park", "Crestview Apartments", "DHH", 
                              "Elder's Lodge", "Fort Road Flats", "Grace Church", "Hallie Q Brown", "Hancock",
                              "Harding High School (East Side)", "Kramer", "La Vina", "Latino Outreach Program",
                              "Listening House of St. Paul", "McDonough Homes", "Merrick Community Services",
                              "Mississippi", "Mt Airy", "Mt. Olivet Church (Rondo/Frogtown)",
                              "NE Seniors", "Nepali Community Center", "Other", "Overcomer's Church", "Partnership Academy",
                              "Pathways on the Park", "Piercing Faith Church",
                              "Pilgrim Baptist Church", "Riverlife Church", "Roosevelt Homes", "Roseville Lutheran",
                              "Salem Lutheran (North Minneapolis)", "Sanctuary Church (North Mpls)",
                              "Sears Parking Lot", "Shakopee", "Shamrock Plaza", "St. Paul Mosque")),
                
                
                submitButton(text="View Statistics")),
                
                
                tableOutput(outputId="cumnums"),
                tableOutput(outputId="cumlbs")),
                
  column(9,
      "", plotOutput(outputId="ageplot"))),

fluidRow(
  column(12,
      "", plotOutput(outputId="demoplot"))))

server <- function(input, output) {
  
  output$cumnums<-renderTable({
    tsf_tidy %>%
      filter(timestamp >= input$timestamp[1] & 
               timestamp <= input$timestamp[2]) %>%
      filter(location %in% input$location) %>%
      drop_na(household_size) %>% 
      summarize(nhouseholds=n(),
                nindividuals=sum(household_size)) %>%
      slice_max(nindividuals, n=1) %>% 
      transmute("Households Served"=formatC(nhouseholds, format="d", big.mark=","),
                "Individuals Served"=formatC(nindividuals, format="d", big.mark=","))
    
    
  })
  
  output$cumlbs<-renderTable({
    food_lbs %>% 
      filter(timestamp >= input$timestamp[1] & 
             timestamp <= input$timestamp[2]) %>%
      summarize(cumlbs=sum(lbs)) %>%
      slice_max(cumlbs, n=1) %>% 
      transmute("Food Pounds"=formatC(cumlbs, format="d", big.mark=","))
      
      
    
  })
  
  output$ageplot<-renderPlot({
    tsf_tidy %>%
      pivot_longer(cols=c("n_youth", "n_adult", "n_senior"),
                   names_to="age",
                   values_to="n_age") %>%
      mutate(age=fct_recode(age, "Youth 0-17"="n_youth", "Adults 18-64"="n_adult",
                        "Seniors 65+"="n_senior")) %>% 
      drop_na(n_age) %>%
      group_by(timestamp, location, age) %>% 
      summarize(n_served_age=sum(n_age)) %>%
      ungroup() %>% 
      group_by(age) %>% 
      filter(timestamp >= input$timestamp[1] & 
               timestamp <= input$timestamp[2]) %>%
      filter(location %in% input$location) %>%
      mutate(total_served_age=cumsum(n_served_age)) %>%
      slice_max(total_served_age, n=1) %>% 
      ggplot(aes(x=fct_relevel(age, "Youth 0-17", "Adults 18-64", "Seniors 65+"),
                 y=total_served_age, fill=age))+
      geom_col() +
      geom_text(aes(x=age, y=total_served_age, label=scales::comma(total_served_age),
                    group=age),position=position_dodge(width = 0.9), vjust=-0.20)+
      scale_y_continuous(breaks=waiver(), labels=scales::comma)+
      scale_fill_manual(labels=c("Youth 0-17", "Adults 18-64", "Seniors 65+"),
                        values=c("#000b8c", "#e08650", "#62a70f"))+
      labs(x="", y="", fill="Age", 
           title="Age Distribution")+
      theme_hc()+
      theme(legend.position = "none",
            plot.title.position = "plot",
            plot.title=element_text(family="serif", face="bold"))
  })
  
  
  output$demoplot<-renderPlot({
   tsf_tidy %>% 
    group_by(race_ethnicity) %>% 
     filter(timestamp >= input$timestamp[1] & 
            timestamp <= input$timestamp[2]) %>%
     filter(location %in% input$location) %>% 
    drop_na(household_size) %>% 
     mutate(total_served_race=cumsum(household_size)) %>%
     slice_max(total_served_race, n=1) %>% 
     ggplot(aes(y=fct_reorder(race_ethnicity, total_served_race),
                x=total_served_race, fill=race_ethnicity))+
     geom_col()+
     geom_text(aes(y=race_ethnicity, x=total_served_race, label=scales::comma(total_served_race)),
               position=position_dodge(width = 0.9), hjust=-0.20)+
     scale_x_continuous(breaks=waiver(), labels=scales::comma)+
     scale_fill_manual(values = c("#000B8C", "#F283B6", "#62A70F", "#E08650",
                                  "#B150C5", "#F5C65D", "#00A1E0", "#C91F3B",
                                  "#FB6107", "#55828B", "#C9EDDC"))+
     labs(x="", y="", fill="Race/Ethnicity", 
           title="Demographic Distribution")+
     theme_hc()+
     theme(legend.position="none",
           plot.title.position = "plot",
           plot.title=element_text(family="serif", face="bold"))
  }
   )}
  

shinyApp(ui = ui, server = server)