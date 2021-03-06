library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(gitlink)
library(tidyverse)
library(leaflet)
library(viridis)
library(htmltools)
library(lubridate)
library(ggtext)

# Pull the data on load
confirmed_cases_covid19_world <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths_covid19_world <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recoveries_covid19_world <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")


####

confirmed_cases_covid19_US <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
deaths_covid19_US <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

# confirmed_cases_covid19_US %>%
#   select(-c("UID" ,"iso2","iso3","code3", "FIPS","Admin2",Combined_Key  )) %>%
#   rbind(confirmed_cases_covid19_world)



# make the data in a long format
confirmed_long <- confirmed_cases_covid19_world %>% 
  pivot_longer( -c(`Province/State`, `Country/Region`, Lat, Long), names_to = "date", values_to = "count_confirmed")


death_long <- deaths_covid19_world %>% 
  pivot_longer( -c(`Province/State`, `Country/Region`, Lat, Long), names_to = "date", values_to = "count_dead")



recoveries_long <- recoveries_covid19_world %>% 
  pivot_longer( -c(`Province/State`, `Country/Region`, Lat, Long), names_to = "date", values_to = "count_recovered")




#merge into a single data frame
covid19_df <- confirmed_long %>% 
  left_join(death_long) %>%
  left_join(recoveries_long)%>%
  na.omit() %>%
  mutate(count = count_confirmed-(count_dead+count_recovered)) %>%
  mutate(date = as.Date(as.character(date), format = '%m/%d/%y'))

# remove extra data frames for memory
rm(confirmed_long, death_long, recoveries_long, confirmed_cases_covid19, deaths_covid19, recoveries_covid19)



server <- function(input, output) {
  
  # Number In box of those recovered.
  output$value_recovered <- shinydashboard::renderValueBox({
    req(input$date_to_map)
    
    recovered <- covid19_df %>%
      filter(date == input$date_to_map) %>%
      group_by(`Province/State`, `Country/Region`) %>%
      summarise(recovered = max(count_recovered,na.rm = T))

    
    valueBox(
      value = prettyNum(sum(recovered$recovered),big.mark = ',') , 
      subtitle = "Recovered", 
      color = "olive", width = 4
    )
  })
  
  # Big Number of Deaths
  output$value_deaths <- renderValueBox({
    req(input$date_to_map)
    
    dead <- covid19_df %>%
      filter(date == input$date_to_map) %>%

      group_by(`Province/State`) %>%

      summarise(dead = max(count_dead, na.rm = T))
    
    
    valueBox(
      value = prettyNum(sum(dead$dead),big.mark = ',') , 
      subtitle = "Deaths", 
      color = "red",
      width = 4
    )
  })
  
  # Big Number Active Cases
  output$value_active_cases <- renderValueBox({
    req(input$date_to_map)
    
    
    active <- covid19_df %>%
      filter(date == input$date_to_map) %>%

      group_by(`Province/State`, `Country/Region`) %>%
      summarise(active = max(count, na.rm = T))
    
    
    valueBox(
      value = prettyNum(sum(active$active),big.mark = ',') , 
      subtitle = "Active Cases", 
      color = "yellow",
      width = 4
    )
  })
  
  # Big number of total Cases
  output$value_total_cases <- renderUI({
    req(input$date_to_map)
    
    
    total <- covid19_df %>%
      filter(date == input$date_to_map) %>%

      group_by(`Province/State`, `Country/Region`) %>%
      summarise(total = max(count_confirmed, na.rm = T))
    
    
    valueBox(
      value = prettyNum(sum(total$total),big.mark = ',') , 
      subtitle = "Total Cases", 
      color = "blue",
      width = 12
    )
  })
  
  output$active_cases_map <- renderLeaflet({
    req(input$date_to_map)
    
    date_selected = input$date_to_map
    
    pal <- colorNumeric("magma", domain = log10(c(1, max(covid19_df$count))))
    
    
    
    map <- covid19_df %>%
      filter(date == date_selected) %>%
      leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addCircleMarkers(
        lng = ~Long, 
        lat = ~Lat,
        radius = ~(log10(count)), 
        color = ~pal(log10(count)), 
        stroke = FALSE, fillOpacity = 0.5
        
      )
  })
 
  
  output$cum_cases_map <- renderLeaflet({
    req(input$date_to_map)
    
    date_selected = input$date_to_map
    
    
    df <- covid19_df %>%
      arrange(date) %>%
      group_by(`Province/State`)%>%
      mutate(cum_cases =  cumsum(count)) %>%
      ungroup
    
    pal <- colorNumeric("magma", domain = log10(c(1, max(df$cum_cases))))
    
    
    map <- df %>%
      filter(date == date_selected) %>%
      leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      #addLegend(title = "cumulative Cases", pal=pal, value = ~df %>% filter(cum_cases >=1) %>% pull(cum_cases))%>%
      addCircleMarkers(
        lng = ~Long, 
        lat = ~Lat,
        radius = ~(log10(cum_cases)), 
        color = ~pal(log10(cum_cases)), 
        stroke = FALSE, fillOpacity = 0.5,
        popup = ~htmlEscape(`Province/State`) 
      )
      
  })
  
  
  output$date_selection <- renderUI({
    min_date = min(covid19_df$date)
    max_date = max(covid19_df$date)
    
    sliderInput("date_to_map", 
                "Date:",
                width="100%",
                min = min_date,
                max = max_date, 
                value = max_date)
  })
  
  
  # Daily Change in new diagnosed, and recovered
  output$daily_change_plot <- renderPlot({
    req(input$date_to_map)
    
    covid19_df %>%
      mutate(count_new_cases = count_confirmed-lag(count_confirmed)) %>%
      mutate(count_new_recovered = count_recovered- lag(count_recovered))%>%
      drop_na(count_new_cases, count_new_recovered)%>%
      select(`Province/State`, `Country/Region`, Lat, Long, date, count_new_cases, count_new_recovered) %>%
      pivot_longer( -c(`Province/State`, `Country/Region`, Lat, Long, date), 
                    names_to = "status", values_to = "group_count") %>%
      filter(group_count >= 0) %>%
      filter(date <= input$date_to_map) %>%
      ggplot(aes(x= date, fill=status, y= group_count))+
      geom_bar(width = .8, position = position_dodge(width=1), stat="identity")+
      labs(title = "<b>COVID-19</b> Count of Daily Change", 
           subtitle = "In <b style = 'color:#F39C12'> Newly Diagnosed</b> and 
       <b style = 'color:#3D9970'> Newly Recovered</b>", 
           y="", x="",
           caption = "Data JHU CSSE\nViz/Code @delabjl")+
      scale_fill_manual(values = c("#F39C12", "#3D9970"))+
      guides(colour = guide_legend(override.aes = list(shape = 19)))+
      theme_minimal()+
      theme(legend.position = "none",
            plot.title.position =  "plot", 
            plot.background = element_rect(fill="#343E48", color = "#343E48"), 
            panel.grid.minor = element_line(color = "#3f4b57", size = .25), 
            panel.grid.major = element_line(color = "#495866", size = .5), 
            axis.title = element_markdown(color="#D6D6D6"), 
            axis.text = element_text(color="#D6D6D6"),
            plot.caption = element_text(color="#D6D6D6")
            
      )+
      theme(plot.title = element_markdown(color = "#D6D6D6", lineheight = 2), 
            plot.subtitle = element_markdown(color = "#D6D6D6", lineheight =  1.2))
  })
  
  output$total_trends_plot <- renderPlot({
    req(input$date_to_map)
    
    covid19_df %>%
      select(date, count_confirmed, count_recovered, count_dead)%>%
      filter(date <= input$date_to_map) %>%
      group_by(date)%>%
      drop_na(count_confirmed, date, count_recovered, count_dead) %>%
      summarise(count_remaining= sum(count_confirmed) - sum(count_recovered), 
                count_recovered=sum(count_recovered), 
                count_confirmed=sum(count_confirmed), 
                count_dead=sum(count_dead)) %>%
      ggplot(aes())+
      geom_line(aes(x = date, y= count_confirmed), color = "#0073B7", size =1.25)+
      geom_point(aes(x = date, y= count_confirmed), color = "#0073B7" , size =2)+
      geom_line(aes(x = date, y= count_recovered), color = "#3D9970" , size =1.25)+
      geom_point(aes(x = date, y= count_recovered), color = "#3D9970" , size =2)+
      geom_line(aes(x = date, y= count_remaining), color = "#F39C12", size =1.25)+
      geom_point(aes(x = date, y= count_remaining), color = "#F39C12" , size =2)+
      geom_line(aes(x = date, y= count_dead), color = "#DD4B39", size =1.25)+
      geom_point(aes(x = date, y= count_dead), color = "#DD4B39" , size =2)+
      labs(title = "<b> COVID-19</b> Trends of", 
           subtitle = "Number of <b style='color:#0073B7'> Total Cases</b>,
       <b style='color:#F39C12'> Active Cases</b>, 
       <b style = 'color:#3D9970'> Recovered Cases</b>, and 
       <b style = 'color:#DD4B39'> Deaths</b>", 
           x="", y="",
           caption = "Data JHU CSSE\nViz/Code @delabjL")+
      theme_minimal()+
      theme(legend.position = "none",
            plot.title.position =  "plot", 
            plot.background = element_rect(fill="#343E48", color = "#343E48"), 
            panel.grid.minor = element_line(color = "#3f4b57", size = .25), 
            panel.grid.major = element_line(color = "#495866", size = .5), 
            axis.title = element_markdown(color="#D6D6D6"), 
            axis.text = element_text(color="#D6D6D6"), 
            plot.caption = element_text(color="#D6D6D6")
            
      )+
      theme(plot.title = element_markdown(color = "#D6D6D6", lineheight = 2), 
            plot.subtitle = element_markdown(color = "#D6D6D6", lineheight =  1.2))
    
    
    
  })
  
  
}
