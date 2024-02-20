
## Library
library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(highcharter)
library(tidyverse)
library(data.table)
library(reshape2)
library(scales)
library(plotly)
library(jsonlite)

#reading data files
df1 <- read_csv("county_data_map.csv")

df_weather <- read_csv("weather_counts.csv")

df_plot2 <- read_csv("state_name.csv")

weather_state_df <- read_csv("map_plot2.csv")

#creating color palette for map
map_color <- function (n = 34, colors = c("#b3c7bf", "#447d84", "#214965")) 
{
  palcols <- (grDevices::colorRampPalette(colors))(n)
  list_parse2(data.frame(q = seq(0, n - 1)/(n - 1), c = palcols))
}

#updating weather data for stack bar plot
df_weather2 <- df_weather %>%
  count(state_abbr,temprature, depression) %>%       
  group_by(state_abbr,temprature) %>%
  mutate(pct= prop.table(n) * 100)

#Converting long to wide form for box plot
df2 <- df1 %>% 
  select(-county_name,-state_name,-county_code,-TotalPopulation)

df3 <- melt(df2, id.vars = c("depression"), variable.name = "factor_name")

my_data3_long <- df2 %>%
  gather(column_name, value)

## Load maps at start for speed
mapdata <- readLines("us-all-all.js", warn = FALSE, encoding = "UTF-8")
mapdata[1] <- gsub(".* = ", "", mapdata[1])
mapdata <- paste(mapdata, collapse = "\n")
mapdata <- stringr::str_remove(mapdata, ";$")
mapdata <- jsonlite::fromJSON(mapdata, simplifyVector = FALSE)
maps1 <- mapdata

mapdata <- readLines("us-all.js", warn = FALSE, encoding = "UTF-8")
mapdata[1] <- gsub(".* = ", "", mapdata[1])
mapdata <- paste(mapdata, collapse = "\n")
mapdata <- stringr::str_remove(mapdata, ";$")
mapdata <- jsonlite::fromJSON(mapdata, simplifyVector = FALSE)
maps2 <- mapdata


#UI part for shiny application
ui <- shiny::fluidPage(
  useShinyjs(),
  #Application title
  fluidRow(style = "background-color: white;",
           column(style='font-family: Georgia;text-align:center;font-size:50px',12,
                  strong("Potential Influences on Depression in USA"))),
  fluidRow(column(12, br())),
  #introduction
  fluidRow(style = "background-color: white;",
           column(style='font-family: Georgia;text-align:center;font-size:23px',8,height = 400, 
                  c("These narrative visualizations will be presented as a 
                    variety of plots and charts, all of which are intended to 
                    clarify the complex connections between depression and its 
                    contributing variables. Demonstration of patterns, connections, 
                    and trends using these visual aids will help our audience to
                    better understand the details. This includes geographic maps 
                    to illustrate regional differences in depression rates, 
                    scatterplot for highlighting correlations, histogram to 
                    illustrate distribution and boxplot to observe outliers. 
                    Better decision-making will be facilitated by this method, 
                    which may also lead to more comprehensive approaches to treating 
                    and lowering depression in the nation.")),
           column(4,img(src = "https://images.wsj.net/im-174983?width=1280&size=1", width = "100%",height = 400) )),
  # fluidRow(column(12, br())),
  fluidRow(
    column(12,br())
  ),
  #Question and Findings
  fluidRow(style = "background-color: white;",
           column(style='font-family: Georgia;border: 1.5px solid black;align:center;text-align:center;font-size:16px',12,
                  fluidRow(
                           column(12,radioButtons("question", "How many people in USA are depressed?",
                                                 choices = list("1 in 5",
                                                                "1 in 10",
                                                                "1 in 20"),
                                                 selected = character(0), inline=T
                           ))),
                  fluidRow(tags$style(
                    HTML(".left-align { text-align: left; }")
                  ),column(12,uiOutput("a"))))),
  #Heat map and sliders
  fluidRow(style = "background-color: white;",
           column(2,fluidRow(style='font-family: Georgia;font-size:16px',
                             
                             tags$style(
             HTML(".irs-grid-text { background: none !important; border: none !important; }"),
             HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .irs-bar { background: #b3c7bf !important; }"),  # Change the slider bar color
             HTML(".irs-to, .irs-from { background: none !important; color: black !important; }")  # Change the color of the values
           ),
                             sliderInput("input_Mental_health_not_good", "Mental health not good Range ",
                                         min = min(df1$`Mental health not good`), max = max(df1$`Mental health not good`),
                                         value = c(min(df1$`Mental health not good`),max(df1$`Mental health not good`))),
                             round=TRUE),
                  fluidRow(style='font-size:16px',
                           sliderInput("input_Arthritis", "Arthritis Range ",
                                       min = min(df1$Arthritis), max = max(df1$Arthritis),
                                       value = c(min(df1$Arthritis),max(df1$Arthritis)))),
                  fluidRow(style='font-size:16px',
                           sliderInput("input_Chronic_disease", "Chronic disease Range ",
                                       min = min(df1$`Chronic disease`), max = max(df1$`Chronic disease`),
                                       value = c(min(df1$`Chronic disease`),max(df1$`Chronic disease`)))),
                  fluidRow(style='font-size:16px',
                           sliderInput("input_Cognitive_disability", "Cognitive disability Range ",
                                       min = min(df1$`Cognitive disability`), max = max(df1$`Cognitive disability`),
                                       value = c(min(df1$`Cognitive disability`),max(df1$`Cognitive disability`)))),
                  fluidRow(style='font-size:16px',
                           sliderInput("input_Current_asthma", "Current asthma Range ",
                                       min = min(df1$`Current asthma`), max = max(df1$`Current asthma`),
                                       value = c(min(df1$`Current asthma`),max(df1$`Current asthma`)))),
                  fluidRow(style='font-size:16px',
                           sliderInput("input_Current_smoking", "Current smoking Range ",
                                       min = min(df1$`Current smoking`), max = max(df1$`Current smoking`),
                                       value = c(min(df1$`Current smoking`),max(df1$`Current smoking`))))),
           (column(10,
                   highcharter::highchartOutput(width = "100%", height = 600,outputId = "map")))
  ),
  fluidRow(column(12,br())),
  fluidRow(column(12,br())),
  #Scatter, boxplot and Histogram
  fluidRow(style = "background-color: white;",
           column(12,style='font-family: Georgia;text-align:center;font-size:23px', 
                  c("Select different factors from below list on left side to see relationship
                    of listed lifestyle factors with Depression"))),
  fluidRow(column(12,br())),
  fluidRow(style = "font-family: Georgia;background-color: white;",
           column(style='font-size:16px',2, 
                  radioButtons("factor_name", "Select Factor",
                               choices = list("Mental health not good",
                                              "Arthritis",
                                              "Chronic disease",
                                              "Cognitive disability",
                                              "Current asthma",
                                              "Current smoking"),
                               selected = "Mental health not good"
                  )),
           (column(style='font-size:20px',5,
                   plotOutput("Viz1"))),
           (column(5,plotOutput("Viz3")))
  ) ,
  fluidRow(column(2,br()),
           column(10,plotlyOutput("Viz2", height = 300))),
  fluidRow(column(12,br())),
  fluidRow(style = "background-color: white;",
           column(12,style='font-family: Georgia;text-align:center;font-size:23px', 
                  c("Click on any state to see distribution of Depressed 
                    population across that state for High and Low tempreature."))),
  #State map for weather and stacked bar chart
  fluidRow(column(12,
                  highcharter::highchartOutput(width = "100%", 
                                               height = 600,
                                               outputId = "map_weather"))),
  fluidRow(
    column(2,shinyjs::hidden(actionButton("usa_bar", "USA Stack Bar Plot"))),
           column(10,plotlyOutput("Viz4"))),
  
  fluidRow(column(12,br())),
  fluidRow(column(12,br())),
  #About
  fluidRow(style = "font-family: Georgia;background-color: white;",column(12,c("About:"))),
  fluidRow(style = "font-family: Georgia;background-color: white;",column(12,a("Data Source 1", href = "https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-County-Data-20/swc5-untb"))),
  fluidRow(style = "font-family: Georgia;background-color: white;",column(12,a("Data Source 2", href = "https://data.census.gov/table?q=age&g=010XX00US$8600000&y=2021&tid=ACSST5Y2021.S1101"))),
  fluidRow(style = "font-family: Georgia;background-color: white;",column(12,a("Data Source 3", href = "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/mapping"))),
  fluidRow(style = "font-family: Georgia;background-color: white;",column(12,a("Data Source 3", href = "https://data.world/nrippner/ansi-geographic-codes"))),
  fluidRow(style = "font-family: Georgia;background-color: white;",column(12,a("Map Plot reference", href = "https://stackoverflow.com/questions/48898192/drilldown-united-states-city-county-map-from-states-using-highcharter")))
  
)

server <- function(input, output, session) {
  
  #Server part for question
  output$a <- renderUI({ 
    if (is.null(input$question)) {
      HTML("<span style='font-size: 24px;'>Think ...</span> <br>")
      
      
    } else if(input$question == "1 in 10"){
      HTML(paste("<span style='font-size: 24px;'>You were incorrect in USA ...</span> <br>
                  &bull;<strong style='text-align: left;'>1 in 5 people are Depressed</strong> <br>
                  &bull; <strong style='text-align: left;'>22 in 100 people are suffering from Arthritis</strong> <br>
                  &bull; <strong style='text-align: left;'>5 in 100 people are suffering from Chronic obstructive pulmonary disease among adults</strong> <br>
                  &bull; <strong style='text-align: left;'>12 in 100 people are suffering from Cognitive disability among adults</strong> <br>
                  &bull; <strong style='text-align: left;'>10 in 100 people are suffering from Current asthma</strong> <br>
                  &bull; <strong style='text-align: left;'>14 in 100 people Smokes daily</strong> <br>
                  &bull; <strong style='text-align: left;'>15 in 100 people are suffering from Mental health not good</strong>"))
    } else if(input$question == "1 in 20"){
      HTML(paste("<span style='font-size: 24px;'>You were incorrect in USA ...</span> <br>
                  &bull;<strong style='text-align: left;'>1 in 5 people are Depressed</strong> <br>
                  &bull; <strong style='text-align: left;'>22 in 100 people are suffering from Arthritis</strong> <br>
                  &bull; <strong style='text-align: left;'>5 in 100 people are suffering from Chronic obstructive pulmonary disease among adults</strong> <br>
                  &bull; <strong style='text-align: left;'>12 in 100 people are suffering from Cognitive disability among adults</strong> <br>
                  &bull; <strong style='text-align: left;'>10 in 100 people are suffering from Current asthma</strong> <br>
                  &bull; <strong style='text-align: left;'>14 in 100 people Smokes daily</strong> <br>
                  &bull; <strong style='text-align: left;'>15 in 100 people are suffering from Mental health not good</strong>"))
    } 
    else {
      HTML(paste("<span style='font-size: 24px;'>Yes you were correct in USA ...</span> <br>
                  &bull;<strong style='text-align: left;'>1 in 5 people are Depressed</strong> <br>
                  &bull; <strong style='text-align: left;'>22 in 100 people are suffering from Arthritis</strong> <br>
                  &bull; <strong style='text-align: left;'>5 in 100 people are suffering from Chronic obstructive pulmonary disease among adults</strong> <br>
                  &bull; <strong style='text-align: left;'>12 in 100 people are suffering from Cognitive disability among adults</strong> <br>
                  &bull; <strong style='text-align: left;'>10 in 100 people are suffering from Current asthma</strong> <br>
                  &bull; <strong style='text-align: left;'>14 in 100 people Smokes daily</strong> <br>
                  &bull; <strong style='text-align: left;'>15 in 100 people are suffering from Mental health not good</strong>"))
    }
    
    
    
  })
  
  #Sliders input for heat map
  slider_Mental_health_not_good <- reactive({
    c(input$input_Mental_health_not_good[1], 
      input$input_Mental_health_not_good[2]) })
  
  slider_Arthritis <- reactive({
    c(input$input_Arthritis[1],
      input$input_Arthritis[2]) })
  
  slider_Chronic_disease <- reactive({ 
    c(input$input_Chronic_disease[1],
      input$input_Chronic_disease[2]) })
  
  slider_Cognitive_disability <- reactive({ 
    c(input$input_Cognitive_disability[1],
      input$input_Cognitive_disability[2]) })
  
  slider_Current_asthma <- reactive({
    c(input$input_Current_asthma[1],
      input$input_Current_asthma[2]) })
  
  slider_Current_smoking <- reactive({
    c(input$input_Current_smoking[1],
      input$input_Current_smoking[2]) })
  
  #filter dataframe
  factor_df <- reactive({
    df3  %>%
      filter(factor_name %in% input$factor_name & depression!=0)
  })
  
  filter_weather_df <- reactive({
    df_weather2 %>%
      filter(input$state_select == state_abbr)
  })
  
  #Filter dataframe for heatmap
  filter_df <- reactive({
    df1  %>%
      filter(`Mental health not good` >= slider_Mental_health_not_good()[1] & 
               `Mental health not good` <= slider_Mental_health_not_good()[2] &
               
               Arthritis >= slider_Arthritis()[1] & 
               Arthritis <= slider_Arthritis()[2] &
               
               `Chronic disease` >= slider_Chronic_disease()[1] & 
               `Chronic disease` <= slider_Chronic_disease()[2] &
               
               `Cognitive disability` >= slider_Cognitive_disability()[1] & 
               `Cognitive disability` <= slider_Cognitive_disability()[2] &
               
               `Current asthma` >= slider_Current_asthma()[1] & 
               `Current asthma` <= slider_Current_asthma()[2] &
               
               `Current smoking` >= slider_Current_smoking()[1] & 
               `Current smoking` <= slider_Current_smoking()[2] )
  })
  
  #Creating county heatmap
  county_map <- shiny::reactive({
    highcharter::highchart() %>%
      highcharter::hc_add_series_map(
        map = maps2,
        # df = data.frame(state_abbr = c("us-tx-03"), y = c(10)),
        df = df_plot2,
        joinBy = c("postal-code", "state_name"),
        value = "y",
        borderColor = "black",borderWidth = 2,
        dataLabels = list(enabled = TRUE, color = "black",
                          textOutline = "0", format = "{point.statename}")
        # showInLegend = TRUE
        # color = "#008000"
      )%>%
      highcharter::hc_add_series_map(
        map = maps1,
        # df = data.frame(state_abbr = c("us-tx-03"), y = c(10)),
        df = filter_df(),
        joinBy = c("hc-key", "county_code"),
        borderColor = "lightgrey",borderWidth = 1,
        value = "depression"
      )  %>%
      hc_colorAxis(stops = map_color()) %>%
      hc_plotOptions(series = list(allAreas = TRUE))%>%
      # hc_colorAxis(dataClasses = stops) %>%
      hc_legend(format = "{value}",
                title = list(text = "Depression %"),align = 'right',verticalAlign = "bottom",x = -10,y = 10) %>%
      hc_tooltip(crosshairs = TRUE,
                 borderWidth = 5,
                 sort = TRUE,
                 table = TRUE,
                 headerFormat = "<b>{point.key}</b><br>",
                 pointFormat = "Depression: {point.value}%<br>
                 Arthritis: {point.Arthritis}%<br>
                 Chronic disease: {point.Chronic disease}%<br>
                 Cognitive disability: {point.Cognitive disability}%<br>
                 Current Asthma: {point.Current asthma}%<br>
                 Current Smoking: {point.Current smoking}%<br>
                 Mental health not good: {point.Mental health not good}%<br>",
                 useHTML = TRUE
      )


  })


  # Set County map at outset
  output$map <- highcharter::renderHighchart({ county_map() })
  
  #Scatter plot
  output$Viz1 <- renderPlot({#https://www.geeksforgeeks.org/comprehensive-guide-to-scatter-plot-using-ggplot2-in-r/
    ggplot(factor_df(), aes(y = depression, x = value)) +
      geom_point( color = "#447d84")  +
      theme_bw() +
      labs(y = "Depression", x = input$factor_name) +
      ggtitle(paste0("Depression vs ",input$factor_name)) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none"
      )
  })
  
  #Histogram
  output$Viz2 <- renderPlotly({
    ggplot(data = factor_df(), aes(x = value)) +
      geom_histogram(binwidth = 0.2, color = "black", fill = "#447d84") +
      labs(x = input$factor_name, y = "Count") +
      ggtitle(paste0("Distribution of ",input$factor_name)) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  #Boxplot
  output$Viz3 <- renderPlot({
    ggplot(data = factor_df(), aes(x = value)) +
      geom_boxplot(fill = "#447d84") +
      labs(x = input$factor_name) +
      ggtitle(paste0("Distribution of Factor ",input$factor_name)) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  
  
  #Stacked bar chart
  weather_county_bar <- 
    renderPlotly({#https://stackoverflow.com/questions/74603107/how-to-create-100-stacked-bar-chart-with-ggplot2
      ggplot(filter_weather_df(), aes(y=temprature, pct, fill=depression)) +
        geom_bar(stat="identity",width=0.4,color = "black", alpha= 0.8) +
        geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
                  position=position_stack(vjust=0.5)) +
        labs(title = "Depression vs Temprature", x = "%of Total Population", y = "Temparture") +
        scale_fill_manual(values = c("#b3c7bf", "#214965")) 
    })
  
  #USA Stacked bar chart
  weather_county_bar_usa <-
    renderPlotly({#https://stackoverflow.com/questions/74603107/how-to-create-100-stacked-bar-chart-with-ggplot2
      ggplot(df_weather2 %>%
               filter(state_abbr == "USA"), aes(y=temprature, pct, fill=depression)) +
        geom_bar(stat="identity",width=0.4,color = "black", alpha= 0.8) +
        geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
                  position=position_stack(vjust=0.5)) +
        labs(title = "Depression vs Temprature", x = "%of Total Population", y = "Temparture") +
        scale_fill_manual(values = c("#b3c7bf", "#214965")) 
    })

  
  ## State map weather
  state_map <- shiny::reactive({
    highcharter::highchart() %>%
      highcharter::hc_add_series_map(
        map = maps2,
        df = weather_state_df,
        joinBy = c("postal-code", "state_abbr"),
        value = "Depression",
        dataLabels = list(enabled = TRUE,color = 'white',
                          format = "{point.State}")
      )  %>%
      hc_colorAxis(stops = map_color()) %>%
      highcharter::hc_tooltip(
        headerFormat = "<b>{point.key}</b><br>",
        pointFormat = "Depression: {point.value}",
        useHTML = TRUE
      ) %>%hc_legend(format = "{value}",
                title = list(text = "Depression %"),align = 'right',verticalAlign = "bottom",x = -10,y = 10)%>%
      highcharter::hc_plotOptions(
        series = list(
          allowPointSelect = TRUE,
          events = list(
            click = htmlwidgets::JS(
              "function(event) {
                 Shiny.setInputValue(
                   'state_select', 
                   event.point.state_abbr, 
                   {priority: 'event'}
                 );
              }"
            )
          )
        )
      )
  })

  ## Set to state map at outset
  output$map_weather <- highcharter::renderHighchart({ state_map() })
  
  #Creating button to show USA stacked bar chart
  output$ui <- shiny::renderUI({
    if (!is.null(input$state_select)) {
      shiny::actionButton(
        inputId = "usa_bar",
        label = "Return to USA Map"
      )
    }
  })
  
  #Plotting USA stacked bar chart
  shiny::observeEvent(
    eventExpr = input$usa_bar,
    handlerExpr = {
      output$Viz4 <- weather_county_bar_usa
      shinyjs::show(id = "usa_bar")
    }
  )

  #Plotting state stacked bar chart
  shiny::observeEvent(
    eventExpr = input$state_select,
    handlerExpr = {
      output$Viz4 <- weather_county_bar
      shinyjs::show(id = "usa_bar")
    }
  )


  
}


shiny::shinyApp(ui = ui, server = server)
