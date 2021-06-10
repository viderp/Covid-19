ui <- dashboardPage(
    ########################################################################################
    # UI Header, simple header
    ########################################################################################
    dashboardHeader(title="COVID-19 Dashboard"),
    ########################################################################################
    # UI Sidebar
    ########################################################################################
    dashboardSidebar(
        sidebarMenu(
            menuItem("Map", tabName = "map_tab", 
                     icon = icon("map-marked")),
            hr(),
            menuItem("Chart, Top N", tabName = "chart_top_tab", 
                     icon = icon("chart-bar")),
            menuItem("Chart, Selected Countries", tabName = "chart_sel_tab", 
                     icon = icon("chart-bar"))
        )
    ),
    ########################################################################################
    # UI Body, style setting and tabItems with 4 tabItem (match menu)
    ########################################################################################

    dashboardBody(
        fluidRow(
            tabItems(
                tabItem("map_tab",
                        # info above map
                        fluidRow(
                            # box with slider and radio buttom for period selection
                            box(column( 
                                sliderInput("period",
                                            "Data period:",
                                            min = min(covid_country_long$Date),
                                            max = max(covid_country_long$Date),
                                            value=c(min(covid_country_long$Date),
                                                    max(covid_country_long$Date)),
                                            timeFormat="%Y/%m/%d"),
                                width=5,offset=0.5),
                                column(
                                    radioButtons("period_quick",label = "Quick select:",
                                                 choices = list("One day" = 1,
                                                                "One week" = 2, 
                                                                "Twe weeks" = 3,
                                                                "One month" = 4, 
                                                                "Whole period" = 5), 
                                                 selected = 5, inline = T),
                                    width=7,style="padding:20px;"),
                                width=8),
                            # 2 value boxes with case and death number
                            uiOutput("total_case"),
                            uiOutput("total_death")
                        ),
                        # map
                        fluidRow(wellPanel(leafletOutput("case_map")))
                ), # tabItem 1 (map) end 
                tabItem("chart_top_tab",fluidRow(box(numericInput(inputId = 'topN',value = 10,label = 'top N contries',min = 1,max = 180)),
                                                 box(radioButtons('log_top',label = 'scale:',choices = list('Linear Scale'=1,"Logarithmic Scale" = 2),selected = 1, inline = T))
                                                 )
                        ,
                        fluidRow(column(htmlOutput("topN_text"),
                                        plotlyOutput("case_plot_top10"),
                                        width=6),
                                 column(htmlOutput("topN_new_text"),
                                        plotlyOutput("newcase_plot_top10"),
                                        width=6)),
                        fluidRow(column(htmlOutput("topN_death_text"),
                                        plotlyOutput("death_plot_top10"),
                                        width=6),
                                 column(htmlOutput("topN_epi_text"),
                                        plotlyOutput("epi_plot_top10"),
                                        width=6)),
                        # info tables
                        ), # tabItem 2 (top N chart) end 
                tabItem("chart_sel_tab",fluidRow(
                    box(selectizeInput("country_select", "Selected countries:",
                                       unique(covid_country_long$`Country/Region`), 
                                       selected = c("Taiwan","China","Iran","Italy",
                                                    "United States of America"), 
                                       multiple = T,
                                       options = NULL),
                        width=4),
                    box(radioButtons("log_sel",label = "Scale: ",
                                     choices = list("Linear Scale" = 1,
                                                    "Logarithmic Scale" = 2), 
                                     selected = 1, inline = T),
                        width=4)
                ),
                fluidRow(column(htmlOutput("country_text"),
                                plotlyOutput("case_plot_country"),
                                width=6),
                         column(htmlOutput("country_new_text"),
                                plotlyOutput("newcase_plot_country"),
                                width=6)),
                fluidRow(column(htmlOutput("country_death_text"),
                                plotlyOutput("death_plot_country"),
                                width=6),
                         column(htmlOutput("country_epi_text"),
                                plotlyOutput("epi_plot_country"),
                                width=6)),
                # info tables
                htmlOutput("country_table_text"),
                fluidRow(wellPanel(DTOutput("table_country")))
                ) # tabItem 3 (selected countries chart) end  
               
            )
        )
    )
)


server<-function(input,output, session){ 
    output$update<-renderUI(tagList(
        paste0("Update: ",max(covid_country_long$Date)),
        br(),
        "Data source:",
        a(href="https://github.com/CSSEGISandData/COVID-19","JHU CSSE",target="_blank"),
        br(),
        "Made by:",
        a("Weida_Pan",target="_blank"),
    )
    )
    
    ### Plot Title
    output$topN_text<-
        renderUI(h3(paste("Total cases, top",input$topN,"countries")))
    output$topN_new_text<-
        renderUI(h3(paste("Daily new cases, top",input$topN,"countries")))
    output$country_text<-
        renderUI(h3(paste("Total cases in selected ",length(input$country_select))," countries"))
    output$country_new_text<-
        renderUI(h3(paste("Daily new cases in selected ",length(input$country_select))," countries"))
    output$country_text_t<-
        renderUI(h3(paste("Total cases in",input$country,", table")))
    output$topN_death_text<-
        renderUI(h3(paste("Total death, top",input$topN,"countries")))
    output$country_death_text<-
        renderUI(h3(paste("Total death in selected ",length(input$country_select))," countries"))
    output$country_death_text_t<-
        renderUI(h3(paste("Total death in",input$country,", table")))
    output$topN_epi_text<-
        renderUI(h3(paste("Number of death since 10th death")))
    output$country_epi_text<-
        renderUI(h3(paste("Number of death since 10th death")))
    output$country_epi_text_t<-
        renderUI(h3(paste("Number of death since 10th death")))
    
    ### valueBox
    output$total_case <- renderUI ({
        valueBox(
            value=format(sum(world_case_dym()@data$NewCaseTotal,na.rm=T), big.mark=","), 
            subtitle=paste0("Cases, ",input$period[1]," to ",input$period[2]), 
            icon = icon("diagnoses", lib = "font-awesome"),
            color = "yellow",width=2
        )
    })
    output$total_death <- renderUI ({
        valueBox(
            value=format(sum(world_case_dym()@data$NewDeathTotal,na.rm=T), big.mark=","), 
            subtitle=paste0("Death, ",input$period[1]," to ",input$period[2]), 
            icon = icon("skull", lib = "font-awesome"),
            color = "red",width=2
        )
    })
    
    ## slider auto update
    observeEvent(input$period_quick, {
        if(input$period_quick=="1"){
            updateSliderInput(session, "period", value = c(max(covid_country_long$Date),
                                                           max(covid_country_long$Date)))
        }else if(input$period_quick=="2"){
            updateSliderInput(session, "period", value = c(max(covid_country_long$Date)-7,
                                                           max(covid_country_long$Date)))
        }else if(input$period_quick=="3"){
            updateSliderInput(session, "period", value = c(max(covid_country_long$Date)-14,
                                                           max(covid_country_long$Date)))
        }else if(input$period_quick=="4"){
            updateSliderInput(session, "period", value = c(max(covid_country_long$Date)-30,
                                                           max(covid_country_long$Date)))
        }else if(input$period_quick=="5"){
            updateSliderInput(session, "period", value = c(min(covid_country_long$Date),
                                                           max(covid_country_long$Date)))
        }
    })
    
    ## map output components setting
    output$case_map<-renderLeaflet({
        #world_case period.....
        leaflet(world_case_dym())  %>%
            addProviderTiles("MapBox", 
                             options = providerTileOptions(
                                 id = "mapbox.light",
                                 accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))%>% 
            addPolygons(
                fillColor = ~pal(NewCaseTotal),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "1",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 1,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = ~lab,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
            ) %>% 
            addLegend(pal = pal, values = ~NewCaseTotal, opacity = 0.7, title = NULL,
                      position = "bottomright") %>%
            setView(0, 25, zoom=2)
        
    })
    
    ## plot output components setting
    output$case_plot_top10<- renderPlotly(
        ggplotly(plot_topN_data())
    )
    output$newcase_plot_top10<- renderPlotly(
        ggplotly(plot_topN_new_data())
    )
    output$death_plot_top10<- renderPlotly(
        ggplotly(plot_topN_death_data())
    )
    output$epi_plot_top10<- renderPlotly(
        ggplotly(plot_topN_epi_data())
    )
    
    output$case_plot_country<- renderPlotly(
        ggplotly(plot_country_case_data()) 
    ) 
    output$newcase_plot_country<- renderPlotly(
        ggplotly(plot_country_newcase_data()) 
    )
    output$death_plot_country<- renderPlotly(
        ggplotly(plot_country_death_data()) 
    )
    output$epi_plot_country<- renderPlotly(
        ggplotly(plot_country_epi_data()) 
    )
    
    ## table output components setting
    output$table_country<-renderDT(
        covid_country %>% ungroup() %>%
            filter(`Country/Region` %in% input$country_select) %>%
            select(`Country/Region`,date,Total_case_num,New_case_num,
                   Total_death_num,New_death_num) %>%
            rename(Date=date,`Total case number`=Total_case_num,
                   `New case number`=New_case_num,
                   `Total death number`=Total_death_num,
                   `New death number`=New_death_num),
        filter = 'top', server = TRUE,
        options = list(pageLength = 10, 
                       autoWidth = TRUE,
                       searching = TRUE),
        rownames= FALSE
    )
    
    ## reactive plot
    plot_topN_data<-reactive({
        p<-ggplot(topN_data())+
            geom_line(aes(x=Date,y=`Case number`,color=`Country/Region`))+
            theme_bw()
        
        if(input$log_top=="2"){
            p+labs(x="Date",y="Total cases (log)")+
                scale_y_log10()
        }else{
            p+labs(x="Date",y="Total cases")
        }
        
    } )
    plot_topN_new_data<-reactive({
        p<-ggplot(topN_new_data())+
            geom_line(aes(x=Date,y=`Case number`,color=`Country/Region`))+
            theme_bw()
        if(input$log_top=="2"){
            p+
                labs(x="Date",y="Daily new cases (log)")+
                scale_y_log10()
        }else{
            p+
                labs(x="Date",y="Daily new cases")
        }
        
    } )
    plot_topN_death_data<-reactive({
        p<-ggplot(topN_death_data())+
            geom_line(aes(x=Date,y=`Case number`,color=`Country/Region`))+
            theme_bw()
        if(input$log_top=="2"){
            p+
                labs(x="Date",y="Daily new cases (log)")+
                scale_y_log10()
        }else{
            p+
                labs(x="Date",y="Daily new cases")
        }
        
    } )
    
    plot_topN_epi_data<-reactive({
        p<-ggplot(topN_epi_data())+
            geom_line(aes(x=nDays,y=`Case number`,color=`Country/Region`))+
            theme_bw()
        if(input$log_top=="2"){
            p+
                labs(x="Number of days since 10th death",y="Total number of death (log)")+
                scale_y_log10()
        }else{
            p+
                labs(x="Number of days since 10th death",y="Total number of death")
        }
        
    } )
    
    
    plot_country_case_data<-reactive({
        p<-ggplot(country_case_data())+
            geom_line(aes(x=Date,y=`Case number`,color=`Country/Region`))+
            theme_bw()
        if(input$log_sel=="2"){
            p+
                labs(x="Date",y="Total cases (log)")+
                scale_y_log10()
            #breaks = trans_breaks("log10", function(x) 10^x),
            #labels = trans_format("log10", math_format(10^.x))
        }else{
            p+
                labs(x="Date",y="Total cases")
        }
        
    } )
    plot_country_newcase_data<-reactive({
        p<-ggplot(country_newcase_data())+
            geom_line(aes(x=Date,y=`Case number`,color=`Country/Region`))+
            theme_bw()
        if(input$log_sel=="2"){
            p+
                labs(x="Date",y="Daily new cases (log)")+
                scale_y_log10()
            #breaks = trans_breaks("log10", function(x) 10^x),
            #labels = trans_format("log10", math_format(10^.x))
        }else{
            p+
                labs(x="Date",y="Daily new cases")
        }
        
    } )
    plot_country_death_data<-reactive({
        p<-ggplot(country_epi_data())+
            geom_line(aes(x=Date,y=`Case number`,color=`Country/Region`))+
            theme_bw()
        if(input$log_sel=="2"){
            p+
                labs(x="Date",y="Total number of death (log)")+
                scale_y_log10()
            #breaks = trans_breaks("log10", function(x) 10^x),
            #labels = trans_format("log10", math_format(10^.x))
        }else{
            p+
                labs(x="Date",y="Total number of death")
        }
        
    } )
    
    plot_country_epi_data<-reactive({
        p<-ggplot(country_death_data())+
            geom_line(aes(x=nDays,y=`Case number`,color=`Country/Region`))+
            theme_bw()
        if(input$log_sel=="2"){
            p+
                labs(x="Number of days since 10th death",y="Total death (log)")+
                scale_y_log10()
            #breaks = trans_breaks("log10", function(x) 10^x),
            #labels = trans_format("log10", math_format(10^.x))
        }else{
            p+
                labs(x="Number of days since 10th death",y="Total death")
        }
    } )
    
    ## reactive data for plot
    country_case_data<-reactive({
        covid_country_long %>% 
            filter(`Country/Region` %in% input$country_select & 
                       Type=="Total_case_num") 
    } )
    country_newcase_data<-reactive({
        covid_country_long %>% 
            filter(`Country/Region` %in% input$country_select & 
                       Type=="New_case_num") 
    } )
    country_epi_data<-reactive({
        covid_country_long %>% 
            filter(`Country/Region` %in% input$country_select & 
                       Type=="Total_death_num") 
    } )
    country_death_data<-reactive({
        covid_country_long %>% 
            filter(`Country/Region` %in% input$country_select & 
                       Type=="Total_death_num") 
    } )
    topN_data<- reactive({
        covid_country_long %>% 
            filter(`Country/Region` %in% TopCaseCountry[1:as.numeric(input$topN)] & 
                       Type=="Total_case_num") %>%
            mutate(`Country/Region`=
                       factor(`Country/Region`,
                              levels = TopCaseCountry[1:as.numeric(input$topN)]))
    } )
    topN_new_data<- reactive({
        covid_country_long %>% 
            filter(`Country/Region` %in% TopNewCaseCountry[1:as.numeric(input$topN)] & 
                       Type=="New_case_num") %>%
            mutate(`Country/Region`=
                       factor(`Country/Region`,
                              levels = TopNewCaseCountry[1:as.numeric(input$topN)]))
    } )
    topN_death_data<- reactive({
        covid_country_long %>% 
            filter(`Country/Region` %in% TopDeathCountry[1:as.numeric(input$topN)] & 
                       Type=="Total_death_num") %>%
            mutate(`Country/Region`=
                       factor(`Country/Region`,
                              levels = TopDeathCountry[1:as.numeric(input$topN)])) 
    } )
    topN_epi_data<- reactive({
        covid_country_long %>% 
            filter(`Country/Region` %in% TopDeathCountry[1:as.numeric(input$topN)] & 
                       Type=="Total_death_num") %>%
            mutate(`Country/Region`=
                       factor(`Country/Region`,
                              levels = TopDeathCountry[1:as.numeric(input$topN)]))
    } )
    
    
    
    ## reactive data for map
    world_case_dym<-reactive({
        period_data<-covid_country %>% 
            filter(date<= lubridate::ymd(input$period[2]) & 
                       date>= lubridate::ymd(input$period[1]))%>% 
            group_by(`Country/Region`) %>% 
            summarise(NewCaseTotal=sum(New_case_num),
                      NewDeathTotal=sum(New_death_num)) %>%
            mutate(CFR=NewDeathTotal/NewCaseTotal)
        
        world_case@data <- 
            left_join(world_case@data, 
                      period_data, 
                      by = c("name" = "Country/Region"))
        world_case@data$lab <- sprintf(
            "<i>%s - %s</i><br/><strong>%s</strong><br/><strong>Total cases:</strong> %g<br/><strong>Total death:</strong> %g<br/><strong>CFR:</strong> %g",
            input$period[1],input$period[2],
            world_case@data$name, world_case@data$NewCaseTotal, 
            world_case@data$NewDeathTotal, round(world_case@data$CFR*100,1)
        ) %>% lapply(htmltools::HTML)
        world_case
    })
    }
shinyApp(ui = ui, server = server)
