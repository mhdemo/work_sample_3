library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(scales)
library(readr)
library(stringr)
library(tidyr)
library(ggsci)
library(DT)
library(here)
library(janitor)
library(maps)
library(purrr)

#Loads data ####
#master_table <- read_rds("master_table.rds")
#lap_times <- read_csv("lapTimes.csv")

#Used when working in r project directory
master_table <- read_rds(paste0(here(), "/F1_App/master_table.rds"))
lap_times <- read_csv(paste0(here(), "/F1_App/lapTimes.csv"))

#Formats data ####
lap_times %>%
    mutate(l_time = hms::as_hms(as_datetime(milliseconds(milliseconds)))) %>%
    select(-c(time, milliseconds)) %>%
    left_join(master_table %>%
                  distinct(raceId, driverId, forename, surname, 
                           race_date, location, country, name,
                           grid, position) %>%
                  rename(f_position = position),
              by = c("raceId", "driverId")) %>%
    mutate(race_location = paste0(location, ", ", country),
           drv_name = iconv(paste(forename, surname), 
                            from = "UTF-8", to = "ASCII", sub = ""),
           dp_name = paste0(f_position, ":", drv_name)) -> lap_times

master_table %>%
    filter(!is.na(dob)) %>%
    mutate(drv_name = iconv(paste(forename, surname), 
                            from = "UTF-8", to = "ASCII", sub = ""),
           race_age = time_length(interval(dob, race_date), "years"),
           ra_group = as.character(cut(race_age, breaks = seq(15, 60, 5),
                                       labels = c("16-20", "21-25",
                                                  "26-30", "31-35",
                                                  "36-40", "41-45",
                                                  "46-50", "51-55", 
                                                  "56-60")))) %>%
    group_by(driverId) %>%
    mutate(podium = if_else(positionOrder %in% c(1:3), 1, 0),
           win = if_else(positionOrder == 1, 1, 0)) %>%
    ungroup() -> master_table

#Remove Antartica from the map
map_data("world") %>%
    filter(region != "Antarctica") -> world_data

#Custom color pallette ####
my_pal <- pal_locuszoom()(7)

#Application ####
header <- dashboardHeader(
    title = "Formula One Performance Analysis",
    titleWidth = 450
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Driver vs. Driver: Single Race", tabName = "drv_v_drv", icon = icon("chart-line")),
        menuItem("Driver vs. Driver: Career", tabName = "drv_career", icon = icon("chart-line")),
        menuItem("Total Podiums by Agg", tabName = "agg_stat", icon = icon("chart-line")),
        menuItem("Top 10 By Agg Level", tabName = "top10ot", icon = icon("chart-line"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "drv_v_drv",
                fluidRow(
                    column(2, offset = 0, style = "padding:1px",
                           selectizeInput("r_loc", "Select Race Location:",
                                       choices = lap_times %>%
                                           arrange(race_location) %>%
                                           pull(race_location) %>%
                                           unique())),
                    column(2, offset = 0, style = "padding:1px",
                           selectInput("r_date", "Select Race Date:",
                                        choices = NULL)),
                    column(2, offset = 0, style = "padding:1px",
                           selectizeInput("drv_1", "Select Driver 1:",
                                       choices = NULL)),
                    column(2, offset = 0, style = "padding:1px",
                           selectizeInput("drv_2", "Select Driver 2:",
                                       choices = NULL))),
                fluidRow(
                    box(plotlyOutput("drv_2_diff", height = 500), width = 12)),
                fluidRow(
                    box(plotlyOutput("dvd_line", height = 500), width = 12)),
                fluidRow(
                    box(DT::dataTableOutput("dvd_table"), width = 12))
        ),
        tabItem(tabName = "drv_career",
                fluidRow(
                    column(2, offset = 0, style = "padding:1px",
                           selectizeInput("drv_hist", "Select Driver(s):",
                                          choices = master_table %>%
                                              pull(drv_name) %>%
                                              unique(),
                                          selected = "Lewis Hamilton",
                                          multiple = TRUE))),
                fluidRow(
                    box(plotlyOutput("tot_pod", height = 500), width = 12),
                    box(plotlyOutput("tot_const", height = 500), width = 12))
        ),
        tabItem(tabName = "agg_stat",
                fluidRow(
                    column(2, offset = 0, style = "padding:1px",
                           selectizeInput("agg_lvl_geo", "Select Aggregation Level:",
                                          choices = c("Driver" = "drv_name", "Nationality" = "nationality", 
                                                      "Constructor" = "c_name", "Race Day Age" = "ra_group"), #Add age grouping
                                          selected = "Driver")),
                    column(2, offset = 0, style = "padding:1px",
                           selectizeInput("agg_filter_geo", "Select Filter:",
                                          choices = NULL))),
                
                fluidRow(
                    box(plotOutput("pod_locs", height = 500), width = 12),
                    box(DT::dataTableOutput("pod_l_table"), width = 12))
        ),
        tabItem(tabName = "top10ot",
                fluidRow(
                    column(2, offset = 0, style = "padding:1px",
                           selectizeInput("agg_lvl_top", "Select Aggregation Level:",
                                          choices = c("Nation" = "nationality",
                                                    "Constructor" = "c_name",
                                                    "Driver" = "drv_name"),
                                          selected = "Nation"))),
                    fluidRow(
                        box(plotlyOutput("top_ot_bar", height = 500), width = 12))
        )
    )
)

ui <- dashboardPage(
    skin = "red",
    header,
    sidebar,
    body
)

server <- function(input, output, session) {

#Reactive expressions ####
    observeEvent(input$r_loc,
                 {updateSelectInput(session, "r_date",
                                    choices = lap_times %>%
                                        filter(race_location == input$r_loc) %>%
                                        arrange(desc(race_date)) %>%
                                        pull(race_date) %>%
                                        unique())})
    
    observeEvent(input$r_date,
                 {updateSelectizeInput(session, "drv_1",
                                       choices = lap_times %>%
                                           filter(race_location == input$r_loc,
                                                  race_date == ymd(input$r_date)) %>%
                                           arrange(f_position) %>%
                                           pull(dp_name) %>%
                                           unique())})
    
    observeEvent(input$r_date,
                 {updateSelectizeInput(session, "drv_2",
                                       choices = lap_times %>%
                                           filter(race_location == input$r_loc,
                                                  race_date == ymd(input$r_date)) %>%
                                           arrange(f_position) %>%
                                           pull(dp_name) %>%
                                           unique())})
    
    race_table <- eventReactive(input$r_date,
                                {
                                    req(input$r_date)
                                    
                                    lap_times %>%
                                        filter(race_location == input$r_loc,
                                               race_date == ymd(input$r_date))
                                    
                                })
    
    drv_table <- eventReactive(input$drv_hist,
                                  {
                                      req(input$drv_hist)
                                      
                                      master_table %>%
                                          filter(drv_name %in% input$drv_hist)
                                  })
    
    drv_const_table <- eventReactive(input$drv_hist,
                                   {
                                       req(input$drv_hist)
                                       
                                       master_table %>%
                                           filter(drv_name %in% input$drv_hist[1:2])
                                   })
    
    observeEvent(input$agg_lvl_geo,
                 {updateSelectizeInput(session, "agg_filter_geo",
                                       choices = master_table %>%
                                           pull(input$agg_lvl_geo) %>%
                                           unique() %>%
                                           sort())})

#Driver vs Driver Single Race Plots ####    
        
    output$drv_2_diff <- renderPlotly({
        
        validate(
            need(input$drv_1 != input$drv_2, "Driver 2 must be different than Driver 1")
        )
        
        dlt1 <- paste0("l_time_", str_remove(input$drv_1, "^[0-9]*:"))
        dlt2 <- paste0("l_time_", str_remove(input$drv_2, "^[0-9]*:"))
        
        #Utilizes tidy evaluation patterns to feed in the correct column name
        #I needed to specify the column orders to ensure that the driver differences were calculated correctly
        diff_plot <- function(d1, d2) {
            
            race_table() %>%
                filter(dp_name %in% c(input$drv_1, input$drv_2)) %>%
                select(drv_name, lap, l_time, position) %>%
                pivot_wider(names_from = drv_name, values_from = c(l_time, position)) %>%
                arrange(lap) %>% #Make sure that laps are in the correct order before calculating the cumsum
                mutate('Driver 1 Cumulative Lap Times' = hms::as_hms(as_datetime(seconds(cumsum(as.numeric(.data[[d1]]))))),
                       'Driver 2 Cumulative Lap Times' = hms::as_hms(as_datetime(seconds(cumsum(as.numeric(.data[[d2]]))))),
                       'Lap Diff' = .data[[d2]] - .data[[d1]]) %>%
                mutate('Cumulative Lap Diff' = .[[7]] - .[[6]]) %>%
                rename(Lap = lap) %>%
                ggplot(aes(Lap, `Cumulative Lap Diff`,
                           group = 1,
                           text = paste0("Driver: ", str_remove(input$drv_2, "^[0-9]*:"), "<br>",
                                         "Lap: ", Lap, "<br>",
                                         "Cumulative Lap Diff (s): ", round(`Cumulative Lap Diff`, 2)))) + 
                geom_line(col = my_pal[4]) +
                scale_x_continuous(breaks = seq(0, 100, 5)) +
                scale_y_time() +
                geom_hline(yintercept = 0, col = "red") +
                labs(x = "Lap", title = paste0(input$drv_2, " vs. ", input$drv_1, " - Delta Times")) +
                theme(plot.title = element_text(hjust = 0.5))
            
            ggplotly(tooltip = "text")
            
        }
        
        diff_plot(dlt1, dlt2)
        
    })
    
    output$dvd_line <- renderPlotly({
        
        race_table() %>%
            filter(dp_name %in% c(input$drv_1, input$drv_2)) %>%
            ggplot(aes(lap, l_time, col = factor(dp_name),
                       group = drv_name,
                       text = paste0("Driver: ", drv_name, "<br>",
                                     "Lap: ", lap, "<br>",
                                     "Lap Time: ", l_time))) +
            geom_point() + geom_line() +
            scale_x_continuous(breaks = seq(0, 100, 10)) +
            labs(x = "Lap", y = "Lap Time", color = "Final Position:Driver",
                 title = "Driver vs Driver Race Performance") +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_color_manual(values = c(my_pal[1], my_pal[4]))
        
        ggplotly(tooltip = "text")
    })
    
    output$dvd_table <- DT::renderDataTable({

        validate(
            need(input$drv_1 != input$drv_2, "Driver 2 must be different than Driver 1")
        )
        
        dlt1 <- paste0("l_time_", str_remove(input$drv_1, "^[0-9]*:"))
        dlt2 <- paste0("l_time_", str_remove(input$drv_2, "^[0-9]*:"))
        
        #Utilizes tidy evaluation patterns to feed in the correct column name
        #I needed to specify the column orders to ensure that the driver differences were calculated correctly
        diff_table <- function(d1, d2) {
            
            race_table() %>%
                filter(dp_name %in% c(input$drv_1, input$drv_2)) %>%
                select(drv_name, lap, l_time, position) %>%
                pivot_wider(names_from = drv_name, values_from = c(l_time, position)) %>%
                arrange(lap) %>% #Make sure that laps are in the correct order before calculating the cumsum
                mutate('Driver 1 Cumulative Lap Times' = hms::as_hms(as_datetime(seconds(cumsum(as.numeric(.data[[d1]]))))),
                       'Driver 2 Cumulative Lap Times' = hms::as_hms(as_datetime(seconds(cumsum(as.numeric(.data[[d2]]))))),
                       'Lap Diff' = round(.data[[d2]] - .data[[d1]], 2)) %>%
                mutate('Cumulative Lap Diff' = round(.[[7]] - .[[6]], 2)) %>%
                rename(Lap = lap,
                       `Driver 1 Lap Times` = .data[[d1]],
                       `Driver 2 Lap Times` = .data[[d2]]) %>%
                select(1, `Driver 1 Lap Times`, `Driver 2 Lap Times`, `Lap Diff`, 
                       `Driver 1 Cumulative Lap Times`, `Driver 2 Cumulative Lap Times`,
                       `Cumulative Lap Diff`)
        }
        
        diff_table(dlt1, dlt2) 
           
    })
    
#Driver Career Plots ####
    
    output$tot_pod <- renderPlotly({
        
        drv_table() %>%
            group_by(drv_name) %>%
            summarize(total_pod = sum(podium),
                      total_win = sum(win),
                      total_race = n_distinct(raceId)) %>%
            pivot_longer(cols = c(total_pod, total_win, total_race), names_to = "perf_type") %>%
            mutate(perf_type = recode(perf_type, "total_race" = "Total Races", "total_pod" = "Total Podiums",
                                      "total_win" = "Total Wins"),
                   perf_type = factor(perf_type, levels = c("Total Races", "Total Podiums", "Total Wins"))) %>%
            ggplot(aes(drv_name, value, fill = perf_type,
                       text = paste0("Driver: ", drv_name, "<br>",
                                     "Race Metric: ", perf_type, "<br>",
                                     "Total: ", value))) + geom_col(position = "dodge") +
            scale_fill_manual(values = c("Total Races" = my_pal[4], "Total Podiums" = my_pal[5], "Total Wins" = my_pal[3]),
                              name = "Race Metric") +
            labs(x = "Driver", y = "Metric Count", title = "Career Races/Podiums/Wins") +
            theme(plot.title = element_text(hjust = 0.5))
        
        ggplotly(tooltip = "text")
    })
    
    output$tot_const <- renderPlotly({
        
        drv_const_table() %>%
            group_by(drv_name) %>%
            count(c_name) %>%
            mutate(n_perc = n / sum(n),
                   cg_name = if_else(n_perc < 0.20, "Other", c_name)) %>%
            ungroup() %>%
            ggplot(aes(drv_name, n, fill = reorder(factor(cg_name), n),
                       text = paste0("Driver: ", drv_name, "<br>",
                                     "Contstructor: ", cg_name, "<br>",
                                     "Race Count: ", n))) + geom_col(position = "stack") +
            labs(x = "Driver", title = "Races with Constructors", fill = "Constructors", y = "",
                 caption = "Constructors that make up less than 20% of a Driver's career races are categorized as 'Other'") +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_fill_locuszoom()
        
        ggplotly(tooltip = "text") %>%
            layout(annotations = list(x = 1, y = -0.1, text = "Constructors that make up less than 20% of a Driver's career races are categorized as 'Other'", 
                                      showarrow = F, xref = 'paper', yref = 'paper', 
                                      font = list(size = 8, color = "red")))
        
    })
    
    output$pod_locs <- renderPlot({
        
        
        agg_pod_plot <- function(dat, metr, m_filter) {
            
            dat %>%
                filter(podium == TRUE) %>%
                count({{ metr }}, lng, lat) %>%
                filter({{ metr }} == m_filter) -> geo_df
            
            ggplot() +
                geom_map(data = world_data, map = world_data,
                         aes(x = long, y = lat, map_id = region),
                         fill = "#a8a8a8", color = "#ffffff", size = 0.5) +
                geom_point(data = geo_df, aes(x = lng, y = lat, size = n),
                           alpha = 0.75, shape = 1, stroke = 2, col = my_pal[1]) +
                scale_radius(range = c(2, 7)) +
                scale_color_locuszoom() +
                labs(size = input$agg_lvl_geo, title = "Podium Locations") +
                theme(axis.title=element_blank(),
                      axis.text=element_blank(),
                      axis.ticks=element_blank(),
                      plot.title = element_text(hjust = 0.5))
            
        }
        
        agg_pod_plot(master_table, !!sym(input$agg_lvl_geo), input$agg_filter_geo)
        
    })
    
    output$pod_l_table <- DT::renderDataTable({
        
        agg_pod_table <- function(dat, metr, m_filter) {
            
            dat %>%
                filter(podium == TRUE) %>%
                mutate(r_loc = paste0(location, ", ", country)) %>%
                count({{ metr }}, r_loc) %>%
                filter({{ metr }} == m_filter) %>%
                arrange(desc(n)) 
            
        }
        
        agg_pod_table(master_table, !!sym(input$agg_lvl_geo), input$agg_filter_geo) %>%
            rename("Filter" = 1,  "Location" = 2, "Podium Count" = 3)
        
    })
    
    output$top_ot_bar <- renderPlotly({
        
        top_10_filter <- function(dat, vrb, vrb_name) {
            
            plty_vrb <- rlang::enquo(vrb_name)
            
            dat %>%
                group_by({{ vrb }}) %>%
                summarize(total_p = sum(podium)) %>%
                arrange(desc(total_p)) %>%
                head(10) %>%
                pull({{ vrb }}) -> top_10_vrb
            
            dat %>%
                filter({{ vrb }} %in% top_10_vrb) %>%
                group_by(race_year, {{ vrb }}) %>%
                summarize(annual_podium = sum(podium)) %>%
                ungroup() %>%
                arrange(race_year) %>%
                pivot_wider(names_from = {{ vrb }}, values_from = annual_podium) %>%
                map_df(~if_else(is.na(.x), 0, .x)) %>%
                pivot_longer(cols = c(2:ncol(.)), names_to = vrb_name, values_to = "annual_podium") %>%
                group_by({{ vrb }}) %>%
                nest() %>%
                mutate(cumm_podiums = map(data, ~cumsum(.x$annual_podium))) %>%
                unnest(c(data, cumm_podiums)) %>%
                ungroup() %>%
                select(-annual_podium) -> vrb_top_10
            
            rlang::eval_tidy(
                rlang::quo_squash(quo(
                    plot_ly(vrb_top_10,
                            x = ~{{ vrb }},
                            y = ~cumm_podiums,
                            color = ~{{ vrb }},
                            colors = my_pal,
                            type = "bar",
                            frame = ~race_year) %>% layout(yaxis = list(title = "Total Podiums"),
                                                           xaxis = list(title = "Top 10"))))) 
            
        }
        
        top_10_filter(master_table, !!sym(input$agg_lvl_top), input$agg_lvl_top)
        
    })
    
}

shinyApp(ui, server)


