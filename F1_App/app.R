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

#Loads data ####
read_rds("master_table.rds")

lap_times <- read_csv("lapTimes.csv")

lap_times %>%
    mutate(l_time = hms::as_hms(as_datetime(milliseconds(milliseconds)))) %>%
    select(-c(time, milliseconds)) %>%
    left_join(master_table %>%
                  distinct(raceId, driverId, forename, surname, 
                           race_date, location, country, name,
                           grid, position) %>%
                  mutate(f_position = position),
              by = c("raceId", "driverId")) %>%
    mutate(race_location = paste0(location, ", ", country),
           drv_name = iconv(paste(forename, surname), 
                            from = "UTF-8", to = "ASCII", sub = "")) -> lap_times

#Custom color pallette ####
my_pal <- pal_rickandmorty()(7)

#Application ####
header <- dashboardHeader(
    title = "Formula One Performance Analysis",
    titleWidth = 450
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Driver vs. Driver", tabName = "drv_v_drv", icon = icon("chart-line")),
        menuItem("Total Wins/Podiums by Agg", tabName = "wpa", icon = icon("chart-bar"))
        # selectizeInput("agg_grp", "Aggregate Selecrion",
        #                choices = c("All", "Butter" = "butter", "Cheese Barrel" = "cheese_barrel",
        #                            "Cheese Block" = "cheese_block", "Dry Milk" = "dry_milk",
        #                            "Dry Whey" = "dry_whey"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "drv_v_drv",
                fluidRow(
                    column(2, offset = 0, style = "padding:1px",
                           selectizeInput("r_loc", "Select Race Location:",
                                       choices = lap_times %>%
                                           arrange(desc(race_location)) %>%
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
                    box(plotlyOutput("dvd_line", height = 500), width = 12)
                )
        )
        # tabItem(tabName = "fore",
        #         fluidRow(
        #             numericInput("h_cast", "Forecast Training Window - n",
        #                          value = 2, min = 1, max = 12,
        #                          step = 1),
        #             actionButton("f_cast", "Forecast"),
        #             box(plotlyOutput("fore_gr", height = 750), width = 12)
        #         )
        # )
    )
)

ui <- dashboardPage(
    skin = "red",
    header,
    sidebar,
    body
)

server <- function(input, output, session) {
    
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
                                           pull(drv_name) %>%
                                           unique())})
    
    observeEvent(input$r_date,
                 {updateSelectizeInput(session, "drv_2",
                                       choices = lap_times %>%
                                           filter(race_location == input$r_loc,
                                                  race_date == ymd(input$r_date)) %>%
                                           arrange(f_position) %>%
                                           pull(drv_name) %>%
                                           unique())})
    
    race_table <- eventReactive(input$r_date,
                                {
                                    req(input$r_date)
                                    
                                    lap_times %>%
                                        filter(race_location == input$r_loc,
                                               race_date == ymd(input$r_date))
                                    
                                })
    
    output$dvd_line <- renderPlotly({
        
        race_table() %>%
            filter(drv_name %in% c(input$drv_1, input$drv_2)) %>%
            ggplot(aes(lap, l_time, col = factor(drv_name))) + geom_point() +
            scale_x_continuous(breaks = seq(0, 100, 10)) +
            geom_line() + labs(x = "Lap", y = "Lap Time", color = "Driver",
                               title = "Driver vs Driver Race Performance")
    })
    
    # pred_filter <- eventReactive(input$f_cast, {
    #     
    #     req(input$h_cast)
    #     
    #     pred_table <- data.frame("sale_date" = date("1/1/1"), "product" = NA, "price" = NA, "value_type" = NA)
    #     
    #     n <- ncol(avg_dairy_ts)
    #     i <- c()
    #     
    #     for(i in 1:n) {
    #         
    #         train <- window(avg_dairy_ts[, i], end = c(year(max_sale_date - months(input$h_cast)), 
    #                                                    month(max_sale_date - months(input$h_cast))))
    #         test <- window(avg_dairy_ts[, i], start = c(year(max_sale_date - months(input$h_cast - 1)), 
    #                                                     month(max_sale_date - months(input$h_cast - 1))))
    #         
    #         fit_arima <- auto.arima(train)
    #         fc1 <- forecast(fit_arima, h = input$h_cast, level = c(95))
    #         
    #         date(max_sale_date) -> pred_table[i, 1]
    #         dairy_products[i] -> pred_table[i, 2]
    #         as.vector(fc1$mean)[input$h_cast] -> pred_table[i, 3]
    #         "pred" -> pred_table[i, 4]
    #         
    #     }
    #     
    #     avg_dairy_tbl %>%
    #         bind_rows(pred_table) %>%
    #         filter(product == input$prod)
    #     
    # })
    # 
    # product_filter <- reactive({
    #     
    #     req(input$prod)
    #     
    #     dairy_data %>%
    #         filter(product == input$prod)
    #     
    # })
    # 
    # output$line_gr <- renderPlotly({
    #     
    #     req(input$prod)
    #     
    #     line_plot <- function(dat, metr) {
    #         
    #         dat %>%
    #             ggplot(aes(sale_date, {{ metr }}, col = product, group = 1,
    #                        text = paste("Product:", product, "<br>",
    #                                     "Sale Date:", sale_date, "<br>",
    #                                     "Metric:", {{ metr }}))) + geom_line() +
    #             scale_y_continuous(labels = comma) +
    #             labs(x = "Date", y = paste0(tools::toTitleCase(str_replace_all(input$metric, "_", " "))),
    #                  color = "Product", title = paste(tools::toTitleCase(str_replace_all(input$prod, "_", " ")), 
    #                                                   tools::toTitleCase(str_replace_all(input$metric, "_", " ")), "Trends")) + 
    #             scale_color_npg() + theme(plot.title = element_text(hjust = 0.5))
    #         
    #         ggplotly(tooltip = "text")
    #     }
    #     
    #     if(input$prod == "All") {
    #         
    #         line_plot(dairy_data, !!sym(input$metric))
    #         
    #     } else {
    #         
    #         line_plot(product_filter(), !!sym(input$metric))
    #         
    #     }
    #     
    # })
    # 
    # output$fore_gr <- renderPlotly({
    #     
    #     if(input$prod == "All") {
    #         
    #         ggplot(data = data.frame("Sale Date" = NA, "Price" = NA),
    #                aes("Sale Date", Price)) + geom_point() +
    #             labs(title = "Select Individual Dairy Product") +
    #             theme(plot.title = element_text(hjust = 0.5))
    #         
    #     } else {
    #         
    #         pred_filter() %>%
    #             filter(value_type == "actual") %>%
    #             ggplot(aes(sale_date, price)) + geom_line(col = my_pal[3]) +
    #             labs(title = "Actual vs. Predicted Price", x = "Sale Date", y = "Price") +
    #             scale_color_discrete(name = "") + 
    #             geom_point(data = pred_filter() %>% 
    #                            filter(value_type == "pred"),
    #                        aes(sale_date, price, col = "Forecast"), size = 3) +
    #             theme(plot.title = element_text(hjust = 0.5))
    #         
    #     }
    #     
    # })
    
}

shinyApp(ui, server)


