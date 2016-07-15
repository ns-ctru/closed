# Load libraries
library(datasets)
library(dplyr)
library(ggplot2)
library(ggseas)
library(magrittr)
library(shiny)
library(RColorBrewer)
# Define Server Output
shinyServer(function(input, output){
    ##################################################################
    ## Sample graphs                                                ##
    ##################################################################
    ## seasonal figures using UKDriverDeaths from dataset package
    ukdeaths_df <- tapply(UKDriverDeaths,
                          list(year = floor(time(UKDriverDeaths)),
                               month = month.abb[cycle(UKDriverDeaths)]), c) %>%
        data.frame()
    ukdeaths_df$year <- rownames(ukdeaths_df)
    ukdeaths_df <- tidyr::gather(ukdeaths_df, key = year)
    names(ukdeaths_df) <- c("year", "month_text", "n")
    ukdeaths_df$month <- "01"
    ukdeaths_df <- within(ukdeaths_df, {
        month[month_text == "Jan"] <- "01"
        month[month_text == "Feb"] <- "02"
        month[month_text == "Mar"] <- "03"
        month[month_text == "Apr"] <- "04"
        month[month_text == "May"] <- "05"
        month[month_text == "Jun"] <- "06"
        month[month_text == "Jul"] <- "07"
        month[month_text == "Aug"] <- "08"
        month[month_text == "Sep"] <- "09"
        month[month_text == "Oct"] <- "10"
        month[month_text == "Nov"] <- "11"
        month[month_text == "Dec"] <- "12"
        date <- paste(year, month, "01", sep = "-") %>% ymd()
    })
    ukdeaths_df <- dplyr::select(ukdeaths_df, date, n)
    ## Now split the counts into two for male and female
    ukdeaths_df$male <- (ukdeaths_df$n * runif(n = nrow(ukdeaths_df))) %>%
        round()                    
    ukdeaths_df$female <- ukdeaths_df$n - ukdeaths_df$male
    ukdeaths_df <- tidyr::gather(data = ukdeaths_df,
                                 key  = date)
    names(ukdeaths_df) <- c("date", "group", "n")
    ## Simple plot of all deaths
    output$all <- renderPlot(
        ggplot(filter(ukdeaths_df,
                      group == "n"),
               aes(x = date, y = n)) +
        geom_line(colour = "grey80") +
        stat_seas(start = c(1969, 1), frequency = 12) +
        ggtitle("UK Road Casulaties 1969-1984") +
        ylab("Deaths") + xlab("Date") + theme_tufte()
    )
    ## Plot by gender  (overlay)
    output$overlay <- renderPlot(
    ggplot(filter(ukdeaths_df,
                  group != "n"),
           aes(x = date, y = n, color = group)) +
        geom_line(colour = "grey80") +
        stat_seas(start = c(1969, 1), frequency = 12) +
        ggtitle("UK Road Casulaties 1969-1984") +
    ylab("Deaths") + xlab("Date") + theme_tufte()
    )
    ## Plot by gender (facet)
    output$facet <- renderPlot(
        ggplot(filter(ukdeaths_df,
                      group != "n"),
               aes(x = date, y = n, color = group)) +
        geom_line(colour = "grey80") +
        stat_seas(start = c(1969, 1), frequency = 12) +
        ggtitle("UK Road Casulaties 1969-1984") +
        ylab("Deaths") + xlab("Date") + theme_tufte() +
        facet_wrap(facets = "group") + theme(legend.position = "none")
    )
    ## Reactive plot that depends on facet/overlay options
    ## output$test <- renderPlot(
    ##     t <- ggplot(filter(ukdeaths_df,
    ##                        group != "n"),
    ##                 aes(x = date, y = n))
    ##     if(input$plot.by$overlay == TRUE){
    ##         t <- t + geom_line(colour = "grey80", aes(color = "group"))
    ##             }
    ##     else{
    ##         t <- t + geom_line(colour = "grey80")
    ##     }
    ##     t <- stat_seas(start = c(1969, 1), frequency = 12) +
    ##         ggtitle("UK Road Casulaties 1969-1984") +
    ##         ylab("Deaths") + xlab("Date") + theme_tufte() +
    ##         theme(legend.position = "none")
    ##     if(input$plot.by$facet == TRUE){
    ##         t <- t + facet_wrap(facets = "group")
    ##     }
    ##     return(t)
    ## )
    ##################################################################
    ## Summary of data                                              ##
    ##################################################################

    ##################################################################
    ## Mortality - Rate                                             ##
    ##################################################################

    ##################################################################
    ## Mortality - Case Fatality Ratio                              ##
    ##################################################################

})
