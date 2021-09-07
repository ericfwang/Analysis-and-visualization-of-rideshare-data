# ------------------------------------------------------------------------------------------
# Author: Eric Wang
# ------------------------------------------------------------------------------------------
# Header ------------------------------------------------------------------
# Path
rm(list = ls())
DATAROOT <-  "{PATH HERE}"
    
# Options
options(scipen = 999)
options(stringsAsFactors = FALSE)
options(mc.cores = 4)

# Packages
library(DBI)
library(tidycensus)
library(readr)
library(readxl)
library(parallel)
library(openxlsx)
library(stringr)
library(lubridate)
library(rlang)
library(purrr)
library(stringdist)
library(tidyr)
library(glue)
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(XML)
library(methods)
library(haven)
library(janitor)
library(dataCompareR)
library(furrr)
library(scales)
library(vctrs)
library(zoo)
library(fredr)
library(wbstats)
library(forcats)

# Analysis ---------------------------------------------------------------------
# Read in data
setwd(DATAROOT)
df_in <- read.csv("data.csv")
df_in <- as.data.frame(df_in) %>% 
    rename(login_date = df_in)

# Summarize by day
by_day <- df_in %>% 
    mutate(login_date = ymd(substr(login_date, 1, 10))) %>% 
    group_by(login_date) %>% 
    summarise(frequency = n())

# Summarize by day of week
by_day_of_week <- df_in %>% 
    mutate(login_date = weekdays(ymd(substr(login_date, 1, 10)))) %>% 
    group_by(login_date) %>% 
    summarise(frequency = n()) %>% 
    mutate(login_date = factor(login_date, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
    arrange(login_date) %>% 
    mutate(login_date = as.Date("2021-06-20") + days(1:7))

# Summarize by hour of day
by_hour <- df_in %>% 
    mutate(login_date = as.POSIXct(paste0("2021-06-20 ", substr(login_date, 12, 13), ":00:00"))) %>% 
    group_by(login_date) %>% 
    summarise(frequency = n())

# Scatter plots with time series regressions -----------------------------------
font_size <- 14
golden_ratio <- (1 + sqrt(5))/2

plot <- function(INPUT, X_AXIS, TITLE) {

    # Set y-axis scale
    ymax <- max(INPUT$frequency, na.rm = TRUE)*(golden_ratio*3/4)
    ymag <- 10^floor(log10(ymax))
    ystep <- if_else(ymax/ymag > 4, ymag, ymag/2)
    ysmax <- ceiling(ymax/ystep)*ystep
    
    # Format linear model statistics
    INPUT <- INPUT %>% mutate(interval = row_number() - 1)
    model <- lm(frequency ~ interval, data = INPUT)
    stats <- summary(model)
    note_text <- paste0("Equation: y = ", format(round(model$coefficients[2], 2), big.mark = ",", nsmall = 2),
                        "x + ", format(round(model$coefficients[1], 2), big.mark = ",", nsmall = 2), 
                        "\nStandard errors: x (", format(round(stats$coefficients[2, 2], 2), nsmall = 2), "), intercept (", format(round(stats$coefficients[1, 2], 2), nsmall = 2), ")",
                        "\nP-values: x (", format(round(stats$coefficients[2, 4], 2), nsmall = 2), "), intercept (", format(round(stats$coefficients[1, 4], 2), nsmall = 2), ")",
                        "\nR-squared: ", format(round(stats$r.squared, 2), nsmall = 2),
                        "\nAdjusted R-squared: ", format(round(stats$adj.r.squared, 2), nsmall = 2))
    
    # Graph
    plot <- ggplot(data = INPUT, aes(x = login_date, y = frequency)
        ) +
        geom_point( 
        ) +
        stat_smooth(
            method='lm', 
            se = T,
            level = 0.95,
            formula = 'y ~ x',
            show.legend = T,
            aes(colour = "Linear model with 95% confidence intervals")
        ) + 
        scale_y_continuous(
            limits = c(0, ysmax),
            breaks = seq(0, ysmax, ystep),
            expand = c(0, 0),
            labels = number_format(big.mark = ",")
        ) +
        theme(
            text = element_text(family = 'Helvetica', size = font_size, colour = "black"),
            
            plot.caption = element_text(hjust = 0),
            plot.title = element_text(hjust = 0.5, size = font_size + 4, face = "bold"),
            plot.subtitle = element_text(hjust = 0),
            plot.margin = unit(rep(1.5, 4), "cm"),
            
            panel.background = element_rect(fill = "white"),
            panel.border = element_blank(), 
            
            axis.line = element_line(),
            axis.text.x = element_text(colour = "black"),
            axis.text.y = element_text(colour = "black"),
            axis.title.y = element_text(angle = 0, vjust = 1.05, hjust = 0, colour = "black"),
            
            legend.text = element_text(size = font_size),
            legend.position = "bottom",
            legend.key = element_rect(fill = "white")
        ) +
        labs(
            title = paste0(TITLE, '\n', "3/1/2012-4/30/2012"),
            subtitle = paste0('Total number of logins'),
            y = '',
            x = X_AXIS,
            caption = note_text
        ) +
        scale_colour_manual(name = "", values = "blue")

    # Format the x-axes correctly
    if (X_AXIS == "Date") {
        plot <- plot + scale_x_date(
            date_breaks = '6 days',
            expand = c(0.02, 0.02),
            labels = function(x) gsub(" 0", " ", strftime(x, "%B %d"))) 
    }
    if (X_AXIS == "Day of Week") {
        plot <- plot + scale_x_date(
            date_breaks = 'day',
            expand = c(0.02, 0.02),
            date_labels = '%A') 
    }
    if (X_AXIS == "Hour") {
        plot <- plot + scale_x_datetime(
            date_breaks = 'hour',
            expand = c(0.02, 0.02),
            labels = function(x) gsub("^0", "", strftime(x, '%H:%M'))) 
    }
    plot
}

# Export the graphs as PDFs
ggsave(plot(by_day, "Date", "Login Frequency Over Time"), path = DATAROOT, filename = "long_term.pdf", height = 8, width = 8 * golden_ratio, encoding = "ISOLatin9.enc")
ggsave(plot(by_day_of_week, 'Day of Week', "Login Frequency by Day of Week"), path = DATAROOT, filename = "day_of_week.pdf", height = 8, width = 8 * golden_ratio, encoding = "ISOLatin9.enc")
ggsave(plot(by_hour, 'Hour', "Login Frequency by Hour of Day"), path = DATAROOT, filename = "hour.pdf", height = 8, width = 8 * golden_ratio, encoding = "ISOLatin9.enc")
