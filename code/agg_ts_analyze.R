########################################################
# Bank Runs Research Project
# Aggregate Time Series
# Author: Daniel Schwindt
# Date: 3/25/2023
# Purpose: Input aggregate time series data and perform
#          preliminary data analysis
########################################################

## 0. Housekeeping
rm(list=ls())
setwd("~/Documents/research_projects/bank_runs/code")

library(tidyverse)
library(readxl)
library(gridExtra)

## 1. Load Datasets
# Bank Suspensions/Failures (Post-FDIC and Pre-FDIC)
bank_failures_FDIC <- read.csv("../data/bank_failures_FDIC.csv", stringsAsFactors=F)
#bank_failures_preFDIC <- read.csv("../data/bank_suspensions_FRB_Sep1937.csv", stringsAsFactors=F)
bank_failures_preFDIC <- read.csv("../data/bank_failures_FRBulletin_1921-1934.csv", stringsAsFactors=F)

# Total banking institutions
bank_totals <- read.csv("../data/bank_totals_FDIC.csv", stringsAsFactors=F)

# Macro data series
macro_series_daily <- read_excel("../data/Bank_Runs_MacroSeries.xls", sheet="Daily")
macro_series_daily7 <- read_excel("../data/Bank_Runs_MacroSeries.xls", sheet="Daily,_7-Day", 
                                  col_types = c("date", rep("numeric",5)))
macro_series_monthly <- read_excel("../data/Bank_Runs_MacroSeries.xls", sheet="Monthly")
macro_series_qtrly <- read_excel("../data/Bank_Runs_MacroSeries.xls", sheet="Quarterly")

frb_h8 <- read.csv("../data/FRB_H8.csv", stringsAsFactors=F) # H8 aggregate assets & liabilities

## 2. Compute bank failures per quarter
bank_fails_preFDIC_q <- bank_failures_preFDIC %>%
  mutate(day = case_when(Month %in% c(1,3,5,7,8,10,12) ~ 31,
                         Month == 2 ~ 28,
                         TRUE ~ 30),
         date = ymd(paste(Year, Month, day, sep="-")),
         qtrdate = quarter(date, with_year=T),
         qtr = quarter(date)
         ) %>% group_by(qtrdate, qtr) %>%
  summarise(failures = sum(Bank.Suspensions)
            ) %>% ungroup()

bank_failures_FDIC <- bank_failures_FDIC %>% 
  mutate(date = as.Date(FAILDATE, format = "%m/%d/%Y"))

# Convert annual bank failures pre-FDIC to quarterly frequency
# start_date_preFDIC <- as.Date("1921-01-01")
# end_date_preFDIC <- as.Date("1933-12-31")
# qtrly_dates <- seq(start_date_preFDIC, end_date_preFDIC, by="quarter")
# bank_suspens_preFDIC_q <- approx(x=bank_failures_preFDIC$Year, 
#                                  y=bank_failures_preFDIC$Suspensions,
#                                  xout=as.numeric(format(qtrly_dates, "%Y")),
#                                  method = "constant")$y/4
# bank_failures_preFDIC_q <- data.frame(qtrly_dates, bank_suspens_preFDIC_q)
# names(bank_failures_preFDIC_q) <- c("date", "failures")
# bank_fails_preFDIC <- bank_failures_preFDIC_q %>% 
#   mutate(date = as.POSIXct(date),
#          qtrdate=quarter(as.POSIXct(date), with_year=TRUE),
#          qtr=quarter(as.POSIXct(date))) %>%
#   select(qtrdate, qtr, failures)

bank_fails_postFDIC <- bank_failures_FDIC %>% 
  mutate(qtrdate = quarter(date, with_year = TRUE), qtr = quarter(date)) %>%
  group_by(qtrdate, qtr) %>% 
  summarise(failures = n()) %>% ungroup()

# Concatenate failures data pre and post FDIC
bank_fails <- rbind(bank_fails_preFDIC_q, bank_fails_postFDIC)

bank_fails <- bank_fails %>% 
  mutate(year = floor(qtrdate),
         month = qtr*3,
         day = case_when(
           month < 6  ~ 31,
           month >= 6 & month < 9 ~ 30,
           month >= 9 & month < 12 ~ 30,
           TRUE ~ 31
         ),
         date = ymd(paste(year, month, day, sep = "-"))
  )

# Merge in number of banks data
bank_fails <- left_join(bank_fails, bank_totals, by=c("year"="YEAR"))
bank_fails <- bank_fails %>% select(qtrdate, qtr, failures, TOTAL, year, month, day, date)

bank_totals_preFDIC <- bank_failures_preFDIC %>% group_by(Year) %>%
  summarise(total_banks = mean(Number.of.Banks, na.rm=T))

bank_fails <- left_join(bank_fails, bank_totals_preFDIC, by=c("year"="Year"))
bank_fails <- bank_fails %>% 
  mutate(total = ifelse(is.na(TOTAL), total_banks, TOTAL),
         fail_rate = 100*failures/total) %>%
  select(date, failures, total, fail_rate)

## 4. Interest rates per quarter
# Correct issue with zeros as NAs
macro_series_daily7 <- macro_series_daily7 %>% 
  mutate(FFHTHIGH = ifelse(FFHTHIGH==0, NA, FFHTHIGH),
         FFHTLOW  = ifelse(FFHTLOW==0, NA, FFHTLOW),
         FFWSJHIGH = ifelse(FFWSJHIGH==0, NA, FFWSJHIGH),
         FFWSJLOW = ifelse(FFWSJLOW==0, NA, FFWSJLOW),
         FFR = ifelse(DATE > as.Date("1954-06-30"), 
                      DFF, 
                      rowMeans(select(., FFHTHIGH, FFHTLOW, FFWSJHIGH, FFWSJLOW), na.rm=T)
                      )
  )

ggplot(data=macro_series_daily7) +
  geom_line(aes(x=DATE, y=FFHTHIGH, color="blue")) + 
  geom_line(aes(x=DATE, y=FFWSJHIGH, color="red")) + 
  geom_line(aes(x=DATE, y=DFF, color="black")) +
  geom_line(aes(x=DATE, y=FFR, color="grey"))

# Convert to quarterly
ffr <- macro_series_daily7 %>% mutate(qtrdate = quarter(DATE, with_year=T), 
                                      qtr=quarter(DATE)) %>%
  group_by(qtrdate, qtr) %>% summarise(ffr_mean = mean(FFR, na.rm=T),
                                  ffr_med = median(FFR, na.rm=T),
                                  ffr_max = max(FFR, na.rm=T),
                                  ffr_min = min(FFR, na.rm=T)) %>%
  mutate(year = floor(qtrdate),
         month = qtr*3,
         day = case_when(
           month < 6  ~ 31,
           month >= 6 & month < 9 ~ 30,
           month >= 9 & month < 12 ~ 30,
           TRUE ~ 31
         ),
         date = ymd(paste(year, month, day, sep = "-"))
  )

## 5. Plot figures
xlimits = c("1934-01-01", "2023-04-01")
ffr_plot <- ggplot(data=ffr) +
  geom_line(aes(x=date, y=ffr_mean), color="black") + 
  geom_ribbon(aes(x=date, ymin=ffr_min, ymax=ffr_max), 
              fill="grey", alpha=0.5) +
  scale_x_date(limits=as.Date(xlimits, format="%Y-%m-%d")) + 
  ylab("Federal Funds Rate (%)") +
  theme_classic()

bank_fail_plot <- ggplot(data=bank_fails) + 
  geom_line(aes(x=date, y=failures), color="black") + 
  scale_x_date(limits=as.Date(xlimits, format="%Y-%m-%d")) +
  xlab("Date") + ylab("Bank Failures (#)") +
  theme_classic()

bank_fail_rate_plot <- ggplot(data=bank_fails) + 
  geom_line(aes(x=date, y=fail_rate), color="black") + 
  scale_x_date(limits=as.Date(xlimits, format="%Y-%m-%d")) +
  scale_y_continuous(limits=c(0,5)) +
  xlab("Date") + ylab("Bank Failures (%)") +
  theme_classic()

## 6. Plot Bank Failures vs. Interest Rates
grid.arrange(ffr_plot, bank_fail_plot, nrow=2)
bank_fails_view <- grid.arrange(ffr_plot, bank_fail_rate_plot, nrow=2)
ggsave("../output/bank_fails_history", plot=bank_fails_view, 
       device="eps", width=6.25, height=4)

## 7. Plot aggregate deposits for large and small banks
frb_h8 <- frb_h8 %>% mutate(date = as.Date(Time.Period),
                            tot_dep_chg = B1058NCBA/lag(B1058NCBA) - 1,
                            lgbank_dep_chg = B1058NLGA/lag(B1058NLGA) - 1,
                            smbank_dep_chg = B1058NSMA/lag(B1058NSMA) - 1)

deposits_plot <- ggplot(data=frb_h8) +
  geom_line(aes(x=date, y=tot_dep_chg), color="black") +
  geom_line(aes(x=date, y=lgbank_dep_chg), color="blue") +
  geom_line(aes(x=date, y=smbank_dep_chg), color="red") +
  scale_x_date(limits=as.Date(c("2006-01-01", "2010-04-01"))) +
  scale_y_continuous(limits=c(-0.025, 0.05))

#B1058NLGA	B1058NSMA