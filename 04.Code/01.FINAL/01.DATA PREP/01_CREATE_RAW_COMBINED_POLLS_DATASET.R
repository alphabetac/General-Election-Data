## PROJECT: SEAT PROJECTION TOOL
## TITLE: CREATE UNIFIED POLLS THING
## AUTHOR(S): Henry Johnston-Ellis
## DATE: Wed Oct 12 16:30:57 2022

## NOTES:

## SET CREDENTIALS --------------------

## LOAD LIBRARIES ---------------------

library(tidyverse)
library(here)

## LOAD DATA --------------------------

pollbase_46_50 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_46-50.csv"))
pollbase_50_51 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_50-51.csv"))
pollbase_51_55 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_51-55.csv"))
pollbase_55_59 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_55-59.csv"))
pollbase_59_64 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_59-64.csv"))
pollbase_64_66 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_64-66.csv"))
pollbase_66_70 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_66-70.csv"))
pollbase_70_74 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_70-74.csv"))
pollbase_74_74 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_74-74.csv"))
pollbase_74_79 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_74-79.csv"))
pollbase_79_83 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_79-83.csv"))
pollbase_83_87 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_83-87.csv"))
pollbase_87_92 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_87-92.csv"))
pollbase_92_97 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_92-97.csv"))
pollbase_97_01 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_97-01.csv"))
pollbase_01_05 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_01-05.csv"))
pollbase_05_10 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_05-10.csv"))
pollbase_10_15 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_10-15.csv"))
pollbase_15_17 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_15-17.csv"))
pollbase_17_19 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_17-19.csv"))
pollbase_19_22 <- read_csv(here("03.Data/01.Raw/pollbase/pollbase_19-22.csv"))

## TIDY DATA --------------------------

tidy_1 <- function(data) {
  
  return(
    data %>%
      fill(Year, Month) %>%
      separate(Fieldwork, into = c("start_day", "end_day"), sep = "-") %>%
      mutate(start_day = case_when(is.na(start_day) ~ 1,
                                   start_day == "?" ~ 1,
                                   start_day == "" ~ 1,
                                   TRUE ~ as.numeric(start_day)),
             start_date = as.Date(paste(start_day, Month, Year), format = "%d %b %Y"),
             end_date = as.Date(paste(end_day, Month, Year), format = "%d %b %Y"),
             pub_date = as.Date(paste(Date, Month, Year), format = "%d %b %Y")) %>%
      select(start_date, end_date, pub_date, Polling, Publisher, Con, Lab, LD)
  )
  
}

tidy_2 <- function(data) {
  
  return(
    data %>%
      fill(Year, Month) %>%
      separate(Fieldwork, into = c("start_day", "end_day"), sep = "-") %>%
      mutate(start_day = case_when(is.na(start_day) ~ 1,
                                   start_day == "?" ~ 1,
                                   start_day == "" ~ 1,
                                   TRUE ~ as.numeric(start_day)),
             start_date = as.Date(paste(start_day, Month, Year), format = "%d %b %Y"),
             end_date = as.Date(paste(end_day, Month, Year), format = "%d %b %Y"),
             pub_date = as.Date(paste(Date, Month, Year), format = "%d %b %Y")) %>%
      select(start_date, end_date, pub_date, Polling, Publisher, Con, Lab, LD, SDP, Green)
  )
  
}

tidy_3 <- function(data) {
  
  return(
    data %>%
      fill(Year, Month) %>%
      separate(Fieldwork, into = c("start_day", "end_day"), sep = "-") %>%
      mutate(start_day = case_when(is.na(start_day) ~ 1,
                                   start_day == "?" ~ 1,
                                   start_day == "" ~ 1,
                                   TRUE ~ as.numeric(start_day)),
             start_date = as.Date(paste(start_day, Month, Year), format = "%d %b %Y"),
             end_date = as.Date(paste(end_day, Month, Year), format = "%d %b %Y"),
             pub_date = as.Date(paste(Date, Month, Year), format = "%d %b %Y")) %>%
      select(start_date, end_date, pub_date, Polling, Publisher, Con, Lab, LD, Referendum)
  )
  
}

tidy_4 <- function(data) {
  
  return(
    data %>%
      fill(Year, Month) %>%
      separate(Fieldwork, into = c("start_day", "end_day"), sep = "-") %>%
      mutate(start_day = case_when(is.na(start_day) ~ 1,
                                   start_day == "?" ~ 1,
                                   start_day == "" ~ 1,
                                   TRUE ~ as.numeric(start_day)),
             start_date = as.Date(paste(start_day, Month, Year), format = "%d %b %Y"),
             end_date = as.Date(paste(end_day, Month, Year), format = "%d %b %Y"),
             pub_date = as.Date(paste(Published, Month, Year), format = "%d %b %Y")) %>%
      select(start_date, end_date, pub_date, Polling, Publisher, Con, Lab, LD)
  )
  
}

tidy_5 <- function(data) {
  
  return(
    data %>%
      fill(Year) %>%
      separate(Fieldwork, into = c("start_day", "end_day"), sep = "-") %>%
      rename(field_start = `...4`,
             method = `...25`) %>%
      mutate(start_day = case_when(is.na(start_day) ~ 1,
                                   start_day == "?" ~ 1,
                                   start_day == "" ~ 1,
                                   TRUE ~ as.numeric(start_day)),
             start_date = as.Date(paste(start_day, Month, Year), format = "%d %b %Y"),
             end_date = as.Date(paste(end_day, Month, Year), format = "%d %b %Y"),
             pub_date = as.Date(paste(Published, Month, Year), format = "%d %b %Y")) %>%
      select(start_date, end_date, pub_date, Polling, Publisher, Con, Lab, LD, method)
  )
  
}

tidy_6 <- function(data) {
  
  return(
    data %>%
      fill(Year) %>%
      separate(Fieldwork, into = c("start_day", "end_day"), sep = "-") %>%
      rename(field_start = `...4`,
             method = `...16`) %>%
      mutate(start_day = case_when(is.na(start_day) ~ 1,
                                   start_day == "?" ~ 1,
                                   start_day == "" ~ 1,
                                   TRUE ~ as.numeric(start_day)),
             start_date = as.Date(paste(start_day, Month, Year), format = "%d %b %Y"),
             end_date = as.Date(paste(end_day, Month, Year), format = "%d %b %Y"),
             pub_date = as.Date(paste(Published, Month, Year), format = "%d %b %Y")) %>%
      select(start_date, end_date, pub_date, Polling, Publisher, Con, Lab, LD, UKIP, Green, BNP, method)
  )
  
}

tidy_7 <- function(data) {
  
  return(
    data %>%
      fill(Year) %>%
      separate(Fieldwork, into = c("start_day", "end_day"), sep = "-") %>%
      rename(field_start = `...4`,
             method = `...20`) %>%
      mutate(start_day = case_when(is.na(start_day) ~ 1,
                                   start_day == "?" ~ 1,
                                   start_day == "" ~ 1,
                                   TRUE ~ as.numeric(start_day)),
             start_date = as.Date(paste(start_day, Month, Year), format = "%d %b %Y"),
             end_date = as.Date(paste(end_day, Month, Year), format = "%d %b %Y"),
             pub_date = as.Date(paste(Published, Month, Year), format = "%d %b %Y")) %>%
      select(start_date, end_date, pub_date, Polling, Publisher, Con, Lab, LD, UKIP, Green, method)
  )
  
}

tidy_8 <- function(data) {
  
  return(
    data %>%
      fill(Year) %>%
      separate(Fieldwork, into = c("start_day", "end_day"), sep = "-") %>%
      rename(field_start = `...4`,
             method = `...24`) %>%
      mutate(start_day = case_when(is.na(start_day) ~ 1,
                                   start_day == "?" ~ 1,
                                   start_day == "" ~ 1,
                                   TRUE ~ as.numeric(start_day)),
             start_date = as.Date(paste(start_day, Month, Year), format = "%d %b %Y"),
             end_date = as.Date(paste(end_day, Month, Year), format = "%d %b %Y"),
             pub_date = as.Date(paste(Published, Month, Year), format = "%d %b %Y")) %>%
      select(start_date, end_date, pub_date, Polling, Publisher, Con, Lab, LD, UKIP, Green, `TIG/CUK`, BXP, method)
  )
  
}

tidy_9 <- function(data) {
  
  return(
    data %>%
      fill(Year) %>%
      separate(Fieldwork, into = c("start_day", "end_day"), sep = "-") %>%
      rename(field_start = `...4`,
             method = `...22`) %>%
      mutate(start_day = case_when(is.na(start_day) ~ 1,
                                   start_day == "?" ~ 1,
                                   start_day == "" ~ 1,
                                   TRUE ~ as.numeric(start_day)),
             start_date = as.Date(paste(start_day, Month, Year), format = "%d %b %Y"),
             end_date = as.Date(paste(end_day, Month, Year), format = "%d %b %Y"),
             pub_date = as.Date(paste(Published, Month, Year), format = "%d %b %Y")) %>%
      select(start_date, end_date, pub_date, Polling, Publisher, Con, Lab, LD, Ukip, Green, `BXP/Reform`, method)
  )
  
}

pollbase_46_50_tidy <- tidy_1(pollbase_46_50)
pollbase_50_51_tidy <- tidy_1(pollbase_50_51)
pollbase_51_55_tidy <- tidy_1(pollbase_51_55)
pollbase_55_59_tidy <- tidy_1(pollbase_55_59)
pollbase_59_64_tidy <- tidy_1(pollbase_59_64)
pollbase_64_66_tidy <- tidy_1(pollbase_64_66)
pollbase_66_70_tidy <- tidy_1(pollbase_66_70)
pollbase_70_74_tidy <- tidy_1(pollbase_70_74)
pollbase_74_74_tidy <- tidy_1(pollbase_74_74)
pollbase_74_79_tidy <- tidy_1(pollbase_74_79)
pollbase_79_83_tidy <- tidy_1(pollbase_79_83)
pollbase_83_87_tidy <- tidy_2(pollbase_83_87)
pollbase_87_92_tidy <- tidy_2(pollbase_87_92)
pollbase_92_97_tidy <- tidy_3(pollbase_92_97)
pollbase_97_01_tidy <- tidy_1(pollbase_97_01)
pollbase_01_05_tidy <- tidy_4(pollbase_01_05)
pollbase_05_10_tidy <- tidy_5(pollbase_05_10)
pollbase_10_15_tidy <- tidy_6(pollbase_10_15)
pollbase_15_17_tidy <- tidy_7(pollbase_15_17)
pollbase_17_19_tidy <- tidy_8(pollbase_17_19)
pollbase_19_22_tidy <- tidy_9(pollbase_19_22)

## JOIN DATA --------------------------

combined_raw_polls <- pollbase_46_50_tidy %>%
  add_row(pollbase_50_51_tidy) %>%
  add_row(pollbase_51_55_tidy) %>%
  add_row(pollbase_55_59_tidy) %>%
  add_row(pollbase_59_64_tidy) %>%
  add_row(pollbase_64_66_tidy) %>%
  add_row(pollbase_66_70_tidy) %>%
  add_row(pollbase_70_74_tidy) %>%
  add_row(pollbase_74_74_tidy) %>%
  add_row(pollbase_74_79_tidy) %>%
  add_row(pollbase_79_83_tidy) %>%
  mutate(SDP = NA,
         Green = NA) %>%
  add_row(pollbase_83_87_tidy) %>%
  add_row(pollbase_87_92_tidy) %>%
  mutate(Referendum = as.numeric(NA)) %>%
  add_row(pollbase_92_97_tidy %>% mutate(SDP = as.numeric(NA), Green = as.numeric(NA))) %>%
  add_row(pollbase_97_01_tidy %>% mutate(SDP = as.numeric(NA), Green = as.numeric(NA), Referendum = as.numeric(NA))) %>%
  add_row(pollbase_01_05_tidy %>% mutate(SDP = as.numeric(NA), Green = as.numeric(NA), Referendum = as.numeric(NA))) %>%
  mutate(method = NA) %>%
  add_row(pollbase_05_10_tidy %>% mutate(SDP = as.numeric(NA), Green = as.numeric(NA), Referendum = as.numeric(NA))) %>%
  mutate(UKIP = NA, BNP = NA) %>%
  add_row(pollbase_10_15_tidy %>% mutate(SDP = as.numeric(NA), Referendum = as.numeric(NA))) %>%
  add_row(pollbase_15_17_tidy %>% mutate(SDP = as.numeric(NA), Referendum = as.numeric(NA))) %>%
  mutate(`TIG/CUK` = NA, BXP = NA) %>%
  add_row(pollbase_17_19_tidy %>% mutate(SDP = as.numeric(NA), Referendum = as.numeric(NA))) %>%
  mutate(`BXP/Reform` = NA) %>%
  add_row(pollbase_19_22_tidy %>% rename(UKIP = Ukip) %>% mutate(BNP = NA, BXP = NA, `TIG/CUK` = NA))

## SAVE DATA --------------------------

combined_raw_polls %>%
  write_csv("03.Data/02.Interim/combined_raw_polls.csv")
