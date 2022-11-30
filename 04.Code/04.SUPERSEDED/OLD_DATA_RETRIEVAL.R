## PROJECT:
## TITLE: GET POLLING DATA AND TIDY
## AUTHOR(S): Henry Johnston-Ellis
## DATE: Thu Oct 06 16:45:19 2022

## NOTES:

## SET CREDENTIALS --------------------

## LOAD LIBRARIES ---------------------

library(tidyverse)
library(httr)
library(XML)
library(glue)
library(lubridate)

## LOAD DATA --------------------------

## GET HTML FOR POLLS DATA PAGES --

get_table_function <- function(url) {
  
  page <- GET(url)
  table <- readHTMLTable(doc = content(page, "text"))
  return(table)
  
}

ge_polls_2024_html_table <- get_table_function("https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_United_Kingdom_general_election")
ge_polls_2019_html_table <- get_table_function("https://en.wikipedia.org/wiki/Opinion_polling_for_the_2019_United_Kingdom_general_election")
ge_polls_2017_html_table <- get_table_function("https://en.wikipedia.org/wiki/Opinion_polling_for_the_2017_United_Kingdom_general_election")
ge_polls_2015_html_table <- get_table_function("https://en.wikipedia.org/wiki/Opinion_polling_for_the_2015_United_Kingdom_general_election")
ge_polls_2010_html_table <- get_table_function("https://en.wikipedia.org/wiki/Opinion_polling_for_the_2010_United_Kingdom_general_election")

## CLEAN DATA -------------------------

ge_polls_2022 <- ge_polls_2024_html_table[[2]] %>%
  tibble() %>%
  rename(dates_conducted = V1,
         pollster = V2,
         client = V3,
         area = V4,
         sample_size = V5,
         conservative = V6,
         labour = V7,
         liberal_democrat = V8,
         scottish_national_party = V9,
         green = V10,
         reform = V11,
         others = V12,
         lead = V13) %>%
  drop_na(labour) %>%
  filter(dates_conducted != "Datesconducted") %>%
  separate(dates_conducted,
           into = c("poll_start", "poll_finish"),
           sep = "–",
           remove = FALSE) %>%
  mutate(sample_size = as.numeric(str_replace(sample_size, ",", "")),
         across(.cols = c(conservative, labour, liberal_democrat,
                          scottish_national_party, green, reform, lead),
                ~ as.numeric(str_extract(.x, pattern = "\\d+")) / 100),
         start_date = 
           case_when(
             !str_detect(poll_start, "[A-Za-z]{3} {0,1}$") ~
               paste(poll_start, str_extract(dates_conducted, "[A-Za-z]{3}$")),
             TRUE ~ poll_start
           ),
         end_date = 
           case_when(
             is.na(poll_finish) ~ as.character(poll_start),
             TRUE ~ as.character(str_extract(poll_finish, "\\d{1,2} [A-Za-z]{3}"))
           ),
         start_date = as.Date(start_date, format = "%d %b"),
         end_date = as.Date(end_date, format = "%d %b"),
         start_date = `year<-`(start_date, 2022),
         end_date = `year<-`(end_date, 2022),
         client = case_when(client == "N/A" ~ as.character(NA),
                            TRUE ~ client)) %>%
  rowwise() %>%
  mutate(other = 1 - sum(conservative, green, labour, liberal_democrat,
                         reform, scottish_national_party, na.rm = T)) %>%
  select(start_date, end_date, pollster, client, area, sample_size, conservative,
         labour, liberal_democrat, scottish_national_party, green, reform, other)

ge_polls_2021 <- ge_polls_2024_html_table[[3]] %>%
  tibble() %>%
  rename(dates_conducted = V1,
         pollster = V2,
         client = V3,
         area = V4,
         sample_size = V5,
         conservative = V6,
         labour = V7,
         liberal_democrat = V8,
         scottish_national_party = V9,
         green = V10,
         reform = V11,
         others = V12,
         lead = V13) %>%
  drop_na(labour) %>%
  filter(dates_conducted != "Datesconducted") %>%
  separate(dates_conducted,
           into = c("poll_start", "poll_finish"),
           sep = "–",
           remove = FALSE) %>%
  mutate(sample_size = as.numeric(str_replace(sample_size, ",", "")),
         across(.cols = c(conservative, labour, liberal_democrat,
                          scottish_national_party, green, reform, lead),
                ~ as.numeric(str_extract(.x, pattern = "\\d+")) / 100),
         start_date = 
           case_when(
             !str_detect(poll_start, "[A-Za-z]{3} {0,1}$") ~
               paste(poll_start, str_extract(dates_conducted, "[A-Za-z]{3}$")),
             TRUE ~ poll_start
           ),
         end_date = 
           case_when(
             is.na(poll_finish) ~ as.character(poll_start),
             TRUE ~ as.character(str_extract(poll_finish, "\\d{1,2} [A-Za-z]{3}"))
           ),
         start_date = as.Date(start_date, format = "%d %b"),
         end_date = as.Date(end_date, format = "%d %b"),
         start_date = `year<-`(start_date, 2021),
         end_date = `year<-`(end_date, 2021),
         client = case_when(client == "N/A" ~ as.character(NA),
                            TRUE ~ client)) %>%
  rowwise() %>%
  mutate(other = 1 - sum(conservative, green, labour, liberal_democrat,
                         reform, scottish_national_party, na.rm = T)) %>%
  select(start_date, end_date, pollster, client, area, sample_size, conservative,
         labour, liberal_democrat, scottish_national_party, green, reform, other)

ge_polls_2020 <- ge_polls_2024_html_table[[4]] %>%
  tibble() %>%
  rename(dates_conducted = V1,
         pollster = V2,
         client = V3,
         area = V4,
         sample_size = V5,
         conservative = V6,
         labour = V7,
         liberal_democrat = V8,
         scottish_national_party = V9,
         green = V10,
         reform = V11,
         others = V12,
         lead = V13) %>%
  drop_na(labour) %>%
  filter(dates_conducted != "Datesconducted") %>%
  separate(dates_conducted,
           into = c("poll_start", "poll_finish"),
           sep = "–",
           remove = FALSE) %>%
  mutate(sample_size = as.numeric(str_replace(sample_size, ",", "")),
         across(.cols = c(conservative, labour, liberal_democrat,
                          scottish_national_party, green, reform, lead),
                ~ as.numeric(str_extract(.x, pattern = "\\d+")) / 100),
         start_date = 
           case_when(
             !str_detect(poll_start, "[A-Za-z]{3} {0,1}$") ~
               paste(poll_start, str_extract(dates_conducted, "[A-Za-z]{3}$")),
             TRUE ~ poll_start
           ),
         end_date = 
           case_when(
             is.na(poll_finish) ~ as.character(poll_start),
             TRUE ~ as.character(str_extract(poll_finish, "\\d{1,2} [A-Za-z]{3}"))
           ),
         start_date = as.Date(start_date, format = "%d %b"),
         end_date = as.Date(end_date, format = "%d %b"),
         start_date = `year<-`(start_date, 2020),
         end_date = `year<-`(end_date, 2020),
         client = case_when(client == "N/A" ~ as.character(NA),
                            TRUE ~ client)) %>%
  rowwise() %>%
  mutate(other = 1 - sum(conservative, green, labour, liberal_democrat,
                         reform, scottish_national_party, na.rm = T)) %>%
  select(start_date, end_date, pollster, client, area, sample_size, conservative,
         labour, liberal_democrat, scottish_national_party, green, reform, other) %>%
  filter(!pollster %in% c("2019 general election", "GB"))

ge_polls_2019 <- ge_polls_2019_html_table[[2]] %>%
  tibble() %>%
  rename(pollster_client = V1, dates_conducted = V2, area = V3,
         sample_size = V4, conservative = V5, labour = V6,
         liberal_democrat = V7, scottish_national_party = V8, 
         plaid_cymru = V9, green = V10, brexit = V11, ukip = V12,
         change_uk = V13, other = V14, lead = V15) %>%
  drop_na(labour) %>%
  filter(!pollster_client %in% c("Pollster/client(s)", "2019 general election",
                                 "GB", "", "12-15 Mar")) %>%
  separate(dates_conducted,
           into = c("poll_start", "poll_finish"),
           sep = "–",
           remove = FALSE) %>%
  mutate(sample_size = as.numeric(str_replace(sample_size, ",", "")),
         across(.cols = c(conservative, labour, liberal_democrat,
                          scottish_national_party, plaid_cymru, green,
                          brexit, ukip, change_uk),
                ~ as.numeric(str_extract(.x, pattern = "\\d+")) / 100),
         start_date = 
           case_when(
             !str_detect(poll_start, "[A-Za-z]{3} {0,1}$") ~
               paste(poll_start, str_extract(dates_conducted, "[A-Za-z]{3}$")),
             TRUE ~ poll_start
           ),
         end_date = 
           case_when(
             is.na(poll_finish) ~ as.character(poll_start),
             TRUE ~ as.character(str_extract(poll_finish, "\\d{1,2} [A-Za-z]{3}"))
           ),
         start_date = as.Date(paste(start_date, "2019"), format = "%d %b %Y"),
         end_date = as.Date(paste(end_date, "2019"), format = "%d %b %Y"),
         start_date = case_when(start_date == "2019-12-21" ~ as.Date("2018-12-21"),
                                TRUE ~ start_date)) %>%
  rowwise() %>%
  mutate(other = round(1 - sum(brexit, change_uk, conservative, green, labour,
                         liberal_democrat, plaid_cymru,
                         scottish_national_party, ukip, na.rm = T), 2)) %>%
  select(start_date, end_date, pollster_client, area, sample_size, brexit,
         change_uk, conservative, green, labour, liberal_democrat,
         plaid_cymru, scottish_national_party, ukip, other) %>%
  drop_na(start_date)

ge_polls_2018 <- ge_polls_2019_html_table[[3]] %>%
  tibble() %>%
  rename(pollster_client = V1, dates_conducted = V2, area = V3,
         sample_size = V4, conservative = V5, labour = V6,
         liberal_democrat = V7, scottish_national_party = V8, 
         plaid_cymru = V9, ukip = V10, green = V11, other = V12,
         lead = V13) %>%
  drop_na(labour) %>%
  filter(!pollster_client %in% c("Pollster/client(s)", "2019 general election",
                                 "GB", "", "12-15 Mar")) %>%
  separate(dates_conducted,
           into = c("poll_start", "poll_finish"),
           sep = "–",
           remove = FALSE) %>%
  mutate(sample_size = as.numeric(str_replace(sample_size, ",", "")),
         across(.cols = c(conservative, labour, liberal_democrat,
                          scottish_national_party, plaid_cymru, green, ukip),
                ~ as.numeric(str_extract(.x, pattern = "\\d+")) / 100),
         start_date = case_when(!str_detect(poll_start, "[A-Za-z]{3} {0,1}$") ~
                                  paste(poll_start, str_extract(dates_conducted, "[A-Za-z]{3}$")),
                                TRUE ~ poll_start),
         end_date = case_when(is.na(poll_finish) ~ as.character(poll_start),
                              TRUE ~ as.character(str_extract(poll_finish, "\\d{1,2} [A-Za-z]{3}"))),
         start_date = as.Date(paste(start_date, "2018"), format = "%d %b %Y"),
         end_date = as.Date(paste(end_date, "2018"), format = "%d %b %Y")) %>%
  rowwise() %>%
  mutate(other = round(1 - sum(conservative, green, labour, liberal_democrat,
                               plaid_cymru, scottish_national_party, ukip,
                               na.rm = T), 2)) %>%
  select(start_date, end_date, pollster_client, area, sample_size, conservative,
         green, labour, liberal_democrat, plaid_cymru, scottish_national_party,
         ukip, other) %>%
  drop_na(start_date)
  
ge_polls_2017_part_2 <- ge_polls_2019_html_table[[4]] %>%
  tibble() %>%
  rename(pollster_client = V1, dates_conducted = V2, area = V3,
         sample_size = V4, conservative = V5, labour = V6,
         liberal_democrat = V7, scottish_national_party = V8, 
         plaid_cymru = V9, ukip = V10, green = V11, other = V12,
         lead = V13) %>%
  drop_na(labour) %>%
  filter(!pollster_client %in% c("Pollster/client(s)", "2019 general election",
                                 "GB", "", "12-15 Mar")) %>%
  separate(dates_conducted,
           into = c("poll_start", "poll_finish"),
           sep = "–",
           remove = FALSE) %>%
  mutate(sample_size = as.numeric(str_replace(sample_size, ",", "")),
         across(.cols = c(conservative, labour, liberal_democrat,
                          scottish_national_party, plaid_cymru, green, ukip),
                ~ as.numeric(str_extract(.x, pattern = "\\d+")) / 100),
         start_date = case_when(!str_detect(poll_start, "[A-Za-z]{3} {0,1}$") ~
                                  paste(poll_start, str_extract(dates_conducted, "[A-Za-z]{3}$")),
                                TRUE ~ poll_start),
         end_date = case_when(is.na(poll_finish) ~ as.character(poll_start),
                              TRUE ~ as.character(str_extract(poll_finish, "\\d{1,2} [A-Za-z]{3}"))),
         start_date = as.Date(paste(start_date, "2017"), format = "%d %b %Y"),
         end_date = as.Date(paste(end_date, "2017"), format = "%d %b %Y")) %>%
  rowwise() %>%
  mutate(other = round(1 - sum(conservative, green, labour, liberal_democrat,
                               plaid_cymru, scottish_national_party, ukip,
                               na.rm = T), 2)) %>%
  select(start_date, end_date, pollster_client, area, sample_size, conservative,
         green, labour, liberal_democrat, plaid_cymru, scottish_national_party,
         ukip, other) %>%
  drop_na(start_date) %>%
  filter(pollster_client != "2017 general election")

ge_polls_2017_part_1 <- ge_polls_2017_html_table[[2]] %>%
  tibble() %>%
  rename(dates_conducted = V1, pollster_client = V2, sample_size = V3,
         conservative = V4, labour = V5, ukip = V6,
         liberal_democrat = V7, scottish_national_party = V8, 
         green = V9, other = V10, lead = V11) %>%
  drop_na(labour) %>%
  filter(!pollster_client %in% c("Polling organisation/client",
                                 "General Election results (GB only) [2]")) %>%
  separate(dates_conducted,
           into = c("poll_start", "poll_finish"),
           sep = "–",
           remove = FALSE) %>%
  mutate(sample_size = as.numeric(str_replace(sample_size, ",", "")),
         across(.cols = c(conservative, labour, liberal_democrat,
                          scottish_national_party, green, ukip),
                ~ as.numeric(str_extract(.x, pattern = "\\d+")) / 100),
         start_date = case_when(!str_detect(poll_start, "[A-Za-z]{3} {0,1}$") ~
                                  paste(poll_start, str_extract(dates_conducted, "[A-Za-z]{3}$")),
                                TRUE ~ poll_start),
         end_date = case_when(is.na(poll_finish) ~ as.character(poll_start),
                              TRUE ~ as.character(str_extract(poll_finish, "\\d{1,2} [A-Za-z]{3}"))),
         start_date = as.Date(paste(start_date, "2017"), format = "%d %b %Y"),
         end_date = as.Date(paste(end_date, "2017"), format = "%d %b %Y")) %>%
  rowwise() %>%
  mutate(other = round(1 - sum(conservative, green, labour, liberal_democrat,
                               scottish_national_party, ukip,
                               na.rm = T), 2)) %>%
  select(start_date, end_date, pollster_client, sample_size, conservative,
         green, labour, liberal_democrat, scottish_national_party,
         ukip, other) %>%
  drop_na(start_date) %>%
  filter(pollster_client != "2017 general election")

ge_polls_2016 <- ge_polls_2017_html_table[[3]] %>%
  tibble() %>%
  rename(dates_conducted = V1, pollster_client = V2, sample_size = V3,
         conservative = V4, labour = V5, ukip = V6,
         liberal_democrat = V7, scottish_national_party = V8, 
         green = V9, other = V10, lead = V11) %>%
  drop_na(labour) %>%
  filter(!pollster_client %in% c("Polling organisation/client",
                                 "General Election results (GB only) [2]")) %>%
  separate(dates_conducted,
           into = c("poll_start", "poll_finish"),
           sep = "–",
           remove = FALSE) %>%
  mutate(sample_size = as.numeric(str_replace(sample_size, ",", "")),
         across(.cols = c(conservative, labour, liberal_democrat,
                          scottish_national_party, green, ukip),
                ~ as.numeric(str_extract(.x, pattern = "\\d+")) / 100),
         start_date = case_when(!str_detect(poll_start, "[A-Za-z]{3} {0,1}$") ~
                                  paste(poll_start, str_extract(dates_conducted, "[A-Za-z]{3}$")),
                                TRUE ~ poll_start),
         end_date = case_when(is.na(poll_finish) ~ as.character(poll_start),
                              TRUE ~ as.character(str_extract(poll_finish, "\\d{1,2} [A-Za-z]{3}"))),
         start_date = as.Date(paste(start_date, "2016"), format = "%d %b %Y"),
         end_date = as.Date(paste(end_date, "2016"), format = "%d %b %Y")) %>%
  rowwise() %>%
  mutate(other = round(1 - sum(conservative, green, labour, liberal_democrat,
                               scottish_national_party, ukip,
                               na.rm = T), 2)) %>%
  select(start_date, end_date, pollster_client, sample_size, conservative,
         green, labour, liberal_democrat, scottish_national_party,
         ukip, other) %>%
  drop_na(start_date)

ge_polls_2015_part_2 <- ge_polls_2017_html_table[[4]] %>%
  tibble() %>%
  rename(dates_conducted = V1, pollster_client = V2, sample_size = V3,
         conservative = V4, labour = V5, ukip = V6,
         liberal_democrat = V7, scottish_national_party = V8, 
         green = V9, other = V10, lead = V11) %>%
  drop_na(labour) %>%
  filter(!pollster_client %in% c("Polling organisation/client", "",
                                 "General Election results (GB only)[6][7]")) %>%
  separate(dates_conducted,
           into = c("poll_start", "poll_finish"),
           sep = "–",
           remove = FALSE) %>%
  mutate(sample_size = as.numeric(str_replace(sample_size, ",", "")),
         across(.cols = c(conservative, labour, liberal_democrat,
                          scottish_national_party, green, ukip),
                ~ as.numeric(str_extract(.x, pattern = "\\d+")) / 100),
         start_date = case_when(!str_detect(poll_start, "[A-Za-z]{3} {0,1}$") ~
                                  paste(poll_start, str_extract(dates_conducted, "[A-Za-z]{3}$")),
                                TRUE ~ poll_start),
         end_date = case_when(is.na(poll_finish) ~ as.character(poll_start),
                              TRUE ~ as.character(str_extract(poll_finish, "\\d{1,2} [A-Za-z]{3}"))),
         start_date = as.Date(paste(start_date, "2015"), format = "%d %b %Y"),
         end_date = as.Date(paste(end_date, "2015"), format = "%d %b %Y")) %>%
  rowwise() %>%
  mutate(other = round(1 - sum(conservative, green, labour, liberal_democrat,
                               scottish_national_party, ukip,
                               na.rm = T), 2)) %>%
  select(start_date, end_date, pollster_client, sample_size, conservative,
         green, labour, liberal_democrat, scottish_national_party,
         ukip, other) %>%
  drop_na(start_date)

ge_polls_2015_part_1 <- ge_polls_2015_html_table[[3]] %>%
  tibble() %>%
  rename(dates_conducted = V1, pollster_client = V2, sample_size = V3,
         conservative = V4, labour = V5, liberal_democrat = V6,
         ukip = V7, green = V8, other = V9, lead = V10) %>%
  drop_na(labour) %>%
  filter(!pollster_client %in% c("Polling organisation/client", "",
                                 "General Election results (GB only)[7][8]")) %>%
  separate(dates_conducted,
           into = c("poll_start", "poll_finish"),
           sep = "–",
           remove = FALSE) %>%
  mutate(sample_size = as.numeric(str_replace(sample_size, ",", "")),
         across(.cols = c(conservative, labour, liberal_democrat, , green,
                          ukip),
                ~ as.numeric(str_extract(.x, pattern = "\\d+")) / 100),
         start_date = case_when(!str_detect(poll_start, "[A-Za-z]{3} {0,1}$") ~
                                  paste(poll_start, str_extract(dates_conducted, "[A-Za-z]{3}$")),
                                TRUE ~ poll_start),
         end_date = case_when(is.na(poll_finish) ~ as.character(poll_start),
                              TRUE ~ as.character(str_extract(poll_finish, "\\d{1,2} [A-Za-z]{3}"))),
         start_date = as.Date(paste(start_date, "2015"), format = "%d %b %Y"),
         end_date = as.Date(paste(end_date, "2015"), format = "%d %b %Y")) %>%
  rowwise() %>%
  mutate(other = round(1 - sum(conservative, green, labour, liberal_democrat,
                               ukip,
                               na.rm = T), 2)) %>%
  select(start_date, end_date, pollster_client, sample_size, conservative,
         green, labour, liberal_democrat,
         ukip, other) %>%
  drop_na(start_date)

ge_polls_2014 <- ge_polls_2015_html_table[[4]] %>%
  tibble() %>%
  rename(dates_conducted = V1, pollster_client = V2, sample_size = V3,
         conservative = V4, labour = V5, liberal_democrat = V6,
         ukip = V7, green = V8, other = V9, lead = V10) %>%
  drop_na(labour) %>%
  filter(!pollster_client %in% c("Polling organisation/client", "")) %>%
  separate(dates_conducted,
           into = c("poll_start", "poll_finish"),
           sep = "–",
           remove = FALSE) %>%
  mutate(sample_size = as.numeric(str_replace(sample_size, ",", "")),
         across(.cols = c(conservative, labour, liberal_democrat, , green,
                          ukip),
                ~ as.numeric(str_extract(.x, pattern = "\\d+")) / 100),
         start_date = case_when(!str_detect(poll_start, "[A-Za-z]{3} {0,1}$") ~
                                  paste(poll_start, str_extract(dates_conducted, "[A-Za-z]{3}$")),
                                TRUE ~ poll_start),
         end_date = case_when(is.na(poll_finish) ~ as.character(poll_start),
                              TRUE ~ as.character(str_extract(poll_finish, "\\d{1,2} [A-Za-z]{3}"))),
         start_date = as.Date(paste(start_date, "2015"), format = "%d %b %Y"),
         end_date = as.Date(paste(end_date, "2015"), format = "%d %b %Y")) %>%
  rowwise() %>%
  mutate(other = round(1 - sum(conservative, green, labour, liberal_democrat,
                               ukip,
                               na.rm = T), 2)) %>%
  select(start_date, end_date, pollster_client, sample_size, conservative,
         green, labour, liberal_democrat,
         ukip, other) %>%
  drop_na(start_date)

## SAVE DATA --------------------------

ge_polls_2022 %>% write_csv(here("03.Data/02.Interim/ge_polls_2022.csv"))
ge_polls_2021 %>% write_csv(here("03.Data/02.Interim/ge_polls_2021.csv"))
ge_polls_2020 %>% write_csv(here("03.Data/02.Interim/ge_polls_2020.csv"))
ge_polls_2019 %>% write_csv(here("03.Data/02.Interim/ge_polls_2019.csv"))
ge_polls_2018 %>% write_csv(here("03.Data/02.Interim/ge_polls_2018.csv"))
ge_polls_2017_part_1 %>% write_csv(here("03.Data/02.Interim/ge_polls_2017_part_1.csv"))
ge_polls_2017_part_2 %>% write_csv(here("03.Data/02.Interim/ge_polls_2017_part_2.csv"))
ge_polls_2016 %>% write_csv(here("03.Data/02.Interim/ge_polls_2016.csv"))
ge_polls_2015_part_1 %>% write_csv(here("03.Data/02.Interim/ge_polls_2015_part_1.csv"))
ge_polls_2015_part_2 %>% write_csv(here("03.Data/02.Interim/ge_polls_2015_part_2.csv"))
ge_polls_2014 %>% write_csv(here("03.Data/02.Interim/ge_polls_2014.csv"))
