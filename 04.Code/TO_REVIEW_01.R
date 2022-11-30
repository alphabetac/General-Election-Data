## PROJECT:
## TITLE:
## AUTHOR(S): Henry Johnston-Ellis
## DATE: Thu Oct 13 15:03:48 2022

## NOTES:

## SET CREDENTIALS --------------------

## LOAD LIBRARIES ---------------------

library(tidyverse)
library(here)

## LOAD DATA --------------------------

combined_raw_polls <- read_csv(here("03.Data/02.Interim/combined_raw_polls.csv"))

## TIDY DATA --------------------------

tidy_polls_wide <- combined_raw_polls %>%
  unite(BXP, BXP:`BXP/Reform`) %>%
  mutate(BXP = as.numeric(str_extract(BXP, "\\d+"))) %>%
  select(start_date, end_date, pub_date, Polling, Publisher,
         BNP, BXP, Con, Green, Lab, LD, Referendum, SDP, `TIG/CUK`, UKIP,
         method) %>%
  mutate(across(.cols = c(BNP, BXP, Con, Green, Lab, LD, Referendum, SDP, `TIG/CUK`, UKIP),
                ~ .x / 100))

tidy_polls_long <- tidy_polls_wide %>%
  pivot_longer(cols = c(BNP, BXP, Con, Green, Lab, LD, Referendum, SDP, `TIG/CUK`, UKIP),
               names_to = "party",
               values_to = "vote_share")
