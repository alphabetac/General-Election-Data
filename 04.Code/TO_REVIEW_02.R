## PROJECT:
## TITLE:
## AUTHOR(S): Henry Johnston-Ellis
## DATE: Thu Oct 13 15:22:09 2022

## NOTES:

## SET CREDENTIALS --------------------

## LOAD LIBRARIES ---------------------

library(tidyverse)
library(here)
library(hjplottools)
library(plotly)

## LOAD DATA --------------------------



## PLOT DATA --------------------------

ggplotly(
  tidy_polls_long %>%
    ggplot(aes(x = start_date, y = vote_share)) +
    geom_point() +
    geom_line(aes(colour = party)) +
    hj_theme()
)
