## ----setup, include=FALSE------------------------------------------------
library(knitr)
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)
library(eidith)
library(dplyr)
library(ggplot2)
P <- rprojroot::find_package_root_file
library(stringr)
library(tibble)

## ----1-------------------------------------------------------------------
#  library(eidith)
#  library(knitr)
#  library(dplyr)

## ----beginning, eval = TRUE----------------------------------------------

humans <- ed2_human()

humans_reduced <- select(humans, participant_id, travel_reason)


## ----head2, eval = TRUE--------------------------------------------------

head(humans_reduced) %>% kable()


## ----wide, eval = TRUE---------------------------------------------------

humans_reduced_wide <- ed2_expand_wide(humans_reduced, travel_reason)

head(humans_reduced_wide) %>% kable()


## ----widegraph, eval = TRUE, fig.width = 9, fig.height = 6---------------

ggplot(data = humans_reduced_wide) +
  geom_bar(aes(x = travel_reason_go_to_market, fill = travel_reason_go_to_market), width = 0.8) +
  theme_minimal() +
  labs(title = "Reason for Travel Participant Responses: Going to Market")



## ----long, eval = TRUE---------------------------------------------------

humans_reduced_long <- ed2_expand_long(humans_reduced, travel_reason)

head(humans_reduced_long) %>% kable()


## ----other, eval = TRUE--------------------------------------------------

humans_other <- filter(humans_reduced_long, !is.na(other_details))

head(humans_other) %>% kable()


## ----longplot, eval = T, fig.height = 10, fig.width = 7------------------

ggplot(data = humans_reduced_long) +
  geom_bar(aes(x = travel_reason_val)) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + # this line of code turns the x-axis labels so they are easier to read
  labs(title = "Reason for Travel Participant Responses")


