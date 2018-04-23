## ----setup, include=FALSE------------------------------------------------
library(knitr)
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)
library(eidith)
library(dplyr)
library(ggplot2)
P <- rprojroot::find_package_root_file

## ----1-------------------------------------------------------------------
#  library(eidith)

## ----2-------------------------------------------------------------------
#  library(dplyr)
#  library(ggplot2)

## ----picture, eval = TRUE, echo = FALSE, results = 'asis', upload.fun = image_uri----
uri <- knitr::image_uri(P("inst","images","total_schema.jpg"))
cat(sprintf("<img src=\"%s\" style=\"width:825px\">", uri))


## ----ed_metadata---------------------------------------------------------
#  ?ed2_metadata

## ----metadata_picture, eval = TRUE, echo = FALSE, results = 'asis', upload.fun = image_uri----
uri2 <- knitr::image_uri(P("inst","images","metadata_example.png"))
cat(sprintf("<img src=\"%s\" style=\"width:825px\">", uri2))


## ----3, eval = TRUE------------------------------------------------------
events <- ed2_events()
animals <- ed2_animals()

## ----anonymize, eval = TRUE, echo = FALSE--------------------------------

num_countries <- length(unique(events$country))

new_names <- paste("Country", LETTERS[1:num_countries])

random_countries <- sample(new_names, num_countries, replace = FALSE)

random_df <- data_frame(country_id = unique(events$country), random_countries)

ea.joined <- left_join(events, animals, by = "event_name")


ea.joined <- left_join(ea.joined, random_df, by = c("country" = "country_id"))

ea.joined$country <- ea.joined$random_countries


## ----4, eval = FALSE-----------------------------------------------------
#  
#  ea.joined <- left_join(events, animals, by = "event_name")
#  

## ----4a------------------------------------------------------------------
#  
#  ea.reduced <- select(ea.joined,event_name, country, animal_id, species_scientific_name)
#  
#  View(ea.reduced)
#  

## ----secret4a, echo = FALSE, eval = TRUE---------------------------------

ea.reduced <- select(ea.joined,event_name, country, animal_id, species_scientific_name)


## ----5a, eval = TRUE, fig.width = 8, fig.height = 6----------------------
library(ggplot2)

ea.summary <- group_by(ea.reduced, country, species_scientific_name) %>%
  summarize(species_num = n())

ggplot(data = ea.summary) +
  geom_bar(aes(x = country)) +
  theme_minimal() + 
  labs(title = "Number of Species by Country", subtitle = "Note: Scrambled Data")


## ----5b, eval = TRUE, fig.width = 8, fig.height = 6----------------------

ggplot(data = ea.summary) +
  geom_bar(aes(x = country, y = species_num), stat = "identity") +
  theme_minimal() +
  labs(title = "Number of Animals by Country", subtitle = "Note: Scrambled Data")


## ----5c, eval = TRUE, fig.width = 8, fig.height = 6----------------------

ggplot(data = ea.summary) +
  geom_bar(aes(x = country, y = species_num, fill = species_scientific_name), stat = "identity") +
  theme_minimal() +
  theme(legend.position = "none") + # removing this line will show the legend with all the species names 
  labs(title = "Number of Animals by Country with Species Colors", subtitle = "Note: Scrambled Data")


