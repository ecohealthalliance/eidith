library(eidith)
library(tidyverse)


e <- ed2_events() |> filter(project == "P1")


a <- ed2_animals() |> filter(project == "P1")


s <- ed2_specimens() |> filter(project == "P1")


t <- ed2_tests() |> filter(project == "P1")


write_csv(e, here::here("p1_extracts/p1_events.csv"))
write_csv(a, here::here("p1_extracts/p1_animals.csv"))
write_csv(s, here::here("p1_extracts/p1_specimens.csv"))
write_csv(t, here::here("p1_extracts/p1_tests.csv"))
