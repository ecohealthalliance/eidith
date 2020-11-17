library(eidith)
library(tidyverse)
library(DBI)
h <- here::here

import_local_db(database = "eha_with_malaysia")

conn <- eidith:::eidith_db()

countries <- eidith::eha_countries()
fs::dir_create(h(paste0("inst/queries/eidith_", countries)))

tbls <- db_list_tables(conn)

for(tb in tbls){
  dat <- DBI::dbReadTable(conn, tb)
  tb_name <- str_remove(tb, "_2")

  for(cntry in countries){
    if(tb_name == "training"){
      dat2 <- dat %>% filter(participant_home_country == cntry)
    }else{
      dat2 <- dat %>% filter(country == cntry)
    }
    write_csv(dat2, h(paste0("inst/queries/eidith_",cntry,"/",cntry, "_", tb_name, ".csv")))
  }
}

dbDisconnect(conn)


