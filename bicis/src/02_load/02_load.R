library(tidyverse)
source("bicis/src/02_load/utils.R")

db_connection <- DBI::dbConnect(RSQLite::SQLite(),
                                "bicis/data/data-warehouse.db"
                                )

load_mad(db_connection)
load_ny(db_connection)
load_sf(db_connection)
load_bo(db_connection)
load_lo(db_connection)
load_bcn(db_connection)
