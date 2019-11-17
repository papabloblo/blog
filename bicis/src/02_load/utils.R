

load_mad <- function(db_connection){
  files <- list.files("bicis/data/mad", full.names = TRUE)
  
  for (file_path in files){
    cat("Loading", file_path, "\n")
    data_file <- readr::read_lines(file_path)
    data_file <- paste(data_file, collapse = ", ")
    data_file <- paste("[", data_file, "]")
    
    data_file <- rjson::fromJSON(data_file)
    data_file <- map_df(data_file,
                        function(x) 
                          list(
                            date_time = x$unplug_hourTime$`$date`,
                            user_type = as.character(x$user_type)
                            )
                        ) %>% 
      group_by(date_time, user_type) %>% 
      summarise(n = n()) %>% 
      ungroup()
    
    data_file <- data_file %>% 
      mutate(
        date_time = lubridate::as_datetime(date_time, tz="Europe/Madrid"),
        city = "mad",
        hour = lubridate::hour(date_time),
        date = as.character(lubridate::date(date_time))
        ) %>% 
      select(city, date, hour, user_type, n)
    
    DBI::dbWriteTable(db_connection, "use", data_file, append = TRUE)
    rm(data_file)
    gc()
  }  
  
}


load_ny <- function(db_connection){
  
  files <- stringr::str_subset(list.files("bicis/data/ny", full.names = TRUE), 
                               pattern = "/20[:digit:]{4}-citibike-tripdata"
                               )
  for (file_path in files){
    cat("Loading", file_path, "\n")  
    data_file <- read_csv(file_path,
                   col_types = cols(.default = col_character())
                   )
    
    names(data_file) <- str_replace_all(str_to_lower(names(data_file)), "\\s", "")
    
    data_file <- data_file %>% 
      select(starttime, usertype)
    
    if (str_detect(data_file$starttime[1], "^[:digit:]{4}")){
      data_file <- data_file %>% 
        mutate(
          starttime = lubridate::as_datetime(starttime),
          hour = lubridate::hour(starttime),
          date = lubridate::date(starttime)
        )
    } else {
      data_file <- data_file %>% 
        mutate(
          hour = as.numeric(
            str_sub(str_extract(starttime, "[:digit:]{2}:.*$"), 1, 2)
            ),
          date = as.Date(starttime, format = format("%m/%d/%Y %H:%M:%S"))
        )
      
    }
    
    data_file <- data_file %>% 
      group_by(
        date, hour, usertype
      ) %>% 
      summarise(n = n()) %>% 
      ungroup() %>% 
      mutate(city = "ny",
             date = as.character(date)) %>% 
      rename(user_type = usertype)
    
    DBI::dbWriteTable(db_connection, "use", data_file, append = TRUE)
    rm(data_file)
    gc()
  }
}


load_sf <- function(db_connection){
  files <- list.files("bicis/data/sf/", full.names = TRUE)
  
  for (file_path in files){
    cat("Loading", file_path, "\n")  
    data_file <- read_csv(file_path,) %>% 
      mutate(
        hour = lubridate::hour(start_time),
        date = lubridate::date(start_time)
      ) %>% 
      group_by(
        date, hour, user_type
      ) %>% 
      summarise(n = n()) %>% 
      ungroup() %>% 
      mutate(city = "sf",
             ddate = as.character(date))
    
    DBI::dbWriteTable(db_connection, "use", data_file, append = TRUE)
    rm(data_file)
    gc()
  }
}



load_bo <- function(db_connection){
  
  files <- list.files("bicis/data/bo/", full.names = TRUE) %>% 
    str_subset("tripdata")
  
  for (file_path in files){
    cat("Loading", file_path, "\n")  
    data_file <-  read_csv(file_path
                           ) %>% 
      mutate(
        hour = lubridate::hour(starttime),
        date = lubridate::date(starttime)
      ) %>% 
      group_by(
        date, hour, usertype
      ) %>% 
      summarise(n = n()) %>% 
      ungroup()  %>% 
      mutate(city = "bo",
             date = as.character(date)) %>% 
      rename(user_type = usertype)
    
    DBI::dbWriteTable(db_connection, "use", data_file, append = TRUE)
    rm(data_file)
    gc()
  }
}


load_lo <- function(db_connection){
  
  files <- list.files("bicis/data/london", full.names = TRUE) %>% 
    str_subset("ourney.*.csv")
  
  for (file_path in files){
    cat("Loading", file_path, "\n")  
    data_file <- read_csv(file_path) %>% 
      mutate(
        hour = as.numeric(str_sub(str_extract(`Start Date`, "[:digit:]{2}:.*$"), 1, 2)),
        date = as.Date(`Start Date`, format = format("%d/%m/%Y %H:%M"))
      ) %>% 
      group_by(
        date, hour
      ) %>% 
      summarise(n = n()) %>% 
      ungroup() %>% 
      mutate(city = "lo",
             date = as.character(date))
    
    DBI::dbWriteTable(db_connection, "use", data_file, append = TRUE)
    rm(data_file)
    gc()
  }
}



load_bcn <- function(db_connection){
  
  files <- list.files("bicis/data/bcn", full.names = TRUE)
  
  for (file_path in files){
    cat("Loading", file_path, "\n")  
    data_file <- read_csv(file_path)  %>% 
      mutate(
        hour = lubridate::hour(dateTime),
        date = lubridate::date(dateTime)
      ) %>% 
      group_by(
        date, hour
      ) %>% 
      summarise(n = sum(bikesInUsage)) %>% 
      ungroup() %>% 
      mutate(city = "bcn",
             date = as.character(date))
    
    DBI::dbWriteTable(db_connection, "use", data_file, append = TRUE)
    rm(data_file)
    gc()
  }
}
