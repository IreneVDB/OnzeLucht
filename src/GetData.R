# Functie om fijnstofdata uit zip bestanden van Madavi.de te halen:

# 1) get the dates of available files:
library(XML)
get.available.zips <- function(sensor_id = "esp8266-16609729"){
  url <- paste0("http://api-rrd.madavi.de/csvfiles.php?sensor=", sensor_id)
  doc <- xmlTreeParse(url, useInternal = TRUE)
  files <- getHTMLLinks(doc)
  zip_files <- grep("(\\.zip)$", files, value = TRUE)
  
  if(length(zip_files) == 0) {
    return(NULL)
  } else{
    get.ym <- function(filename) str_extract(filename, "\\d{4}-\\d{2}(?=\\.zip)")
    
    first_month <- ym(get.ym(zip_files[1]))
    last_month <- ym(get.ym(rev(zip_files)[1]))
    
    zips <- seq(first_month, last_month, by = "month")
    return(zips)
  }
  }

# get the data from Zip or from file per month:
get.data.from.web <- function(year, month, sensor_id = "esp8266-16609729"){
  month <- str_pad(month, 2, "left", "0")
  name <- paste(sensor_id, year, month, sep = "-")
  
  temp <- tempfile()
  download.file(file.path("https://api-rrd.madavi.de/data_csv", year, month, paste0(
      "data-", name, ".zip")), temp, mode = "wb")
  allfiles <- unzip(temp, list = TRUE)
    
  df <- suppressWarnings(map_dfr(allfiles$Name, function(file){
    df <- vroom(unz(temp, filename = file), delim = ";",
                  col_select = c(Time, SDS_P1, SDS_P2, Temp, Humidity),
                  col_types = cols(),
                  na = "NA", col_names = TRUE)
    })) %>%
      mutate(Time = as_datetime(Time))
  
  unlink(temp)
  return(df)
}

# maak een overzichtstabel:
summarise.df <- function(df){
  summary <- df %>%
    pivot_longer(cols = 2:5) %>%
    mutate(name = factor(name, levels = c("SDS_P2", "SDS_P1", "Temp", "Humidity"))) %>%
    group_by(name) %>%
    summarise(Gemiddelde = round(mean(value, na.rm = TRUE), 1),
              Min = round(min(value, na.rm = TRUE), 1),
              Max = round(max(value, na.rm = TRUE), 1),
              .groups = "drop") %>%
    mutate(Meetwaarde = paste0(c("Fijnstof PM2.5","Fijnstof PM10",
                                 "Temperatuur", "Luchtvochtigheid"), 
                               " (", 
                               c(rep("\u03BCg/m\u00B3", 2), "\u00B0C", "%"),
                               ")")) %>%
    select(Meetwaarde, everything(), -name)
}

  