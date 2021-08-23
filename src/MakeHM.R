# Functie voor het maken van een heatmap gebaseerd op dataframe van gekozen sensor, jaar, maand
make.df_time <- function(df, t_min){
  t_min <- as.numeric(t_min)
  
  df_time <- df %>%
    mutate(Time = floor_date(Time, unit = paste(t_min, "mins"))) %>%
    group_by(Time) %>%
    summarise(SDS_P1 = mean(SDS_P1, na.rm = TRUE),
              SDS_P2 = mean(SDS_P2, na.rm = TRUE),
              Temp = mean(Temp, na.rm = TRUE),
              Humidity = mean(Humidity, na.rm = TRUE))
              # {{ meetwaarde }} := mean(!!as.name(meetwaarde), na.rm = TRUE))
}

make.heatmap <- function(df,  t_min, meetwaarde,
                         mar = margin(6, 2, 2, 2, "mm"), legend_w = 1.5){
  t_min <- as.numeric(t_min)
  
  meetbereik <- case_when(meetwaarde == "SDS_P1" ~ c(0, 112),
                          meetwaarde == "SDS_P2" ~ c(0, 56),
                          meetwaarde == "Temp" ~ c(-10, 30),
                          meetwaarde == "Humidity" ~ c(0, 100))
  
  eenheid <- case_when(meetwaarde %in% c("SDS_P1", "SDS_P2") ~ "\u03BCg/m\u00B3",
                      meetwaarde == "Temp" ~ "\u00B0C",
                      meetwaarde == "Humidity" ~ "%")
  
  plotlabel <- case_when(meetwaarde == "SDS_P1" ~ "Fijnstof PM10",
                         meetwaarde == "SDS_P2" ~ "Fijnstof PM2.5",
                         meetwaarde == "Temp" ~ "Temperatuur",
                         meetwaarde == "Humidity" ~ "Luchtvochtigheid")
  
  hue <- case_when(meetwaarde %in% c("SDS_P1", "SDS_P2") ~ 0.56,
                   meetwaarde == "Temp" ~ 0.55,
                  meetwaarde == "Humidity" ~ 0.1)
  
  colgrad <- if(meetwaarde %in% c("SDS_P1", "SDS_P2")){
    hsv(h = c(rep(hue, 2), hue - 0.2, hue - 0.4, 
              hue - 0.45, hue - 0.575, rep(hue - 0.7, 2)) %% 1,
        s = c(rep(1,2), 0.6, 0.2, 0.2, 0.6, rep(1,2)),
        v = c(0.5, 0.7, 0.9, 1, 1, 0.8, 0.5, 0.3))
    } else{
      hsv(h = c(rep(hue, 2), 0, rep(hue + 0.4, 2) %% 1),
          s = c(1, 1, 0, 1, 1), 
          v = c(0.5, 1, 0.925, 1, 0.5))
    }

  all_datetime <- data.frame(Time = seq(
    floor_date(df$Time[1], unit = paste(t_min, "mins")),
    floor_date(rev(df$Time)[1], unit = paste(t_min, "mins")),
    by = paste(t_min, "mins")))

  maanden <- c("Januari", "Februari", "Maart", "April", "Mei", "Juni", "Juli",
               "Augustus", "September", "Oktober", "November", "December")

  df <- df %>%
    select(Time, meetwaarde) %>%
    left_join(all_datetime, ., by = "Time") %>%
    arrange(Time) %>%
    mutate(date = as_date(Time),
           Time = substr(Time, 12, 16),
           # {{ meetwaarde }} := imputeTS::na_ma(!!as.name(meetwaarde),
           #                                     k = 60 / t_min, maxgap = 60 / t_min),
           year = year(date),
           month = maanden[month(date)],
           day = day(date),
           text = paste0("Datum: ", date, "\n", "Tijd: ", Time, "\n",
                          plotlabel, ": ", format(round(!!as.name(meetwaarde), 2), nsmall = 2), " ", eenheid),
           normalized = case_when(!!as.name(meetwaarde) > meetbereik[2] ~ meetbereik[2],
                              !!as.name(meetwaarde) < meetbereik[1] ~ meetbereik[1],
                               TRUE ~ !!as.name(meetwaarde))) %>%
    arrange(date, Time)

  plot <- ggplot(df, aes(x = day, y = Time, fill = normalized, text = text)) + 
    geom_tile(color = "white", size = 0.1) +
    scale_fill_gradientn(colors = colgrad, na.value = "white", limits = meetbereik) +
    guides(fill = guide_colorbar(title = paste0(plotlabel, "\n(", eenheid, ")"))) +
    facet_grid(year~month) + 
    scale_x_continuous(limits = c(df$day[1] - 0.5, rev(df$day)[1] + 0.5),
                       breaks = df$day[1]:rev(df$day)[1],
                       expand = c(0.01, 0.01)) +
    scale_y_discrete(limits = rev, expand = c(0.01, 0.01),
                     breaks = unique(df[["Time"]][which(substr(df[["Time"]], 4, 5) == "00")])) +
    theme_minimal(base_family = "Comfortaa") +
    labs(title = paste("OnzeLucht sensor", plotlabel, sep = " - "), 
         subtitle = paste(unique(df$month), unique(df$year)),
         x = "Dag", y = "Tijd") +
    theme(plot.margin = mar,
          plot.title.position = "plot",
          plot.title = element_text(size = 14, hjust = 1, color = "grey30", face = 2),
          plot.subtitle = element_text(size = 10, color = "grey30", hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(colour="white"),
          strip.text.x = element_text(size = 10, color = "grey40", face= 2),
          strip.text.y = element_text(size = 10, color = "grey40", face= 2),
          axis.title = element_text(size = 12, color = "grey40", face = 2),
          axis.text.x = element_text(size=8, color = "grey60"),
          axis.text.y = element_text(size=8, color = "grey60"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(color = "grey60", size = 0.25),
          legend.position = "bottom",
          legend.justification = c(0,0),
          legend.margin = margin(0,0,0,0, unit = "pt"),
          legend.title = element_text(size=9, color = "grey40", hjust = 0.5, vjust = 1),
          legend.text = element_text(size=8, color = "grey40"),
          legend.key.width = unit(legend_w, "cm"),
          legend.key.height = unit(0.75, "cm")) 

  return(plot)
}


