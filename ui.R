library(shiny)
library(shinycustomloader)
library(tidyverse)
library(lubridate)
library(vroom)
library(plotly)
library(tippy)

maanden <- as_factor(c("Januari", "Februari", "Maart", "April", "Mei", "Juni", "Juli",
                       "Augustus", "September", "Oktober", "November", "December"))

ui <- navbarPage(
  theme = bslib::bs_theme(
    bootswatch = "cerulean",
    base_font = "Catamaran"
  ),
  windowTitle = "Onze Lucht in Heatmaps",
  title =  span(a(
     img(src='OnzeLucht.png',
         style="margin-top: 10px; padding-right:10px;padding-bottom:10px",
         width = "50%"),
         href="https://onzelucht.nl"),
     strong("Onze Lucht"), style="font-size:30px;"),
  br(),
  wellPanel(
    fluidRow(column(4, 
                    textInput("id", "Sensor ID:", 
                              value = "esp8266-16609729")),
    column(3, actionButton("update", "Vind sensor", 
                           icon = icon("search"),width = "100%",
                           style="color: #fff; background-color: #2fa4e7; border-color: #0f364d"),
           tippy_this("update", tooltip = "Klik button om te verbinden met sensor"),
           style = "margin-top: 31px;"),
    column(2, uiOutput("kies_jaar")),
    column(3, uiOutput("kies_maand"))
     ), 
    fluidRow(column(4, 
                    withLoader(textOutput("available_zips"),
                               type = "text",
                               loader = list(marquee("Verbinden...")),
                               proxy.height = "25px"),
                    tags$head(tags$style("#available_zips{font-size: 10px;
                                         font-style: italic;
                                         }"))
                    )
    ), style= "background: #e6f6ff"
  ),
  br(),
  tabsetPanel(
    tabPanel("Heatmaps",
             icon = icon("chart-bar"),
             br(),
             wellPanel(fluidRow(column(3, selectInput("meetwaarde", "Meetwaarde:",
                                                      choices = list("Fijnstof PM2.5" = "SDS_P2",
                                                                     "Fijnstof PM10" = "SDS_P1",
                                                                     "Temperatuur" = "Temp",
                                                                     "Luchtvochtigheid" = "Humidity"))),
                                column(3, selectInput("time_min", "Tijdsinterval (minuten):",
                                                      choices = c(5, 10, 15, 20, 30, 60),
                                                      selected = 10)),
                                column(2, uiOutput("png"),
                                       tippy_this("png", tooltip = "Sla heatmap op als png"),
                                  style = "margin-top: 31px;"),
                                column(2, uiOutput("pdf"), 
                                       tippy_this("pdf", tooltip = "Sla heatmap op als pdf"),
                                  style = "margin-top: 31px;"),
                                column(2, uiOutput("csv"),
                                       tippy_this("csv", tooltip = "Sla heatmap data op als csv"),
                                  style = "margin-top: 31px;")),
                       style= "background: #e6f6ff"),
             withLoader(plotlyOutput("heatmap", height = "800px"),
                        type = "image", loader = "air.gif")
    ),
    tabPanel("Data", icon = icon("table"),
             br(),
             textOutput("title"),
             dataTableOutput("df"),
             column(4, downloadButton("csv_raw", "Download maand data",
                              icon = icon("download"), width = "100%",
                              style="color: #fff; background-color: #2fa4e7; border-color: #0f364d"),
                    tippy_this("csv_raw", tooltip = "Download alle data voor de geselecteerde maand"))
    )
    ),
  hr(),
  div(a(
    img(src = "logo_grey.png",
        width = 40), 
    href="https://www.jebentwatjemeet.nl"),
    style="text-align: center; padding-bottom:10px;"),
  div("\u00A9 JeBentWatJeMeet 2021", 
      style="text-align:center; font-size:10px;padding-bottom:10px;")
)



  
