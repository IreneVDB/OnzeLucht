server <- function(input, output, session) {
  source("src/GetData.R")
  source("src/MakeHM.R")
  source("src/MakeFootnote.R")
  
  maanden <- as_factor(c("Januari", "Februari", "Maart", "April", "Mei", "Juni", "Juli",
                         "Augustus", "September", "Oktober", "November", "December"))
  
  # 1) After sensor selection: connect - update selection month & year:
  sensor <- eventReactive(input$update, tolower(input$id))
  zips <- eventReactive(input$update, get.available.zips(tolower(input$id)))
  
  display_text <- reactive({
    if(is.null(zips())){
      "GEEN DATA GEVONDEN - CONTROLEER SENSOR ID"
    } else{
      paste0("Beschikbare data voor sensor ",
             sensor(), ": ",
             paste0(maanden[month(zips()[1])], strftime(zips()[1], format = "'%y")), " t/m ",
             paste0(maanden[month(rev(zips())[1])], strftime(rev(zips())[1], format = "'%y")))
    }
  })
  
  output$available_zips <- renderText(display_text())
  
  observeEvent(zips(), {
    choices <- unique(year(zips()))
    output$kies_jaar <- renderUI({
      selectInput("jaar", "Jaar:", choices = rev(choices))
    })
  })
  
  months <- reactive({
    req(input$jaar)
    subset(zips(), year(zips()) == input$jaar)
  })
  
  observeEvent(months(), {
    freezeReactiveValue(input, "maand")
    choices <- setNames(month(months()), maanden[month(months())])
    output$kies_maand <- renderUI({
      selectInput("maand", "Maand:", choices = rev(choices))
    })
  })
  
  filename <- reactive({
    req(input$maand)
    paste(sensor(), input$jaar, input$maand, sep = "_")
    })
  
  # 2) After selection month: create table and plot
  
  # Table:
  title <- reactive({
    req(input$maand)
    paste("Overzicht sensor", sensor(),
          maanden[as.numeric(input$maand)], input$jaar)
  })
  
  output$title <- renderText(title())
  
  df <- reactive({
    req(input$maand)
    get.data.from.web(year = input$jaar, month = input$maand, sensor_id = sensor())
    })
  
  summary <- reactive(summarise.df(df = df()))
  
  output$df <- renderDataTable(summary(), options = list(dom = "t",
                                                          paging = FALSE,
                                                          searching = FALSE,
                                                          ordering = FALSE))
  
  output$csv_raw <- downloadHandler(
    filename = function() paste0(filename(), ".csv"),
    content = function(file){
      write.csv(df(), file)
    }
  )
  
  # Heatmap:
  df_time <- reactive(make.df_time(df = df(), t_min = input$time_min))
  heatmap <- reactive(make.heatmap(df = df_time(), t_min = input$time_min,
                                   meetwaarde = input$meetwaarde, legend_w = 1))

  plotly_hm <- reactive({
    gp <- ggplotly(heatmap(), tooltip = "text")
    gp[["x"]][["layout"]][["annotations"]][[2]][["x"]] <- -0.065
    return(gp)
    })
  
  output$heatmap <- renderPlotly(plotly_hm() %>%
                                   layout(margin = list(t = 100,
                                                        b = 50,
                                                        l = 95),
                                          annotations = list(x=1, y=1.025, showarrow=F,
                                                             text = paste("Sensor id:", sensor()),
                                                             xref='paper', yref='paper',
                                                             xanchor='right', yanchor='bottom',
                                                             font=list(size = 10, color = "#7F7F7F"))))
  
  render.download.buttons <- function(type){
    renderUI({
      req(df())
      downloadButton(paste0("download_", type), paste0(".", type),
                   icon = icon("print"), width = "100%",
                   style="color: #fff; background-color: #2fa4e7; border-color: #0f364d")
      })
  }
  
  output$png <- render.download.buttons("png")
  output$pdf <- render.download.buttons("pdf")
  output$csv <- render.download.buttons("csv")
  
  download.file.from.button <- function(type){
    downloadHandler(
      filename = function() paste0(filename(), "_", input$meetwaarde, "_", input$time_min, "min.", type),
      if(type == "csv"){
        content = function(file){
          write.csv(df_time(), file)
          }
      } else{
          content = function(file){
            if(type == "png"){
              mar <- margin(0.5, 0.5, 1.4, 0.5, "in")
              png(file, width = 8.27, height = 11.64, units = "in", res = 300)
            } else if(type == "pdf"){
              mar <- margin(1, 1, 1.4, 1, "in")
              cairo_pdf(file, width = 8.27, height = 11.64)
            }
            print(make.heatmap(df = df_time(), t_min = input$time_min, 
                               meetwaarde = input$meetwaarde,
                               mar = mar))
            makeFootnote(info = "Meer info: www.onzelucht.nl",
                         bron = "https://api-rrd.madavi.de",
                         sensor_id = sensor())
            dev.off()
          }
      }
    )
  }
  
  output$download_png <- download.file.from.button("png")
  output$download_pdf <- download.file.from.button("pdf")
  output$download_csv <- download.file.from.button("csv")
 
}
