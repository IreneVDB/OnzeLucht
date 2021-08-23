#Make footnote:
makeFootnote <- function(info, bron, sensor_id) {
  require(grid)
  require(png)
  img <- readPNG("www/logo_grey.png")
  pushViewport(viewport())
  grid.lines(x = unit(c(0, 1), "npc") + unit(c(1,-1),"in"),
             y = unit(1, "in"),
             gp = gpar(col = "grey30", lwd = 0.4))
  grid.text(label= paste0("Bron: ", bron, "\nSensor: ", sensor_id),
            x = unit(1,"npc") - unit(1, "in"),
            y = unit(1, "in") - unit(2, "mm"),
            just=c("right", "top"),
            gp = gpar(cex = 0.6, col = "grey50", fontfamily = "Comfortaa"))
  grid.raster(img, x = 0.5, 
              y = unit(1, "in") - unit(2, "mm"),
              just = c("center", "top"),
              width = unit(10, "mm"))
  grid.text(label= "\u00A9 JeBentWatJeMeet 2021",
            x = 0.5, y = unit(10, "mm"),
            just=c("center", "bottom"),
            gp=gpar(cex= 0.5, col="grey60", fontfamily = "Comfortaa"))
  grid.text(label= info,
            x = unit(1,"npc") - unit(1, "in"),
            y = unit(10, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= 0.6, col="grey50", fontfamily = "Comfortaa"))
  popViewport()
}

