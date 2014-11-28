# server.R
# Load packages
library("ggplot2")
library("stringr")
library("scales")


# create static plot  -------------------
numboxes <- length(grep("# Box number", bgm))

# extract the box vertices
vertices <- data.frame()
for(i in 1:numboxes){
  vert_tmp <- grep(paste("box", i - 1, ".vert ", sep = ""), bgm)
  vertices <- rbind(vertices, cbind(i - 1, bgm[vert_tmp]))
}

# extract lat and long
coords_tmp <- str_split(vertices$V2, pattern = " ")
x <- sapply(coords_tmp,`[`,2)
y <- sapply(coords_tmp,`[`,3)

# recombine into data.frame
map_base <- data.frame(boxid = vertices$V1, x = x, y = y)
map_base$x <- as.double(as.character(map_base$x))
map_base$y <- as.double(as.character(map_base$y))

shinyServer(
  function(input, output) {
    # Olive-type map
    output$map <- renderPlot({
      tmp <- vars[[input$var]]
      if(length(dim(tmp)) == 2 & dim(tmp)[1] == 53){
        tmp <- vars[[input$var]][,input$time]
      } else {
        if(length(dim(tmp)) == 2 & dim(tmp)[1] == 7){
        tmp <- vars[[input$var]][input$layer,]
        } else {
        tmp <- vars[[input$var]][input$layer,,input$time]
      }
      }
      # This is for Iceland Atlantis only
      # boxes 19 and 52 are islands, so color code them differently
      if(length(tmp) == 53)
        tmp[c(20,53)] <- NA
      
      data_tmp <- data.frame(boxid = 0:(numboxes - 1), tmp)
      
      map_base <- merge(map_base, data_tmp)
      ggplot(data = map_base, aes(x = x, y = y)) +
       geom_polygon(aes(group = boxid, fill = tmp), colour = "black") +
       theme_bw() + xlab("Longitude") + ylab("Latitude") +
       scale_fill_gradient2(low = muted("blue"), high = muted("green")) +
       theme(legend.title=element_blank()) 
      })

    # Relative biomass map
    output$rel_map <- renderPlot({
      qplot(y = relative[[input$rel_var]], x = Time, data = relative, geom = "line") +
        ylab("") +  theme_bw()
    })

    # SSB map
   output$ssb_map <- renderPlot({
      qplot(y = ssb[[input$ssb_var]], x = Time, data = ssb, geom = "line") +
        ylab("") +  theme_bw()
    }) 
   # YOY map
   output$yoy_map <- renderPlot({
      qplot(y = yoy[[input$yoy_var]], x = Time, data = yoy, geom = "line") +
      ylab("") +  theme_bw()
    }) 
  })
