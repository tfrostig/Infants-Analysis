### Plotting Functions

## Create Room borders 
## Input: None. 
## Output: lines indicating the borders of theroom 

room <- function()
{
  abline(v=369)
  abline(v=0)
  abline(h=0)
  abline(h=546)
}

## Function to plot the path of the children including lingering and progression 
## Input: Data frame of children contating coordinates, object list true.name and false name 
## Output: Plotting path  

PathPlot <- function(true.name, false.name, children.df, object.list, txt = '',excursion = F, ...) { 
  if (excursion == F) {
  prog.children.df <- subset(children.df, prog.vec == 1)
  plot(prog.children.df$y ~ prog.children.df$x, main = paste(false.name, txt),
       xlim =  c(0,375), ylim =c(0,550), 
       asp = 1 , type = 'p' , col = 'red', cex = 0.6, ...)
  ling.children.df <- subset(children.df, prog.vec == 0)
  points(ling.children.df$y ~ ling.children.df$x,
         xlim =  c(0,375), ylim =c(0,550), 
         asp = 1 , type = 'p' , col = 'blue', cex = 0.6,  ...)
  DrawAllObjects(true.name, object.list)
  room()
  }
}

## Function to plot all objects in room 
## Input: Name of children, object list 
## Output: Drawing objects inside room, green for mom, pink for everything else 

DrawAllObjects <- function(true.name, object.list) {
  fitting.object <- object.list[[grep(true.name, names(object.list))]]
  mom.index      <- grep('Mother', names(fitting.object))
  mom.object     <- fitting.object[[mom.index]]
  fitting.object <- fitting.object[-mom.index]
  for (i in 1:length(fitting.object)) {
    ObjectDraw(fitting.object[[i]], col = 'gray25', density = 30)
  }  
  DrawRect(mom.object, col = 'green', density = 20)
}

## Drawing rectangle 
## Input: Matrix with coordinates 
## Output: Drawing rectangle 

DrawRect <- function(rect.coord, ...) {
  x.left <- rect.coord[1,1] 
  y.bottom <- rect.coord[2,2] 
  x.right <- rect.coord[2,1] 
  y.top <- rect.coord[1,2]
  rect(x.left, y.bottom, x.right, y.top,...) 
}

## Object Drawer 
## Input: Some type of object 
## Output: Draw someobject 

ObjectDraw <- function(coord, ...) {
  if (nrow(coord) == 2) {
    DrawRect(coord, ...) 
  } else {
    coord <- as.numeric(coord)
    filledcircle(coord[3], 0, coord[1:2], ...)
  }
}

### Creating pallette 
col.pal <- c(seq_gradient_pal('white', 'red')(seq(0, 1, length.out = 20)), 
             seq_gradient_pal('red', 'yellow')(seq(0, 1, length.out = 30)),
             seq_gradient_pal('yellow', 'yellow')(10))


## Draw Heatmap 
## Input: heatmap and number of visits 
## Output: ggplot2 map  

DrawHeat <- function(heat.map, visit.df, arc.df) {  
    ggplot() + 
    geom_tile(data =  heat.map, aes(x = x, y = y, z = z , fill = z)) + 
    geom_contour(data =  heat.map, aes(x = x, y = y, z = z), 
                 color = "darkgrey", alpha = 1, bins = 10) +   
    scale_fill_gradientn(colors = col.pal, values = seq(0, 37, length.out = 50) / 37)  + 
    #scale_fill_gradientn(colors = col.pal) +
    theme_bw() + expand_limits(y=c(0,540) , x = c(0,365)) + 
    coord_fixed(ratio = 1) + 
    geom_segment(data = visit.df , aes(x = x, y= y , xend=x + r * sin(z), 
                                       yend=y + r * cos(z)),
                 size = 0.5 , col = 'black', alpha = 0.8) +
    geom_path(data = arc.df, aes(x = x + r * sin(arc.cord),
                                 y = y + r * cos(arc.cord),
                                 group = Visit.Num), size = 1.5)
}

# 
# AddArc <- function(visit.df, arc.df) { 
#   geom_segment(data = visit.df , aes(x = x, y= y , xend=x + r * sin(z), 
#                                   yend=y + r * cos(z)),
#                size = 0.5 , col = 'black', alpha = 0.8) +
#     geom_line(data = arc.df, aes(x = x + r * sin(arc.cord),
#                                  y = y + r * cos(arc.cord),
#                                  group = Visit.Num), size = 1.5)
#   }


## Create the nuer of visits 
## Input: Heatmap, smooth see table , progression and lingering 
## Ouput: Create heatmap 

VisitMaker <- function(Map , SmoothedSEE , ProgVec , Threshold = 1 , lower = 10 , upper = 100) { 
  xVec <- unique(Map$x)
  yVec <- unique(Map$y)
  resMatX <- maxVecMaker(Map , xVec)
  resMatY <- maxVecMaker(Map , yVec)
  TopZ <- intersect(resMatX[localMaxima(resMatX[,2]),2] , resMatY[localMaxima(resMatY[,2]),2])
  XYZTop <- data.frame(cbind(Map[(Map$z %in% TopZ),1:2],'NoV' = 0))
  #XYZTop <- cbind(MaxFinder(Map) , 0 )
  return(FindVisit(XYZTop , SmoothedSEE , ProgVec , Threshold = 1, lower = lower , upper = upper))
}


## Draw star indicating the number of visits 
## Input: visits matrix , r indicating the length of the lines 
## Output: plot star unto ggplot2 plot 

StarDraw <- function(Visits , r = 20) {
  if (length(Visits[,3] == 0) == 0) {}
  Visits <- Visits[which(Visits[,3] > 0),]
  df <- data.frame(x=rep(Visits[,1], Visits[,3]), y=rep(Visits[,2], Visits[,3]))
  df$z <- ave(1:nrow(df), df$x, df$y, FUN = seq_along)
  df$r <- r
  return(df)
}


## Create rectangle in ggplot2 
## Input: matrix of rectangle 
## Ouput: Draw rectangle 

DrawRectangleGG2 <- function(matrix , col.temp = 'darkgrey' , alpha = 0.3) { 
  x.min <- min(matrix[,1]) 
  x.max <- max(matrix[,1]) 
  y.min <- min(matrix[,2])
  y.max <- max(matrix[,2])
  rect.dat <- data.frame(xmin = x.min, xmax = x.max, ymin = y.min, ymax = y.max)
  return(geom_rect(data = rect.dat, aes(xmin=xmin, xmax =xmax, ymin=ymin, ymax=ymax), 
            alpha = alpha, inherit.aes = FALSE, fill = col.temp))
}

## Create circle in ggplot2 
## Input: matrix of rectangle 
## Ouput: Draw rectangle 


DrawCircleGG2 <- function(circ.vec , col.temp = 'darkgrey') { 
  angle        <- seq(-pi, pi, length = 100)
  circ.vec     <- as.numeric(circ.vec)
  circle.df    <- data.frame(x = sin(angle) * circ.vec[3] + circ.vec[1],
                             y = cos(angle) * circ.vec[3] + circ.vec[2])
  geom_polygon(aes(x, y), data = circle.df,
               inherit.aes = F,  alpha = 0.2 , col = col.temp)
}


## Create circle or rectangle in ggplot2 dependes on input 
## Input: matrix of rectangle or vector 
## Ouput: Draw rectangle or circle 

DrawGG <- function(object.draw) {
  if (nrow(object.draw) == 2) {
    return(DrawRectangleGG2(object.draw))
  } 
  if (nrow(object.draw) == 1){ 
    return(DrawCircleGG2(object.draw))
    }
}



## Create Heatmap 

PlotHeatMap <- function(heatamp.list, visit.list, object.list = object.list,
                        true.name = '', false.name = true.name) {
  temp.plot <- DrawHeat(heatmap.list[[grep(true.name, names(heatmap.list))]], 
                        visiting.list[[grep(true.name, names(visiting.list))]])
  obj.loc   <- grep(true.name, names(object.list))
  mom.loc   <- obj.loc[grep('Mom', names(object.list)[obj.loc])]
  for (i in obj.loc) { 
    temp.plot <- temp.plot + DrawGG(object.list[[i]])
  }
  temp.plot <- temp.plot + DrawRectangleGG2(object.list[[mom.loc]], col.temp = 'green') + 
  theme(plot.margin = unit(c(1,-2,0,-2), "lines"), 
        legend.position="none", 
        plot.title = element_text(size = 18, face = "bold")) + 
  xlab("X (CM)") +
  ylab("Y (CM)") + 
  ggtitle(false.name) 
  return(temp.plot)
  
}



## Excursions plotter 
## Input: fake.name, true.name, and full data 
## Output: Draws all excurions 

DrawExcursions <- function(temp.df, ex.vec, temp.radius, true.name , center.list){
  ex.num  <- max(ex.vec)
  temp.mom.circ <- as.numeric(center.list[[grep(pattern = paste(true.name, 'Mom', sep = '') ,
                                                x = names(center.list))]])
  if (ex.num > 0) {
    par(mfrow = c(ceiling(ex.num / 3), 3))
    for (j in 1:ex.num) {
      temp.df.ex <- subset(temp.df ,ex.vec == j)
      PathPlot(true.name = true.name, 
               false.name = paste(true.name,'Excursion',j), 
               children.df = temp.df.ex,
               object.list = object.list, excursion = F, 
               cex.main = 1.5, xlab = 'X (CM)', ylab = 'Y (CM)' , cex.lab = 1.5) 
      ## Mom Radius  
      draw.circle(temp.mom.circ[1], temp.mom.circ[2], temp.radius , border = 'grey')
      ## Start Point 
      points(temp.df.ex$y[1] ~ temp.df.ex$x[1], 
             pch = 8 , cex = 2.5)
      ## End Point 
      points(temp.df.ex$y[nrow(temp.df.ex)] ~ temp.df.ex$x[nrow(temp.df.ex)], 
             pch = 15, cex = 2.5)
      
    } 
  } else {
    PathPlot(true.name = true.name, 
             false.name = true.name,
             children.df = temp.df,
             object.list = object.list, excursion = F, 
             cex.main = 1.5, xlab = 'X (CM)', ylab = 'Y (CM)' , cex.lab = 1.5) 
  }
  
}
