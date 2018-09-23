### Function For Children Analysis 

## Getting all files from directories 
## Input: Directory and string to find in files list 
## Output: files list in directory containing the target string 

GetObjects <- function(Dir , target, rec = FALSE) {
  files.list <- list.files(Dir , full.name = TRUE, recursive = rec)
  return(files.list[grepl(target , files.list)])
}

## Finding List of directories contaning target string in directory 
## Input: Directory and string to find in directory list, rec - flag if to use recursive search
## Output: directory list in directory containing the target string 
GetDirs <- function(Dir , target, rec = FALSE) { 
  dir.list <- list.dirs(Dir , recursive = rec)
  return(dir.list[grepl(target , dir.list)])
}


## Group vector if there is a difference more than 1 than joined in the same Group
## Input: vector of indices 
## Output: matrix, first column is beginning of following indices 
##         second column is end of the following indices 

Group <- function(vector) 
{
  diff.vector <- diff(vector)
  ent.condition <- vector[c(1,(which(diff.vector > 1) + 1))]
  ext.condition <- vector[c(which(diff.vector > 1),length(vector))]
  return(cbind(ent.condition , ext.condition))
}



## Sequence maker make list of sequences out of table and vector 
## Input: seq.mat (out put of Group) vector.target 
## Output: list of vector.target elements devided by Groups in seq.mat 

SeqMaker <- function(seq.mat, vector.target) { 
  if (nrow(seq.mat) == 1) { 
    return(vector.target[seq.mat[1]:seq.mat[2]])
  }
  seq.list <- mapply(seq , seq.mat[,1] , seq.mat[,2])
  return(lapply(seq.list,  IndiceOutputer , vector.target))
}


## Run function on list 
## Input: list or vector and function 
## Output: vector with result for each vector from the list 

RunOnObject <- function(target, Func) { 
  if (is.list(target) == T) {
    return(sapply(target, Func)) 
  } else { 
    return(Func(target))
  }
}

## Input: vector and indices 
## Output: vector at specific indices 

IndiceOutputer <- function(l, tarVec){
  outVec <- tarVec[c(l)] 
  return(outVec)
}


## Finding progression vector according to threshold 
## Input: Smooth matrix and threshold (speed)
## Output: vector , 0 for lingering 1 for progression 

LingProgMaker <- function(smooth.mat , threshold) {
  n                   <- nrow(smooth.mat)
  speed               <- sqrt((smooth.mat[,'v.x'])^2 + (smooth.mat[,'v.y'])^2)
  speed.nozero.mat    <- Group(which(speed > 0))
  speed.nozero.list   <- SeqMaker(speed.nozero.mat, speed)
  indices.nozero.list <- SeqMaker(speed.nozero.mat, 1:n)
  above.threshold     <- lapply(speed.nozero.list, max) > threshold
  prog.index          <- unlist(indices.nozero.list[above.threshold])  
  ling.prog.vec       <- rep(0,n)
  ling.prog.vec[prog.index] <- 1 
  return(ling.prog.vec)
}


## Center finder function to find all centers of object 
## Handle round and square objects 
## Input: object coordinates 
## Output: object center 

CenterFinder <- function(object) {
  if (length(object) == 3) { 
    center   <- c(object[1], object[2] , object[3])
  } else { 
    center <- apply(object , 2, mean)
    center <- c(center , RadMaker(object))
  }
  center <- as.numeric(center)
  names(center) <- c('x', 'y' ,'r')
  return(center)
}


## Grouping frames based on a certain criterion 
## Input: Vector of 0,1 
## Output: matrix with begin frame and end frame 1's 
group <- function(vector) 
{
  diff.vector <- diff(vector)
  ent.condition <- vector[c(1,(which(diff.vector > 1) + 1))]
  ext.condition <- vector[c(which(diff.vector > 1),length(vector))]
  return(cbind(ent.condition , ext.condition))
}


## Radius maker finds the radius of the object or the circle blocked by it 
## Input: object coordinates (rectangular)
## Output: radius of circle blocked by object 

RadMaker <- function(object)
{
  radius.small <- min(abs(object[2,] - object[1,]) / 2)
  return(c('r' = radius.small))
}

### Simple Excursions Finder 
### Input: Center point, Radius 
### Output: Number of Excursions 

SimpleExcursions <- function(smooth.table, center, rad = 25){ 
  ex.mat <- group(which((smooth.table$x - center[1])^2 + (smooth.table$y - center[2])^2 < rad^2))
  return(ex.mat)
}


### Minimum finder 
### 

FindMin <- function(coord.vec, dist.vec) { 
  if (length(coord.vec) <= 1) {
    return(NULL)
  } else {
    return(which.min(dist.vec[coord.vec[1]:coord.vec[2]]) + coord.vec[1] - 1)
  }
}



## Creating Circle 

circleFun <- function(center = c(0,0), r, npoints = 300){
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}


## Heatmap 
NoTess <- function(island.center, smooth.table, rad.calc, rad.draw, rad.mom) {
  vis.list    <- list()
  last.seg    <- nrow(island.center)
  coord.df    <- smooth.table[,c('x' ,'y')]
  for (i in 1:(last.seg - 1)) { 
    temp.visit    <-  DoubleVisit(island.center[i,], coord.df, s.rad = rad.calc, l.rad = rad.calc + 20)
    vis.list[[i]] <-  temp.visit[!is.na(temp.visit[,1]), , drop = F]
  }
  if (rad.mom > 0) {
    temp.visit    <-  DoubleVisit(island.center[last.seg,], coord.df, s.rad = rad.mom, l.rad = rad.mom + 30)
    vis.list[[last.seg]] <-  temp.visit[!is.na(temp.visit[,1]), , drop = F]
  }
  if (rad.mom == 0) { 
    temp.visit    <-  DoubleVisit(island.center[last.seg,], coord.df, s.rad = rad.calc)
    vis.list[[last.seg]] <-  temp.visit[!is.na(temp.visit[,1]),, drop = F]
  }
  visit.num  <- unlist(lapply(vis.list, nrow))
  #gini.coef.visit <- round(unlist(lapply(tile.visit.list, function(x) ineq(c(x[-1,1], nrow(smooth.table)) - x[,2], type = 'Gini'))), 2)
  z.ent <- (unlist(lapply(vis.list, function(x) x[ ,1])))
  z.ext <- (unlist(lapply(vis.list, function(x) x[ ,2])))
  draw.visit.df <- data.frame('x' = rep(island.center[,1], visit.num), 
                              'y' = rep(island.center[,2], visit.num),
                              'z.ent' = (z.ent / nrow(smooth.table)) * 2 * pi,
                              'z.ext' = (z.ext / nrow(smooth.table)) * 2 * pi,
                              'r'     = rad.draw,
                              'Visit.Num' = 1:sum(visit.num))
  arc.df <- CreateArcs(draw.visit.df)
  draw.visit.df <- melt(draw.visit.df, id.vars = c('x','y','r','Visit.Num'), value.name = 'z')
  return(list('Arcs' = arc.df, 'Visits' = draw.visit.df))
}

## Islands 

FindIslands <- function(wide.heat.smooth, quant) { 
  y.dim <- ncol(wide.heat.smooth)
  x.dim <- nrow(wide.heat.smooth)
  island.group <- GroupIslands(wide.heat.smooth > quantile(wide.heat.smooth, quant))
  return(island.group)
}

FindMax <- function(wide.heat.smooth, quant) { 
  y.dim <- ncol(wide.heat.smooth)
  x.dim <- nrow(wide.heat.smooth)
  wide.heat.smooth[wide.heat.smooth < quantile(wide.heat.smooth, quant)] <- 0
  M1 <- cbind(0,wide.heat.smooth[ ,-y.dim]); M2 <- cbind(wide.heat.smooth[ ,-1], 0); M3 <- rbind(0, wide.heat.smooth[-x.dim, ])
  M4 <- rbind(wide.heat.smooth[-1, ], 0);
  M5 <- rbind(0,M1[-1, ]); M6 <- rbind(0, M2[-1, ]); M7 <- rbind(M1[-y.dim,],0); M8 <- rbind(M2[-1,], 0);
  max.point <- which(wide.heat.smooth >= M1 & wide.heat.smooth > M2 & wide.heat.smooth > M3 &
                       wide.heat.smooth > M4 & wide.heat.smooth > M5 & wide.heat.smooth > M6 &
                       wide.heat.smooth > M7 & wide.heat.smooth > M8, arr.ind = TRUE)
  max.point[,1] <- x.dim - max.point[,1]
  colnames(max.point) <- c('y', 'x'); max.point <- max.point[,2:1];
  return(max.point)
}  


tile.group <- function(visit.time) {
  seg <- max(visit.time)
  vis.seg <- unique(visit.time)
  visit.list <- list()
  for (i in 1:seg) { 
    visit.list[[i]] <- group(which(visit.time == i))
    print(paste(i, visit.list[[i]]))
  }
  return(visit.list)
}

MeanIslands <- function(island.group, wide.heat.smooth) {
  island.num   <- max(as.numeric(island.group), na.rm = T)
  island.center <- matrix(NA, ncol = 2, nrow = island.num)
  x.dim <- nrow(wide.heat.smooth)
  for (i in 1:island.num) {
    arr.ind.island <- which(island.group == i, arr.ind = T)
    val.island     <- wide.heat.smooth[arr.ind.island] 
    island.center[i,] <- round(apply(arr.ind.island, 2, weighted.mean, w = wide.heat.smooth[arr.ind.island]))
  }
  island.center[,1] <- x.dim - island.center[,1]
  colnames(island.center) <- c('y', 'x'); island.center <- island.center[,2:1];
  return(island.center) 
}


## Percentage of room covered 
CountMap <- function(smooth.table) { 
  x.vec = seq(0, 367, 1); y.vec = seq(0, 546, 1);
  temp.grid <- expand.grid('X' = x.vec,'Y' = y.vec)
  temp.loc <- plyr::count(cbind('X' = floor(smooth.table$x), 'Y' = floor(smooth.table$y))); 
  names(temp.loc) <- c('X', 'Y', 'Count');
  heat.grid <- plyr::join(temp.grid, temp.loc, type = 'left', by = c('X','Y'))
  return(mean(!is.na(heat.grid[,3])))
}

DoubleVisit <- function(center.coord, smooth.table, s.rad, l.rad = 1.1 *  s.rad) { 
  s.visit <- which((smooth.table$x - center.coord[1])^2 + (smooth.table$y - center.coord[2])^2 < s.rad^2)
  l.visit <- SimpleExcursions(smooth.table, center.coord, l.rad)
  return(l.visit[apply(l.visit, 1, IsInRange, s.visit), , drop = F])
}

IsInRange <- function(int.vec, target.vec) { 
  return(any(target.vec >= min(int.vec) & target.vec <= max(int.vec)))
}

MatchTessilate <- function(visits, coord.df, tess.num, tile.list) {
  ## Corecing matrix 
  visits <- data.frame(matrix(visits, ncol = 2))
  for (i in 1:nrow(visits)) { 
    current.seq <- seq(visits[i,1],visits[i,2])
    fit <- apply(coord.df[current.seq, ], 
                 1, function(x) which.tile(x[1], x[2], tile.list)) == tess.num
    temp.group  <- group(current.seq[fit])
    visits[i, ] <- temp.group
  }
  return(visits)
}


visit.find <- function(coord.df, tess.obj, rad.calc = 25, rad.mom) { 
  vis.list    <- list()
  tile.list   <- tile.list(tess.obj)
  vis.centers <- as.matrix(tess.obj$summary[,c('x' ,'y')])
  last.seg    <- nrow(vis.centers)
  for (i in 1:(last.seg - 1)) { 
    temp.visit    <-  DoubleVisit(vis.centers[i,], coord.df, s.rad = rad.calc, l.rad = rad.calc + 20)
    temp.visit    <-  MatchTessilate(temp.visit, coord.df, i, tile.list)
    vis.list[[i]] <-  temp.visit[!is.na(temp.visit[,1]),]
  }
  if (rad.mom > 0) {
    temp.visit    <-  DoubleVisit(vis.centers[last.seg,], coord.df, s.rad = rad.mom, l.rad = rad.mom + 30)
    temp.visit    <-  MatchTessilate(temp.visit, coord.df, last.seg, tile.list)
    vis.list[[last.seg]] <-  temp.visit[!is.na(temp.visit[,1]),]
  }
  if (rad.mom == 0) { 
    temp.visit    <-  DoubleVisit(vis.centers[last.seg,], coord.df, s.rad = rad.calc, l.rad = rad.calc + 20)
    temp.visit    <-  MatchTessilate(temp.visit, coord.df, last.seg, tile.list)
    vis.list[[last.seg]] <-  temp.visit[!is.na(temp.visit[,1]),]
  }
  return(vis.list)
}


WrapIslands <- function(wide.heat.smooth, 
                        smooth.table, 
                        rad.draw = 50, 
                        rad.calc = 50, 
                        rad.mom  = 0,
                        quant, 
                        find.max = F, 
                        mom.cent = F,
                        tess = F) {
  if (find.max == T) { 
    center.df <- FindMax(wide.heat.smooth, quant)
  }
  if (find.max == F) { 
    center.df <- MeanIslands(FindIslands(wide.heat.smooth, quant), wide.heat.smooth)
  }
  if (rad.mom == 0 && tess == T) {
    visit.list <- Tessilate(center.df, smooth.table, rad.calc, rad.draw, rad.mom)
  }
  if (rad.mom > 0 && tess == T) {
    center.df <- center.df[!((center.df[,1] - mom.cent[1])^2 + (center.df[,2] - mom.cent[1])^2) < rad.mom^2,] 
    center.df <- rbind(center.df, mom.cent) 
    visit.list <- Tessilate(center.df, smooth.table, rad.calc, rad.draw, rad.mom)
  }
  if (rad.mom > 0 && tess == F) {
    center.df <- center.df[!((center.df[,1] - mom.cent[1])^2 + (center.df[,2] - mom.cent[1])^2) < rad.mom^2,] 
    center.df <- rbind(center.df, mom.cent) 
    visit.list <- NoTess(center.df, smooth.table, rad.calc, rad.draw, rad.mom)
  }
  if (rad.mom == 0 && tess == F) { 
    visit.list <- NoTess(center.df, smooth.table, rad.calc, rad.draw, rad.mom)
  }
  return(visit.list)
}




HeatMaker <- function(smooth.table, 
                      x.vec = seq(0, 367, 1), 
                      y.vec = seq(0, 546, 1), 
                      sig = 9, n = 57, 
                      rad.draw = 25, 
                      rad.calc = 60,
                      rad.mom = 0,
                      quant = 0.95, 
                      mom.cent,
                      find.max = F,
                      tess = F) {
  temp.grid <- expand.grid('X' = x.vec,'Y' = y.vec)
  temp.loc  <- plyr::count(cbind('X' = floor(smooth.table$x), 'Y' = floor(smooth.table$y))); names(temp.loc) <- c('X', 'Y', 'Count');
  heat.grid <- plyr::join(temp.grid, temp.loc, type = 'left', by = c('X','Y'))
  heat.grid[is.na(heat.grid)] <- 0
  wide.heat <- acast(heat.grid,Y ~ X, value.var = 'Count')
  wide.heat <- wide.heat[nrow(wide.heat):1, ]
  wide.heat.smooth <- kernel2dsmooth(wide.heat, kernel.type="gauss", nx = n, ny = n, sigma = sig)
  arc.list <- WrapIslands(wide.heat.smooth, smooth.table, 
                          rad.draw = rad.draw, rad.calc = rad.calc, rad.mom = rad.mom,
                          quant, find.max = find.max, mom.cent, tess)
  return(list('Heatmap' = grid.to.xyz(wide.heat.smooth) ,
              'Visit' = arc.list$Visits,
              'Arcs'= arc.list$Arcs,
              'Voroni' = arc.list$Voroni))
}



## Tesselation Fucntion 
Tessilate <- function(island.center, smooth.table, rad.calc, rad.draw, rad.mom) { 
  tess.obj <- deldir(island.center[,1], island.center[,2])
  tile.visit.list <- visit.find(smooth.table[ ,c('x', 'y')], tess.obj, rad.calc = rad.calc, rad.mom)
  visit.num  <- unlist(lapply(tile.visit.list, nrow))
  #gini.coef.visit <- round(unlist(lapply(tile.visit.list, function(x) ineq(c(x[-1,1], nrow(smooth.table)) - x[,2], type = 'Gini'))), 2)
  z.ent <- (unlist(lapply(tile.visit.list, function(x) x[,1])))
  z.ext <- (unlist(lapply(tile.visit.list, function(x) x[,2])))
  draw.visit.df <- data.frame('x' = rep(island.center[,1], visit.num), 
                              'y' = rep(island.center[,2], visit.num),
                              'z.ent' = (z.ent / nrow(smooth.table)) * 2 * pi,
                              'z.ext' = (z.ext / nrow(smooth.table)) * 2 * pi,
                              'r'     = rad.draw,
                              'Visit.Num' = 1:sum(visit.num))
  arc.df <- CreateArcs(draw.visit.df)
  draw.visit.df <- melt(draw.visit.df, id.vars = c('x','y','r','Visit.Num'), value.name = 'z')
  return(list('Arcs' = arc.df, 'Visits' = draw.visit.df, 'Voroni' = tess.obj$dirsgs))
}

CreateArcs <- function(visit.df) {
  n <- nrow(visit.df) 
  arc.df <- visit.df[rep(1:n, times = 150), ] %>% arrange(Visit.Num) %>%
    mutate(arc.cord = unlist(mapply(
      seq,
      visit.df[, 'z.ent'],
      visit.df[, 'z.ext'],
      length.out =  150,
      SIMPLIFY = FALSE))) 
  return(arc.df)
}


### Logit 

Logit <- function(x){
  if (any (x <= 0) | any(x > 1)) { 
    stop('Input is not between 0 and 1')
    }
  return(log(x / (1 - x)))
}
