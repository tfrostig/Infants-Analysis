
## Looping through each infant applying SEE and adding Touching 
p <- nrow(Parameter.DF)
see.list <- list()

for (i in 1:p) { 
  temp.name     <- Parameter.DF[i, 'Name'] 
  temp.link     <- GetObjects(GetDirs(data.dir, paste0(temp.name,'/Movment'), TRUE), 'Coordinates')[1] ## SEE creates two copies
  Parameter.DF[i, 'Link'] <- temp.link
  temp.smooth   <- see.smooth(Filename        = temp.link,
                              HalfWindowWidth = Parameter.DF[i, 'HalfWindowWidth'],
                              MovingMedCutOffValue = Parameter.DF[i, 'MovingMedCutOffValue'],
                              Arrests = Parameter.DF[i, 'Arrests'],
                              silent = T)
  temp.smooth   <- temp.smooth[['smoothed']] ## Removing original coordinates 
  ## Segmenting to progression and lingering 
  temp.seg                     <- see.segment(temp.smooth) ## Segmenting 
  Parameter.DF[i, 'Threshold'] <- temp.seg[['thresholdNonTransformed']] ## Saving found threshold 
  temp.smooth[ ,'prog.vec']    <- LingProgMaker(temp.smooth, temp.seg[['thresholdNonTransformed']]) 
  ## Adding Touches 
  touching.seg  <- read.csv(GetObjects(GetDirs(data.dir, paste0(temp.name,'/Notation'), TRUE), 'Gaze'))
  touching.seg  <- touching.seg[seq(Parameter.DF[i,'Begin'], Parameter.DF[i,'End'], 1) ,'Touch']
  temp.smooth[ ,'Touch'] <- touching.seg
  see.list[[i]] <- temp.smooth
  print(paste('Done with', temp.name))
}

## Naming list 
names(see.list) <- Parameter.DF[, 'Name']


# ## Saving Results 
# saveRDS(see.list, 'Output/smooth.see.list.RDS')
# write.csv(Parameter.DF, 'Output/ParameterDF_Updated.csv', row.names = FALSE)
