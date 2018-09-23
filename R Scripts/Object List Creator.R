

## Looping through each infant and applying SEE 
p <- nrow(Parameter.DF)
see.list <- list()
object.list <- list()

for (i in 1:p) { 
  temp.name        <- Parameter.DF[i, 'Name'] 
  ## Finding where objects locations are stored 
  temp.folder      <- GetDirs(data.dir, paste0(temp.name,'/Objects'), TRUE)
  ## Names of objects 
  temp.list        <- sapply(list.files(temp.folder, full.names = TRUE),
                          read.csv, header = FALSE)
  ## Naming objects  
  names(temp.list) <- gsub('.csv', '', list.files(temp.folder))
  ## Adding Doors 
  ## Doors Matrices 
  temp.list[['Exit Door']]    <- rbind(c(0, 0), c(10, 80))
  temp.list[['Service Door']] <- rbind(c(0, 545), c(80, 535))  
  ## Transforming From Data frame to matrcies and saving 
  object.list[[i]] <- lapply(temp.list, as.matrix)
}

## Naming list 
names(object.list) <- Parameter.DF[ ,'Name']


## Finding the center of all objects 
center.list  <- lapply(object.list, lapply, CenterFinder)
