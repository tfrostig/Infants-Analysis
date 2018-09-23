## Can be downloaded using
## install.packages("RSEE", repos=c("http://cs.tau.ac.il/~itamares/rsee/R","http://cran.r-project.org"))

## Loading functions 
source('R Scripts/Package required.R')
source('R Scripts/Functions.R')

### Loading parameters for the SEE algorithm 
Parameter.DF <- read.csv(GetObjects('Input', 'Parameters'), 
                         stringsAsFactors = FALSE)
p <- nrow(Parameter.DF)

## Data directory - change to the corresponding directory 
data.dir     <- paste0(getwd(),'/Infants Data')

## Applying SEE 
source('R Scripts/RSEE_smoothing.R')
saveRDS(see.list, 'Output/smooth_see_list.RDS')

## Finding Objects 
source('R Scripts/Object List Creator.R')


## Saving Results 
saveRDS(center.list, 'Output/center_list.RDS')
saveRDS(object.list, 'Output/object_list.RDS')
write.csv(Parameter.DF, 'Output/ParameterDF_Updated.csv', row.names = FALSE)


