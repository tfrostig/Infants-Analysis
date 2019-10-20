#### RSEE Dependencies 
pacakge.list <- c('here','Rcpp', 'data.table', 'ggplot2', 'RInside', 
                  'mixtools', 'timeSeries', 'timeDate', 'dplyr', 'smoothie', 'RSAGA', 'scales',
                  'shape', 'reshape2', 'gridExtra', 'cowplot', 'rmarkdown', 'knitr', 'grid', 'viridis')


#### Installing missing libraries 
new.packages <- pacakge.list[!(pacakge.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#### Loading packages 
lapply(pacakge.list, require, character.only = TRUE)

#### Dealing with RSEE 
to.install <- !('RSEE' %in% installed.packages()[,"Package"])

if (to.install) {
  install.packages(paste0(here::here(),'/RSEE_1.5.2.zip'), type = "win.binary", repos = NULL)
}

library('RSEE')