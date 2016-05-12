
##########################################################################
#function that returns the complete data with three additional columns

completeData <- function(file1, file2)
{
  if(!is.null(file1) & !is.null(file2)){
    data1 <- tryCatch(read.csv(paste0(file1, ".csv")), error = function(c) {
      c$message <- paste0(c$message, "( Incorrect file name - ", file1, ")")
      stop(c)
    })
    data2 <- tryCatch(read.csv(paste0(file2, ".csv")), error = function(c) {
      c$message <- paste0(c$message, "( Incorrect file name - ", file2, ")")
      stop(c)
    })
  } else {
    stop('Missing file names')
  }
  
  
  #file that contains yield is harvest file
  commonVar <- intersect(colnames(data1), "yield")
  if(!is.null(commonVar)) {
    assign("harvest", data1)
    assign("plant", data2)
  } else {
    assign("harvest", data2)
    assign("plant", data1) 
  }
  
  #nearest neighbour to harvest points using KD-TREE
  ptm <- proc.time()
  nearestPoints_KDTree <- kd_tree(plant[,c("long", "lat")], harvest[,c("long", "lat")])
  proc.time() - ptm
 
  finalData <-as.data.frame(cbind(harvest, plant[nearestPoints_KDTree, c("variety", "seeding_rate", "seed_spacing")]))
  
  #check if number of rows in finalData is same as that of harvest
  return (finalData)
  
}

############# Plant Harvest Point Association Algorithms ################################
##################### Method 1 - Implementing KD Tree ##############################################################
#FNN package contains the function that implements kd-tree nearest neighbour search
library(FNN)
kd_tree <- function(data1, data2){
  model <- get.knnx(data1,data2, 1, algorithm=c("kd_tree"))
  
  return (model$nn.index) 
}

################# Method 2 - DISTANCE BASED #############################################
#find the distance for each of the harvest point with the planting point, and then choose minimum distance 
#planting point

closestPoint <- function(x, data){
  dis <- (x[1]-data$long)^2 + (x[2]-data$lat)^2
  
  return(which.min(dis))
}

################# Method 3 - Recursive Partioning based only on Latitude ####################################

recursive_Lat <- function(par, data)
{
  harLong <- par[1]
  harLat <- par[2]
  
  point <- data[sample(1:nrow(data), 1), "lat"]
  if(abs(harLat - point) < .0001){
    temp <- closestPoint(par, data)
    return (data[temp,1])
  }
  
  if(harLat > point){  
    recursive_Lat(par, data=data[data$lat > point ,])
  } else if(harLat < point ){
    recursive_Lat(par, data=data[data$lat < point,])
  } else {
    temp <- closestPoint(par, data)
    return (data[temp,1])
  }
}

####################### Method 4 - Search a point in a reduced planting space ####################
#search for nearest neighbour for each of the harvest points only in reduced planting space
nearestPoint_subset <- function(par, data)
{
  harLong <- par[1]
  harLat <- par[2]
  
  #subset the data
  data <- data[((data$lat > (harLat-.0001) & data$lat < (harLat+.0001)) & (data$long > (harLong-.0001) & data$long < (harLong +.0001)) ), ]
  temp <- closestPoint(par, data)
  
  return (data[temp,1])
}

###################### RandomForest Implementation ########################

library(randomForest)
randomforest_model <- function(train, test){
  model.rf <- randomForest(yield ~ variety + seeding_rate +seed_spacing , data=train, importance=TRUE, ntree=2000)
  prediction <- predict(model.rf, test)
  
  return (prediction)
}
##############################################################################