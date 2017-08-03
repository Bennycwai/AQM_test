## check diceR stability output

## clean up, order and rename--------------------------
rm(list = ls())
setwd("C:/Main/AQM/Translink_Project/diceR_outputs")

diceRoutput <- readRDS("C:/Main/AQM/Translink_Project/diceR_outputs/DelayEvents_2016_AllAlgoOutput_hc20by20.rds")
colnames(diceRoutput)

# re-ordering
diceRoutput <- diceRoutput[,c(1:41,seq(42,240,2),seq(242,440,2),seq(442,640,2),seq(642,840,2),seq(842,1040,2),seq(43,241,2),seq(243,441,2),seq(443,641,2),seq(643,841,2),seq(843,1041,2),1042:1541)]

# re-naming (probably a smarter way to do this, but just simple copy and paste)
colnames(diceRoutput)[42:141] <- paste("CMEANS_02_", 1:100, sep="")
colnames(diceRoutput)[142:241] <- paste("CMEANS_04_", 1:100, sep="")
colnames(diceRoutput)[242:341] <- paste("CMEANS_06_", 1:100, sep="")
colnames(diceRoutput)[342:441] <- paste("CMEANS_08_", 1:100, sep="")
colnames(diceRoutput)[442:541] <- paste("CMEANS_10_", 1:100, sep="")
colnames(diceRoutput)[542:641] <- paste("HCSM10_02_", 1:100, sep="")
colnames(diceRoutput)[642:741] <- paste("HCSM10_04_", 1:100, sep="")
colnames(diceRoutput)[742:841] <- paste("HCSM10_06_", 1:100, sep="")
colnames(diceRoutput)[842:941] <- paste("HCSM10_08_", 1:100, sep="")
colnames(diceRoutput)[942:1041] <- paste("HCSM10_10_", 1:100, sep="")
colnames(diceRoutput)[1042:1141] <- paste("HCSM20_02_", 1:100, sep="")
colnames(diceRoutput)[1142:1241] <- paste("HCSM20_04_", 1:100, sep="")
colnames(diceRoutput)[1242:1341] <- paste("HCSM20_06_", 1:100, sep="")
colnames(diceRoutput)[1342:1441] <- paste("HCSM20_08_", 1:100, sep="")
colnames(diceRoutput)[1442:1541] <- paste("HCSM20_10_", 1:100, sep="")

colnames(diceRoutput)

DelayEvents_2016_orderedOutput <- diceRoutput

#saveRDS(DelayEvents_2016_orderedOutput, file = "DelayEvents_2016_orderedOutput.rds")

# check top k most frequent clusters, used for dbscan
k = 5
sort(table(diceRoutput[,41]),decreasing=TRUE)[1:k]


## preparing for the hungarian method ---------------------
rm(list = ls())
setwd("C:/Main/AQM/Translink_Project/diceR_outputs")


DelayEvents_2016_orderedOutput <-  readRDS("C:/Main/AQM/Translink_Project/diceR_outputs/DelayEvents_2016_orderedOutput.rds")
colnames(DelayEvents_2016_orderedOutput)


## get matrix rows for specified 100 columns (one at a time) -----------------

start.time <- Sys.time()

## take in the 100 columns of interest (input 42:141, 142:241, ect)
outputSubset <- DelayEvents_2016_orderedOutput[,42:141]

## setting up k
k <- as.integer(substr(colnames(outputSubset)[1],8,9))
## get method name for saving later
methodName <- substr(colnames(outputSubset)[1],1,6)

matrixRows <- matrix(0, nrow = (k*(ncol(outputSubset)-1)), ncol = k)


# b is the bth run compared to the first run
for (b in 1:(ncol(outputSubset)-1))
{
  ## grab current columns
  currCols <- outputSubset[,c(1,(b+1))]
  
  ## if at least one side has NA, not applicable: remove to save run time
  currCols <- currCols[complete.cases(currCols),]
  
  # i is for each data point
  for (i in 1:nrow(currCols))
  {
    # t is the cluster number of the first run
    for (t in 1:k)
    {
      # f is the cluster number of the bth run
      for (f in 1:k)
      {
        # add to correct matrix location
        if (currCols[i,1] == t && currCols[i,2] == f)
        {
          matrixRows[(b-1)*k+t,f] <- matrixRows[(b-1)*k+t,f] + 1
        }
      }
    }
  }
}

# calculate run time
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

## rename based on k and method
assign(paste0("matrixRows_", methodName,"_",k), matrixRows)
saveRDS(get(paste0("matrixRows_", methodName,"_",k)),file = paste0(paste0("matrixRows_", methodName,"_",k),'.rds'),compress=TRUE)


## get matrix rows for specified 100 columns (all at the same time) -----------------
## WARNING: WILL TAKE A LONG TIME (few days?)
start.time <- Sys.time()

# for each method for specific k
for (j in 1:15)
{
  ## take in the 100 columns of interest
  outputSubset <- DelayEvents_2016_orderedOutput[,((j-1)*100+42):((j-1)*100+141)]
  ## setting up k
  k <- as.integer(substr(colnames(outputSubset)[1],8,9))
  ## get method name for saving later
  methodName <- substr(colnames(outputSubset)[1],1,6)
  
  matrixRows <- matrix(0, nrow = (k*(ncol(outputSubset)-1)), ncol = k)
  
  
  # b is the bth run compared to the first run
  for (b in 1:(ncol(outputSubset)-1))
  {
    ## grab current columns
    currCols <- outputSubset[,c(1,(b+1))]
    
    ## if at least one side has NA, not applicable: remove to save run time
    currCols <- currCols[complete.cases(currCols),]
    
    # i is for each data point
    for (i in 1:nrow(currCols))
    {
      # t is the cluster number of the first run
      for (t in 1:k)
      {
        # f is the cluster number of the bth run
        for (f in 1:k)
        {
          # add to correct matrix location
          if (currCols[i,1] == t && currCols[i,2] == f)
          {
            matrixRows[(b-1)*k+t,f] <- matrixRows[(b-1)*k+t,f] + 1
          }
        }
      }
    }
  }
  
  # calculate run time
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  ## rename based on k and method
  assign(paste0("matrixRows_", methodName,"_",k), matrixRows)
  saveRDS(get(paste0("matrixRows_", methodName,"_",k)),file = paste0(paste0("matrixRows_", methodName,"_",k),'.rds'),compress=TRUE)
  
}
