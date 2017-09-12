# getting matrices for hungarian method

# read the file with the method and cluster number of interest
outputSubset <- readRDS("HC_SOM_20_04_SET.rds")

## setting up k
k <- as.integer(substr(colnames(outputSubset)[1],11,12))
## get method name for saving later
methodName <- substr(colnames(outputSubset)[1],1,9)

matrixRows <- matrix(0, nrow = (k*(ncol(outputSubset)-1)), ncol = k)

##NOW-----------------------

library(inline)

doublematrix <- cxxfunction(signature(x = "numeric"), body = '
Rcpp::NumericMatrix xcpp(x);
int nrows = xcpp.nrow();
int ncolumns = xcpp.ncol();
for (int i = 0; i < nrows; i++){
    for (int j = 0; j < ncolumns; j++){
                            xcpp[nrows * j + i] *= 2;
                            }
                            }
                            return xcpp;
                            ', plugin="Rcpp")
print(doublematrix(matrix(1:10, nrow = 2)))


##LATER---------------------------
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

## rename based on k and method
assign(paste0("matrixRows_", methodName,"_",k), matrixRows)
saveRDS(get(paste0("matrixRows_", methodName,"_",k)),file = paste0(paste0("matrixRows_", methodName,"_",k),'.rds'),compress=TRUE)


rm(list = ls())
setwd("C:/Main/AQM/Translink_Project/diceR_outputs/Hung_prep_cplusplus")

HC_SOM_20_04_SET <- readRDS("HC_SOM_20_04_SET.rds")
write.table(HC_SOM_20_04_SET, file = "HC_SOM_20_04_SET.csv",sep = ",", row.names = FALSE, col.names = FALSE)

