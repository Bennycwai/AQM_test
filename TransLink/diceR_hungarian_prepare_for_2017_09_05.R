## check diceR stability output

## clean up and output files with 20--------------------------
rm(list = ls())
setwd("C:/Main/AQM/Translink_Project/diceR_outputs")

diceRoutput <- readRDS("C:/Main/AQM/Translink_Project/diceR_outputs/DelayEventsNormalized_HCSOM_20.rds")
colnames(diceRoutput)

# grab columns of originals for saving---
ORIGINAL_VAR <- diceRoutput[,1:31]
  
# grab output cols only
diceRoutput <- diceRoutput[,32:length(diceRoutput)]
colnames(diceRoutput)

# not the same length, so have to do by hand
HC_SOM_20_02_SET <- diceRoutput[,1:34]
HC_SOM_20_04_SET <- diceRoutput[,35:85]
HC_SOM_20_06_SET <- diceRoutput[,86:117]
HC_SOM_20_08_SET <- diceRoutput[,118:168]
HC_SOM_20_10_SET <- diceRoutput[,169:268]
HC_SOM_20_12_SET <- diceRoutput[,269:309]
HC_SOM_20_14_SET <- diceRoutput[,310:345]
HC_SOM_20_16_SET <- diceRoutput[,346:389]
HC_SOM_20_18_SET <- diceRoutput[,390:426]
HC_SOM_20_20_SET <- diceRoutput[,427:476]

# rename first column the name of the file (to simplify for hugarian prepare file)
colnames(HC_SOM_20_02_SET)[1] <- "HC_SOM_20_02_SET"
colnames(HC_SOM_20_04_SET)[1] <- "HC_SOM_20_04_SET"
colnames(HC_SOM_20_06_SET)[1] <- "HC_SOM_20_06_SET"
colnames(HC_SOM_20_08_SET)[1] <- "HC_SOM_20_08_SET"
colnames(HC_SOM_20_10_SET)[1] <- "HC_SOM_20_10_SET"
colnames(HC_SOM_20_12_SET)[1] <- "HC_SOM_20_12_SET"
colnames(HC_SOM_20_14_SET)[1] <- "HC_SOM_20_14_SET"
colnames(HC_SOM_20_16_SET)[1] <- "HC_SOM_20_16_SET"
colnames(HC_SOM_20_18_SET)[1] <- "HC_SOM_20_18_SET"
colnames(HC_SOM_20_20_SET)[1] <- "HC_SOM_20_20_SET"

for (i in 1:10)
{
  k <- i*2
  if (k < 10)
  {
    saveRDS(get(paste0("HC_SOM_20_0", k,"_SET")),file = paste0(paste0("HC_SOM_20_0", k,"_SET"),'.rds'),compress=TRUE)
  }
  else
  {
    saveRDS(get(paste0("HC_SOM_20_", k,"_SET")),file = paste0(paste0("HC_SOM_20_", k,"_SET"),'.rds'),compress=TRUE)
  }
}

saveRDS(ORIGINAL_VAR, file = "ORIGINAL_VAR.rds")

## clean up and output files with 50--------------------------
rm(list = ls())
setwd("C:/Main/AQM/Translink_Project/diceR_outputs")

diceRoutput <- readRDS("C:/Main/AQM/Translink_Project/diceR_outputs/DelayEventsNormalizedHC_SOM50.rds")
colnames(diceRoutput)

# grab columns of originals for saving---
ORIGINAL_VAR <- diceRoutput[,1:31]

# grab output cols only
diceRoutput <- diceRoutput[,31:length(diceRoutput)]
colnames(diceRoutput)

# not the same length, so have to do by hand
HC_SOM_50_10_SET <- diceRoutput[,1:100]
HC_SOM_50_12_SET <- diceRoutput[,101:200]
HC_SOM_50_14_SET <- diceRoutput[,201:300]
HC_SOM_50_16_SET <- diceRoutput[,301:400]
HC_SOM_50_18_SET <- diceRoutput[,401:500]
HC_SOM_50_20_SET <- diceRoutput[,501:523]

# rename first column the name of the file (to simplify for hugarian prepare file)
colnames(HC_SOM_50_10_SET)[1] <- "HC_SOM_50_10_SET"
colnames(HC_SOM_50_12_SET)[1] <- "HC_SOM_50_12_SET"
colnames(HC_SOM_50_14_SET)[1] <- "HC_SOM_50_14_SET"
colnames(HC_SOM_50_16_SET)[1] <- "HC_SOM_50_16_SET"
colnames(HC_SOM_50_18_SET)[1] <- "HC_SOM_50_18_SET"
colnames(HC_SOM_50_20_SET)[1] <- "HC_SOM_50_20_SET"

for (i in 0:5)
{
  k <- 10+i*2
  saveRDS(get(paste0("HC_SOM_50_", k,"_SET")),file = paste0(paste0("HC_SOM_50_", k,"_SET"),'.rds'),compress=TRUE)
}

