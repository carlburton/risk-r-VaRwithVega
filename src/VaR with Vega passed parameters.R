#### Runs Value at Risk ####

#### Program calculates VaR at five levels -- Firm, Group, Location, Grouping, and BaseUsym ####

#### INPUTS
#### Requires two arguments -- date of the correlation file, and date of the input file (both in 'yyyymmdd' format)
#### correlation file and input file must be in respective folders 'N:\\Ned.Consultant\\VaR\\Hist\\ [yyyymmdd] \\
#### correlation file must be named [yyyymmdd] & '_mat_corr_input.csv'
#### input file must be named [yyyymmdd] & '_mat_mv_details.csv'
#### the inputs for Vega is built into the table 'mat_mv_details'; see ???? for details

#### OUTPUT FILES
#### FirmVaR - "N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_out_var_firm.csv"
#### ResultVaRGrouping - "N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_out_varm_grouping.csv"
#### ResultVaRGroup - "N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_out_varm_group.csv"
#### ResultVaRBaseUsym - "N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_out_varm_baseusym.csv"
#### ResultVaRLocation - "N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_out_varm_location.csv"


library(pastecs)
library(Matrix)
library(data.table)
args<-commandArgs(TRUE)


#### function to calculate VaR, returns vecVar
funcVaRCalc <- function(vecRic, listGreeks, idxVol, matGammaVol, matCov, vecCovWgt, FirmVaR) {
  
  dtListGreeks <- data.table(listGreeks)
  setkey(dtListGreeks,Base.Usym)
  
  ## aggregate by Base.Usym
  dtListGreeksAgg <- dtListGreeks[,list(DeltaVaRI=sum(DeltaVaRI),DeltaVaRM=sum(DeltaVaRM),DeltaVaR=sum(DeltaVaR),GammaVaRI=sum(GammaVaRI),GammaVaRM=sum(GammaVaRM),GammaVaR=sum(GammaVaR)),by=Base.Usym]
  
  dtVecRic <- data.table(vecRic)
  
  dtListGreeksRic  <- dtListGreeksAgg[dtVecRic]
  
  dtListGreeksRic[is.na(dtListGreeksRic)] <- 0
  listGreeksRic <- as.data.frame(dtListGreeksRic)
  
  idxRic <- match(vecRic,listGreeksRic[, 1])
  listGreeksRicOrig <- listGreeksRic[idxRic,]
  
  listGamma <- as.matrix(listGreeksRicOrig[, c("GammaVaRI", "GammaVaRM", "GammaVaR")]) * 100 / 2
  vecGammaCovWgt <-  diag(t(listGamma) %*% matGammaVol %*% listGamma)
  listDelta <- as.matrix(listGreeksRicOrig[, c("DeltaVaRI", "DeltaVaRM", "DeltaVaR")])
  vecWgtCovWgt <- diag(t(listDelta) %*% matCov %*% listDelta)
  
  vecVaR <- sqrt(vecWgtCovWgt + vecGammaCovWgt) * 2.326 / 16 ## 2.326 is 99% confidence value; 16 is sqrt(256 days)
  vecVaR[2] <- (FirmVaR - vecVaR[2]) * 10000
  vecVaR[1] <- (FirmVaR - vecVaR[1])
  return(vecVaR)
  
}

message("test1")
## Read in files
inpMatCorr <- read.table(paste("N:\\Ned.Consultant\\VaR\\Hist\\", args[1], "\\", args[1], "_mat_corr_input.csv", sep=""), sep = ",")
inpMatMvDet <- read.table(paste("N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_mv_details.csv", sep=""), header = TRUE, sep = ",", stringsAsFactors=FALSE)
#inpMatCorr <- read.table(paste("N:\\Ned.Consultant\\VaR\\Hist\\20140811\\20140811_mat_corr_input.csv", sep=""), sep = ",")
#inpMatMvDet <- read.table(paste("N:\\Ned.Consultant\\VaR\\Hist\\20140815\\20140815_mat_mv_details.csv", sep=""), header = TRUE, sep = ",", stringsAsFactors=FALSE)
message("test2")

## set up matrix 'matCorr'
matCorr <- as.matrix(inpMatCorr[match(c(1),inpMatCorr[,2]):nrow(inpMatCorr), 2:ncol(inpMatCorr)])
class(matCorr) <- "numeric"
row.names(matCorr) <- seq(1,nrow(matCorr))
colnames(matCorr) <- seq(1,ncol(matCorr))

vecRic <- inpMatCorr[match(c(1),inpMatCorr[,2]):nrow(inpMatCorr),1]

## any cell in position file that is null is changed to 'MISSING'
inpMatMvDet[inpMatMvDet[, "Group"]    == "", "Group"]  <- "MISSING"
inpMatMvDet[inpMatMvDet[, "Grouping"] == "", "Grouping"]  <- "MISSING"
inpMatMvDet[inpMatMvDet[, "Location"] == "", "Location"]  <- "MISSING"

## only uses rows where Delta.US is not zero
matMvDet <- as.vector(inpMatMvDet)
matMvDet <- matMvDet[matMvDet[,"Delta.US."] != 0,]

## build vol vector and matrix
idxVol <- match(vecRic, matMvDet[, "Base.Usym"])
vecVol <- as.vector(matMvDet[idxVol, "VaR.Vol.Input"])/100 # 100 is percent
vecVol[is.na(vecVol)] <- 0
matVol <- as.matrix(Diagonal(n=length(vecVol), vecVol))
matVol[is.na(matVol)] <- 0

## build MV vector
inpMatMV <- aggregate(matMvDet[, c("Delta.US.", "Gamma.US.")], list(matMvDet[, "Base.Usym"]), sum)
idxMV <- match(vecRic,inpMatMV[, 1])
vecMV <- as.vector(t(inpMatMV[idxMV, "Delta.US."]))
vecMV[is.na(vecMV)] <- 0

## gamma components of VaR
vecGamma <- as.vector(matMvDet[idxVol, "Gamma.US."]) * 100 / 2
vecGamma[is.na(vecGamma)] <- 0
matGammaVol <- 2*((matVol/16)^4) #### 16 is sqrt(256 days)
matGammaVol[is.na(matVol)] <- 0

## covariance matrix
matCov <- matCorr %*% matVol
matCov <- t(matVol) %*% matCov

vecCovWgt <- matCov %*% vecMV
valWgtCovWgt <- (vecMV %*% vecCovWgt) 
GammaCovWgt <-  (t(vecGamma) %*% matGammaVol%*% vecGamma)


#### Calculate VaR for firm ####
FirmVaR <- sqrt(valWgtCovWgt + GammaCovWgt) * 2.326 / 16 #### 2.326 is 99% confidence value; 16 is sqrt(256 days)
FirmVaR

#### Calculate VaR by Group ####
iListGroup <- unique(matMvDet[, "Group"])

ResultVaRGroup <- data.frame(iListGroup, 0, 0, 0)
colnames(ResultVaRGroup) <- c("iListGroup", "VaRI", "VaRM", "VaR")

for (iGroup in 1:length(iListGroup)) {
  listGreeks <- matMvDet[, c("Base.Usym", "Group", "Delta.US.", "Delta.US.", "Delta.US.", "Gamma.US.", "Gamma.US.", "Gamma.US.")]
  colnames(listGreeks) <- c("Base.Usym", "Group", "DeltaVaRI", "DeltaVaRM",  "DeltaVaR", "GammaVaRI", "GammaVaRM", "GammaVaR")
  #setup for Marginal VaR calculation -- for each group, multiply position by .9999 
  listGreeks[listGreeks[, "Group"]==iListGroup[iGroup], c("GammaVaRM", "DeltaVaRM")] <- listGreeks[listGreeks[, "Group"]==iListGroup[iGroup], c("GammaVaRM", "DeltaVaRM")] * .9999
  
  #setup for Incremental VaR calculation -- for each group, multiply position by .9999 
  listGreeks[listGreeks[, "Group"]==iListGroup[iGroup], c("DeltaVaRI", "GammaVaRI")] <- 0
  listGreeks[listGreeks[, "Group"]!=iListGroup[iGroup], c("DeltaVaR", "GammaVaR")] <- 0
  
  ResultVaRGroup[iGroup,2:4] <- funcVaRCalc(vecRic, listGreeks, idxVol, matGammaVol, matCov, vecCovWgt, FirmVaR)
}


#### Calculate VaR by Grouping ####
iListGrouping <- unique(matMvDet[,"Grouping"])

ResultVaRGrouping <- data.frame(iListGrouping, 0, 0, 0)
colnames(ResultVaRGrouping) <- c("iListGroup", "VaRI", "VaRM", "VaR")

for (iGroup in 1:length(iListGrouping)) {
  
  listGreeks <- matMvDet[, c("Base.Usym", "Grouping", "Delta.US.", "Delta.US.", "Delta.US.", "Gamma.US.", "Gamma.US.", "Gamma.US.")]
  colnames(listGreeks) <- c("Base.Usym", "Grouping", "DeltaVaRI", "DeltaVaRM",  "DeltaVaR", "GammaVaRI", "GammaVaRM", "GammaVaR")
  
  listGreeks[listGreeks[, "Grouping"]==iListGrouping[iGroup], c("GammaVaRM", "DeltaVaRM")] <- listGreeks[listGreeks[, "Grouping"]==iListGrouping[iGroup], c("GammaVaRM", "DeltaVaRM")] * .9999
  
  listGreeks[listGreeks[, "Grouping"]==iListGrouping[iGroup], c("DeltaVaRI", "GammaVaRI")] <- 0
  listGreeks[listGreeks[, "Grouping"]!=iListGrouping[iGroup], c("DeltaVaR", "GammaVaR")] <- 0
  
  ResultVaRGrouping[iGroup,2:4] <- funcVaRCalc(vecRic, listGreeks, idxVol, matGammaVol, matCov, vecCovWgt, FirmVaR)
  
  if(iGroup%%25 < 1) {
    cat(iGroup)
  }
  
}


#### Calculate VaR by BaseUsym ####
iListBaseUsym <- unique(matMvDet[,"Base.Usym"])

ResultVaRBaseUsym <- data.frame(iListBaseUsym, 0, 0, 0)
colnames(ResultVaRBaseUsym) <- c("iListGroup", "VaRI", "VaRM", "VaR")

for (iGroup in 1:length(iListBaseUsym)) {
  
  listGreeks <- matMvDet[, c("Base.Usym", "Base.Usym", "Delta.US.", "Delta.US.", "Delta.US.", "Gamma.US.", "Gamma.US.", "Gamma.US.")]
  colnames(listGreeks) <- c("Base.Usym", "Base.Usym2", "DeltaVaRI", "DeltaVaRM",  "DeltaVaR", "GammaVaRI", "GammaVaRM", "GammaVaR")
  
  listGreeks[listGreeks[, "Base.Usym"]==iListBaseUsym[iGroup], c("GammaVaRM", "DeltaVaRM")] <- listGreeks[listGreeks[, "Base.Usym"]==iListBaseUsym[iGroup], c("GammaVaRM", "DeltaVaRM")] * .9999
  
  listGreeks[listGreeks[, "Base.Usym"]==iListBaseUsym[iGroup], c("DeltaVaRI", "GammaVaRI")] <- 0
  listGreeks[listGreeks[, "Base.Usym"]!=iListBaseUsym[iGroup], c("DeltaVaR", "GammaVaR")] <- 0
  
  ResultVaRBaseUsym[iGroup,2:4] <- funcVaRCalc(vecRic, listGreeks, idxVol, matGammaVol, matCov, vecCovWgt, FirmVaR)
  
  if(iGroup%%25 < 1) {
    cat(iGroup)
  }  
}


#### Calculate VaR by Location ####
iListLocation <- unique(matMvDet[,"Location"])

ResultVaRLocation <- data.frame(iListLocation, 0, 0, 0)
colnames(ResultVaRLocation) <- c("iListLocation", "VaRI", "VaRM", "VaR")

for (iGroup in 1:length(iListLocation))
{
  listGreeks <- matMvDet[, c("Base.Usym", "Location", "Delta.US.", "Delta.US.", "Delta.US.", "Gamma.US.", "Gamma.US.", "Gamma.US.")]
  colnames(listGreeks) <- c("Base.Usym", "Location", "DeltaVaRI", "DeltaVaRM",  "DeltaVaR", "GammaVaRI", "GammaVaRM", "GammaVaR")
  
  listGreeks[listGreeks[, "Location"]==iListLocation[iGroup], c("GammaVaRM", "DeltaVaRM")] <- listGreeks[listGreeks[, "Location"]==iListLocation[iGroup], c("GammaVaRM", "DeltaVaRM")] * .9999
  
  listGreeks[listGreeks[, "Location"]==iListLocation[iGroup], c("DeltaVaRI", "GammaVaRI")] <- 0
  listGreeks[listGreeks[, "Location"]!=iListLocation[iGroup], c("DeltaVaR", "GammaVaR")] <- 0
  
  
  ResultVaRLocation[iGroup,2:4] <- funcVaRCalc(vecRic, listGreeks, idxVol, matGammaVol, matCov, vecCovWgt, FirmVaR)
  
  if(iGroup%%25 < 1) {
    cat(iGroup)
  }
}

## sort objects
ResultVaRGrouping <- ResultVaRGrouping[order(ResultVaRGrouping[,4], decreasing = TRUE),]
ResultVaRGroup <- ResultVaRGroup[order(ResultVaRGroup[,4], decreasing = TRUE),]
ResultVaRBaseUsym <- ResultVaRBaseUsym[order(ResultVaRBaseUsym[,4], decreasing = TRUE),]
ResultVaRLocation <- ResultVaRLocation[order(ResultVaRLocation[,4], decreasing = TRUE),]

## export objects
write.table(FirmVaR,file=paste("N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_out_var_firm.csv",sep=""),sep=",", col.names = T, row.names = F, quote = FALSE)
write.table(ResultVaRGrouping,file=paste("N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_out_varm_grouping.csv",sep=""),sep=",", col.names = T, row.names = F, quote = FALSE)
write.table(ResultVaRGroup,file=paste("N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_out_varm_group.csv",sep=""),sep=",", col.names = T, row.names = F, quote = FALSE)
write.table(ResultVaRBaseUsym,file=paste("N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_out_varm_baseusym.csv",sep=""),sep=",", col.names = T, row.names = F, quote = FALSE)
write.table(ResultVaRLocation,file=paste("N:\\Ned.Consultant\\VaR\\Hist\\", args[2], "\\", args[2], "_mat_out_varm_location.csv",sep=""),sep=",", col.names = T, row.names = F, quote = FALSE)

#}  