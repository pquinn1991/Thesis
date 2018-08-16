#### Script to create the dataset used by first draft model ####
library(dplyr)
library(reshape)
## Set working directory
setwd("~/Documents/MSPA/590 - Thesis") # Home
setwd("~/MSPA/590 - Thesis") # Work

#### Read in dataset #######
electionsRaw <- read.csv("electionsNew2006_2018.csv", stringsAsFactors = TRUE, na.strings = c("#N/A", "NA"))

### Update Sabato Scores and Numbers, re-write data ###
## Last updated: 13 June 2018
sabatoUpdate <- data.frame(ID = c("2018SC1"), Sabato2 = c("Likely R"), SabatoNum2 = c(3))
electionsRaw <- left_join(electionsRaw, sabatoUpdate, by = "ID")
electionsRaw$Sabato[!is.na(electionsRaw$Sabato2)] <- electionsRaw$Sabato2[!is.na(electionsRaw$Sabato2)]
electionsRaw$SabatoNum[!is.na(electionsRaw$SabatoNum2)] <- electionsRaw$SabatoNum2[!is.na(electionsRaw$SabatoNum2)]
electionsRaw$Sabato2 <- NULL
electionsRaw$SabatoNum2 <- NULL
rm(sabatoUpdate)

### Refresh DW_NOMINATE?
nom_dat <- read.csv("https://voteview.com/static/data/out/members/HSall_members.csv")
nom_dat <- nom_dat[nom_dat$congress == 115 & nom_dat$chamber == "House",]
nom_dat$ID <- paste0("2018", nom_dat$state_abbrev, nom_dat$district_code)
nom_dat <- nom_dat[,c("ID", "nominate_dim1")]
electionsRaw <- left_join(electionsRaw, nom_dat, by = "ID")
electionsRaw$INC_DW_NOM[electionsRaw$year == "2018"] <- electionsRaw$nominate_dim1[electionsRaw$year == "2018"]
electionsRaw$nominate_dim1 <- NULL
rm(nom_dat)

### Refresh Spending
disb <- read.csv("https://classic.fec.gov/data/CandidateSummary.do?format=csv&election_yr=2018&can_nam=&can_off=&can_off_sta=&can_off_dis=&can_par_aff=&can_inc_cha_ope_sea=&tot_rec=&tot_dis=&cas_on_han_clo_of_per=&deb_owe_by_com=&cov_dat=&sortField=can_nam&sortOrder=0")

disb <- subset(disb, can_off == "H")
disb$can_off_dis[disb$can_off_dis == 0] <- 1
disb$ID <- paste0("2018", disb$can_off_sta, disb$can_off_dis)
disb$tot_dis <- sub('\\$','',as.character(disb$tot_dis))
disb$tot_dis <- as.numeric(sub('\\,','',as.character(disb$tot_dis)))
disb$tot_dis[is.na(disb$tot_dis)] <- 0
disb$can_par_aff <- as.character(disb$can_par_aff)
disb$can_par_aff[disb$can_par_aff != "DEM" & disb$can_par_aff != "REP"] <- "OTHER"

disbTable <- cast(disb, ID ~ can_par_aff, sum, value = 'tot_dis')

electionsRaw <- left_join(electionsRaw, disbTable, by = "ID")
electionsRaw$Dcont[electionsRaw$year == "2018"] <- electionsRaw$DEM[electionsRaw$year == "2018"]
electionsRaw$Rcont[electionsRaw$year == "2018"] <- electionsRaw$REP[electionsRaw$year == "2018"]
electionsRaw$Ocont[electionsRaw$year == "2018"] <- electionsRaw$OTHER[electionsRaw$year == "2018"]
electionsRaw$DEM <- NULL
electionsRaw$REP <- NULL
electionsRaw$OTHER <- NULL

rm(disb, disbTable)

#### Subset and create new variables ####
# Incumbent winner indicator
electionsRaw$IncWin <- 0
electionsRaw$IncWin[electionsRaw$INC == electionsRaw$Winner] <- 1
# Incumbent party registration
electionsRaw$IncReg <- 0
electionsRaw$IncReg[electionsRaw$INC == "D"] <- electionsRaw$Dreg[electionsRaw$INC == "D"]
electionsRaw$IncReg[electionsRaw$INC == "R"] <- electionsRaw$Rreg[electionsRaw$INC == "R"]
# Non-Incumbent party registration
electionsRaw$NIncReg <- 0
electionsRaw$NIncReg[electionsRaw$INC == "D"] <- electionsRaw$Rreg[electionsRaw$INC == "D"]
electionsRaw$NIncReg[electionsRaw$INC == "R"] <- electionsRaw$Dreg[electionsRaw$INC == "R"]
# Registration difference
electionsRaw$RegDiff <- electionsRaw$IncReg - electionsRaw$NIncReg
# Existence of third party candidate indicator
electionsRaw$thirdParty <- 0
electionsRaw$thirdParty[electionsRaw$oPct > .01] <- 1
# Create third party effect variable
electionsRaw$thirdPartyEffect <- electionsRaw$Oreg*electionsRaw$thirdParty
# Re-code Pres_Incumbent_SameParty
electionsRaw$Pres_Incumbent_SameParty[electionsRaw$Pres_Incumbent_SameParty == 1] <- 1
electionsRaw$Pres_Incumbent_SameParty[electionsRaw$Pres_Incumbent_SameParty == 0] <- -1
# Create midterm effect variable
electionsRaw$midtermEffect <- electionsRaw$Midterm. * electionsRaw$Pres_Incumbent_SameParty
# Create Ideology Effect variable
electionsRaw$ideologyEffect <- electionsRaw$PVI...100.to.100. * electionsRaw$INC_DW_NOM
# Incumbent campaign spending variable
electionsRaw$IncCont <- 0
electionsRaw$NincCont <- 0
electionsRaw$IncCont[electionsRaw$INC == "R"] <- electionsRaw$Rcont[electionsRaw$INC == "R"]
electionsRaw$NincCont[electionsRaw$INC == "R"] <- electionsRaw$Dcont[electionsRaw$INC == "R"]
electionsRaw$IncCont[electionsRaw$INC == "D"] <- electionsRaw$Dcont[electionsRaw$INC == "D"]
electionsRaw$NincCont[electionsRaw$INC == "D"] <- electionsRaw$Rcont[electionsRaw$INC == "D"]
electionsRaw$IncContDiff <- log(electionsRaw$IncCont + 1) - log(electionsRaw$NincCont + 1)
electionsRaw$dContDiff <- electionsRaw$Dcont - electionsRaw$Rcont
# Log incumbent spending variable
electionsRaw$logIncCont <- log(electionsRaw$IncCont + 1)
# Republican indicator
electionsRaw$Repub <- 1
electionsRaw$Repub[electionsRaw$INC == "D"] <- -1
# PVI effect variable
electionsRaw$PVIeffect <- electionsRaw$PVI...100.to.100. * electionsRaw$Repub
# Two-party vote shares
electionsRaw$dPct2 <- electionsRaw$D/(electionsRaw$D + electionsRaw$R)
electionsRaw$rPct2 <- electionsRaw$R/(electionsRaw$D + electionsRaw$R)
# Dem win?
electionsRaw$dWin <- 0
electionsRaw$dWin[electionsRaw$dPct2 > 0.5] <- 1

### Change variable classes for EDA (e.g. change year to factor instead of integer) ###
electionsRaw$year <- as.factor(electionsRaw$year)
electionsRaw$district <- as.factor(electionsRaw$district)
electionsRaw$IncWin <- as.factor(electionsRaw$IncWin)
electionsRaw$thirdParty <- as.factor(electionsRaw$thirdParty)
electionsRaw$Midterm. <- as.factor(electionsRaw$Midterm.)
electionsRaw$Pres_Incumbent_SameParty <- as.factor(electionsRaw$Pres_Incumbent_SameParty)
electionsRaw$midtermEffect <- as.factor(electionsRaw$midtermEffect)
electionsRaw$Winner <- relevel(electionsRaw$Winner, "R")
electionsRaw$INC <- relevel(electionsRaw$INC, "R")
#Oreg has a lot of 1s because districts that do not require/collect party registration for voters are given 1 under Oreg
electionsRaw[which(electionsRaw$Oreg == 1),"Oreg"] <- NA
#Get rid of "O" factor for INC
electionsRaw$INC <- factor(electionsRaw$INC)
# Log NincCont
electionsRaw$logNincCont <- log(electionsRaw$NincCont + 1)
# Inc 2-party share
electionsRaw$IncPct2 <- electionsRaw$D/(electionsRaw$D + electionsRaw$R)
electionsRaw$IncPct2[electionsRaw$INC == "R"] <- electionsRaw$R[electionsRaw$INC == "R"]/(electionsRaw$D[electionsRaw$INC == "R"] + electionsRaw$R[electionsRaw$INC == "R"])
# Sabato
electionsRaw$SabatoScore <- electionsRaw$SabatoNum * electionsRaw$Repub
#Challenged
electionsRaw$Challenge[electionsRaw$year %in% c("2006", "2008", "2010", "2012", "2014", "2016")] <- 1
electionsRaw$Challenge[electionsRaw$year %in% c("2006", "2008", "2010", "2012", "2014", "2016") & (electionsRaw$dPct2 == 1 | electionsRaw$dPct2 == 0)] <- 0


### Polls, Open, and Challenge ###
library(googlesheets)
library(dplyr)

dk <- gs_title("Daily Kos Elections House open seat tracker")

open <- gs_read(ss = dk, ws = "2018 - open seats", skip = 1)
open <- filter(open, Clinton %in% c(as.character(seq(0,100,1))))
open$Dist2 <- gsub("AL","1", substr(open$District,4,6))
open$Dist2[substr(open$Dist2,1,1) == "0"] <- substr(open$Dist2[substr(open$Dist2,1,1) == "0"], 2,2)
open$state <- substr(open$District,1,2)
open$ID <- paste0("2018", open$state, open$Dist2)
open$Open2 <- 1
open <- open[,12:13]
electionsRaw <- left_join(electionsRaw, open, by = "ID")
electionsRaw$Open2[is.na(electionsRaw$Open2)] <- 0
electionsRaw$Open[electionsRaw$year == "2018"] <- electionsRaw$Open2[electionsRaw$year == "2018"]
electionsRaw$Open2 <- NULL

unchallenged <- gs_read(ss = dk, ws = "2018 - uncontested seats", skip = 1)
unchallenged <- filter(unchallenged, Clinton %in% c(as.character(seq(0,100,1))))
unchallenged$Dist2 <- gsub("AL","1", substr(unchallenged$District,4,6))
unchallenged$Dist2[substr(unchallenged$Dist2,1,1) == "0"] <- substr(unchallenged$Dist2[substr(unchallenged$Dist2,1,1) == "0"], 2,2)
unchallenged$state <- substr(unchallenged$District,1,2)
unchallenged$ID <- paste0("2018", unchallenged$state, unchallenged$Dist2)
unchallenged$Challenge2 <- 0
unchallenged <- unchallenged[,10:11]
electionsRaw <- left_join(electionsRaw, unchallenged, by = "ID")
electionsRaw$Challenge2[is.na(electionsRaw$Challenge2)] <- 1
electionsRaw$Challenge[electionsRaw$year == "2018"] <- electionsRaw$Challenge2[electionsRaw$year == "2018"]
electionsRaw$Challenge2 <- NULL

pollYear <- data.frame(year = c("2006", "2008", "2010", "2012", "2014", "2016", "2018"), Polls = c(7.9,10.7,-6.8,1.2,-5.7,-1.1,8.2), dSwing = c(10.5,2.8,-17.4,8,-6.9,4.6,9.3))
polls <- read.csv("https://projects.fivethirtyeight.com/generic-ballot-data/generic_topline.csv")
pollYear$Polls[7] <- round(polls$dem_estimate[3] - polls$rep_estimate[3], digits = 1)
pollYear$dSwing[7] <- pollYear$Polls[7] - pollYear$Polls[6]
electionsRaw <- left_join(electionsRaw, pollYear, by = "year")

rm(open, polls, pollYear, unchallenged)

## Other modeling variables ##
modelData <- electionsRaw
modelData$PrevElectionD2 <- modelData$PrevElectionD./(modelData$PrevElectionD. + modelData$PrevElectionR.)
modelData$Prev2ElectionD2 <- modelData$PrevElection2D./(modelData$PrevElection2D. + modelData$PrevElection2R.)
modelData$Prev2ElectionD2Diff <- (modelData$PrevElection2D./(modelData$PrevElection2D. + modelData$PrevElection2R.)) -.5
modelData$OpenInc <- "Open"
modelData$OpenInc[modelData$Open == 0] <- as.character(modelData$INC[modelData$Open == 0])
modelData$midPres <- "On-cycle"
modelData$midPres[modelData$Midterm. == 1] <- as.character(modelData$PresParty[modelData$Midterm. == 1])
modelData$PrevElectionD2Diff <- modelData$PrevElectionD2 - .5
modelData$PrevElectionD2DiffSwing <- modelData$PrevElectionD2Diff - (modelData$Prev2ElectionD2 - .5)

modelData$Dcont[is.na(modelData$Dcont)] <- 0
modelData$Rcont[is.na(modelData$Rcont)] <- 0

modelData$logDCont <- log(modelData$Dcont + 1)
modelData$logRCont <- log(modelData$Rcont + 1)
modelData$logDContDiff <- modelData$logDCont - modelData$logRCont

#modelData$logDContDiff <- 0
#modelData$logDContDiff[modelData$Dcont >= modelData$Rcont] <- log(modelData$Dcont[modelData$Dcont >= modelData$Rcont] - modelData$Rcont[modelData$Dcont >= modelData$Rcont] + 1)
#modelData$logDContDiff[modelData$Rcont >= modelData$Dcont] <- log(modelData$Rcont[modelData$Rcont >= modelData$Dcont] - modelData$Dcont[modelData$Rcont >= modelData$Dcont] + 1)*-1

modelData$OpenInc <- as.factor(modelData$OpenInc)
modelData$midPres <- as.factor(modelData$midPres)

modelData$ideologyEffect2 <- modelData$ideologyEffect*modelData$Repub

modelData$crossPressure <- 0
modelData$crossPressure[sign(modelData$INC_DW_NOM) != sign(modelData$PVI2) & modelData$INC == "D"] <- -1
modelData$crossPressure[sign(modelData$INC_DW_NOM) != sign(modelData$PVI2) & modelData$INC == "R"] <- 1

## Final model data before subsetting to challenged becomes exploratory dataset
explore <- modelData
# Subset model data
modelData <- subset(modelData, Challenge == 1)


### Fix exploratory dataset
colnames(explore)
explore <- subset(explore, select = c("year", "state", "district"))

### filters out certain races for modeling ###

#modelData <- electionsRaw
#modelData <- rbind(subset(electionsRaw, IncPct2 < 1 & year != "2018"), subset(electionsRaw, year == "2018"))
#modelData <- subset(electionsRaw, SabatoNum < 5 & SabatoNum > -5)
#modelData <- modelData[-which(modelData$SabatoNum == 5 & modelData$Winner == "D" | modelData$SabatoNum == -5 & modelData$Winner == "R"),]

#extraElections <- rbind(subset(electionsRaw, IncPct2 == 1),electionsRaw[which(electionsRaw$SabatoNum == 5 & electionsRaw$Winner == "D" | electionsRaw$SabatoNum == -5 & electionsRaw$Winner == "R"),])

#### Create "modeling" dataset, which only includes certain variables #####
#modelVariables <- c("IncPct2", "IncWin", "IncReg", "INC",
#                    "PrevElectionIncumbParty.", "thirdParty",
#                    "PercentDelta.median.household.income", 
#                    "Delta.unemployment.Rate", "midtermEffect", 
#                    "logIncCont", "PVIeffect", "IncContDiff", "RegDiff", "year", "SabatoScore", "Open"
#)

#colnames(modelData)
#modelData2 <- modelData[,-c(2:11,13, 15:19,26:30,64:65,67:69,70:71,81:83)]

#modelDataAll <- modelData[,modelVariables]

#modelDataNoReg <- modelData[,modelVariables[-which(modelVariables == "IncReg" | modelVariables == "RegDiff")]]

#modelDataReg <- modelData[,modelVariables[-which(modelVariables == "midtermEffect")]]
#modelDataReg <- subset(modelDataReg, IncReg > 0)

