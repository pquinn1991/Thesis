#### Script to create the dataset used by first draft model ####
## Set working directory
setwd("~/Documents/MSPA/590 - Thesis") # Home
setwd("~/MSPA/590 - Thesis") # Work

#### Read in dataset #######
electionsRaw <- read.csv("elections2.csv", stringsAsFactors = TRUE, na.strings = "#N/A")

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
electionsRaw$Pres_Incumbent_SameParty[electionsRaw$Pres_Incumbent_SameParty == 0] <- 0
# Re-code Pres_NIncumbent_SameParty
electionsRaw$Pres_NIncumbent_SameParty[electionsRaw$Pres_Incumbent_SameParty == 1] <- 0
electionsRaw$Pres_NIncumbent_SameParty[electionsRaw$Pres_Incumbent_SameParty == 0] <- 1
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
electionsRaw$NIncContDiff <- log(electionsRaw$NincCont + 1) - log(electionsRaw$IncCont + 1)
electionsRaw$dContDiff <- electionsRaw$Dcont - electionsRaw$Rcont
# Log incumbent spending variable
electionsRaw$logIncCont <- log(electionsRaw$IncCont + 1)
# Log dem spending variable
electionsRaw$logDcont <- log(electionsRaw$Dcont + 1)
# Log rep spending variable
electionsRaw$logRcont <- log(electionsRaw$Rcont + 1)
# Log Other spending variable
electionsRaw$logOcont <- log(electionsRaw$Ocont + 1)
# Republican indicator
electionsRaw$Repub <- 1
electionsRaw$Repub[electionsRaw$INC == "D"] <- -1
# Republican indicator 2
electionsRaw$Repub2 <- 1
electionsRaw$Repub2[electionsRaw$INC == "D"] <- 0
# Democrat indicator
electionsRaw$Democrat <- 1
electionsRaw$Democrat[electionsRaw$INC == "R"] <- 0
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
# NINC_DW_NOM
electionsRaw$NINC_DW_NOM <- 0
#absIdeology
electionsRaw$absIdeology <- abs(electionsRaw$INC_DW_NOM)


#### Create "modeling" dataset, which only includes certain variables #####
BLPmodelVariables <- c("dPct2", "Dreg", "logDcont",
                       "Rreg", "logRcont", "logOcont", "Dcont", "Rcont", "Democrat", "absIdeology", "Repub2",
                       "Oreg",
                       "state", "district", "INC", "PVI...100.to.100.", "Pres_Incumbent_SameParty", 
                       "PrevElectionR.", "PrevElectionD.", 
                       "Age.25.to.44", "Age.65.and.over", "Delta.unemployment.Rate", "Median.household.income",
                       "PercentDelta.median.household.income", "IncWin", "ideologyEffect",
                       "dContDiff", "PVIeffect", "dWin"
)


BLPdata <- electionsRaw[,BLPmodelVariables]
#write.csv(electionsRaw, "electionsRaw.csv", row.names = FALSE)
BLPdata <- na.omit(BLPdata)

### Exclude non-competitive elections?
BLPdata <- subset(BLPdata, dPct2 < 1)
BLPdata <- subset(BLPdata, dPct2 > 0)

#BLPdata <- subset(BLPdata, Dcont > 1000)
#BLPdata <- subset(BLPdata, Rcont > 1000)

### Include constants?
## What is beta_S in vote empirics paper?

### Third party candidate in denominator?
### Log spending?
