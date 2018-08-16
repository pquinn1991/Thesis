#### EDA for data in Model1Data #####

#### Random exploratory graphs ###
library(ggplot2)
ggplot(data = electionsRaw, aes(PVI...100.to.100., INC_DW_NOM)) + geom_point(aes(color = as.factor(IncWin))) + xlab("Partisan Voter Index") + ylab("Incumbent Ideology Score") + scale_colour_discrete(name = "Incumbent Win?")
ggplot(data = electionsRaw, aes(INC_DW_NOM, IncPct)) + geom_point(aes(color = as.factor(IncWin))) + xlab("Incumbent Ideology Score") + ylab("Incumbent Vote Share") + scale_colour_discrete(name = "Incumbent Win?")
ggplot(data = electionsRaw, aes(abs(PVI...100.to.100.), IncPct)) + geom_point(aes(color = as.factor(IncWin))) + xlab("Partisan Voter Index") + ylab("Incumbent Vote Share") + scale_colour_discrete(name = "Incumbent Win?")
ggplot(data = electionsRaw, aes(PrevElectionIncumbParty., IncPct)) + geom_point(aes(color = as.factor(IncWin))) + xlab("Previous Election Incumbent Vote Share") + ylab("Incumbent Vote Share") + scale_colour_discrete(name = "Incumbent Win?")

levels(modelData$Winner)[levels(modelData$Winner)=="D"] <- "Democrat"
levels(modelData$Winner)[levels(modelData$Winner)=="R"] <- "Republican"
ggplot(data = modelData, aes(PVI...100.to.100., dPct2)) + geom_point(aes(color = Winner), size = 3, alpha = .2) + xlab("Partisan Voter Index") + ylab("Democrat Vote Share") + geom_hline(yintercept = .5, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") + ylim(0,1) + theme_bw() + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12, face = "bold"), legend.text = element_text(size=12),legend.title = element_text(size = 12, face = "bold"), legend.title.align = .5, legend.position = c(.2,.3), legend.background = element_rect(size=.5, linetype="solid", color = "black"))

ggplot(data = electionsRaw, aes(INC_DW_NOM, dPct2)) + geom_point(aes(color = Winner)) + xlab("Incumbent Ideology Score") + ylab("Democrat Vote Share") + geom_hline(yintercept = .5, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed")
ggplot(data = electionsRaw, aes(INC_DW_NOM*PVI...100.to.100., IncPct)) + geom_point(aes(color = IncWin)) + xlab("Incumbent Ideology Score * PVI") + ylab("Incumbent Vote Share") + geom_hline(yintercept = .5, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed")

ggplot(data = electionsRaw, aes(Dcont - Rcont, dPct)) + geom_point(aes(color = Winner)) + xlab("Dem Spending - Rep Spending") + ylab("Dem Vote Share") + geom_hline(yintercept = .5, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed")
ggplot(data = electionsRaw, aes(IncContDiff, IncPct)) + geom_point(aes(color = IncWin)) + xlab("Incumbent Spending Difference") + ylab("Incumbent Vote Share") + geom_hline(yintercept = .5, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") + geom_smooth(method = lm)

plot(electionsRaw$IncWin, electionsRaw$IncContDiff, ylim = c(-5000000,5000000), xlab = "Incumbent Win?", ylab = "Incumbent spending - challenger spending")

ggplot(data = electionsRaw, aes(x = INC_DW_NOM, fill = INC)) + geom_density(aes(group = INC), alpha = 0.5) + xlim(-1,1) + geom_vline(xintercept = 0, linetype = "dashed") + facet_grid(year~.)
ggplot(data = electionsRaw, aes(x = PVI...100.to.100., fill = Winner)) + geom_density(aes(group = Winner), alpha = 0.5) + geom_vline(xintercept = 0, linetype = "dashed")
ggplot(data = electionsRaw, aes(x = IncContDiff, fill = IncWin)) + geom_density(aes(group = IncWin), alpha = 0.5) + xlim(-5000000,5000000) + geom_vline(xintercept = 0, linetype = "dashed")
ggplot(data = electionsRaw, aes(x = dPct, fill = Winner)) + geom_density(aes(group = Winner), alpha = 0.5) + geom_vline(xintercept = 0, linetype = "dashed")
ggplot(data = electionsRaw, aes(x = Race.White, fill = Winner)) + geom_density(aes(group = Winner), alpha = 0.5) + geom_vline(xintercept = 0, linetype = "dashed")

### More exploratory graphs ###
ggplot(data = modelData, aes(abs(PVI...100.to.100.), IncPct)) + geom_point(aes(color = as.factor(IncWin))) + geom_smooth(method = "lm") + xlab("Abs(Partisan Voter Index)") + ylab("Incumbent Vote Share") + scale_colour_discrete(name = "Incumbent Win?")
ggplot(data = modelData, aes(PrevElectionIncumbParty., IncPct)) + geom_point(aes(color = as.factor(IncWin))) + geom_smooth(method = "lm") + xlab("Previous Election Incumbent Vote Share") + ylab("Incumbent Vote Share") + scale_colour_discrete(name = "Incumbent Win?")
ggplot(data = modelData, aes(abs(PVI...100.to.100.), PrevElectionIncumbParty.)) + geom_point(aes(color = as.factor(IncWin))) + geom_smooth(method = "lm") + xlab("Abs(Partisan Voter Index)") + ylab("Previous Election Incumbent Vote Share") + scale_colour_discrete(name = "Incumbent Win?")
ggplot(data = modelData, aes(PVIeffect, IncPct)) + geom_point(aes(color = as.factor(IncWin))) + geom_smooth(method = "lm") + xlab("PVI Effect") + ylab("Incumbent Vote Share") + scale_colour_discrete(name = "Incumbent Win?") + ylim(0,1)
ggplot(data = modelData, aes(Delta.unemployment.Rate, IncPct)) + geom_point(aes(color = as.factor(IncWin))) + geom_smooth(method = "lm") + xlab("Change in Unemployment Rate") + ylab("Incumbent Vote Share") + scale_colour_discrete(name = "Incumbent Win?")

plot(modelData$IncWin, modelData$logIncCont, xlab = "Incumbent Win?", ylab = "log(Incumbent Spending)")
plot(modelData$IncWin, modelData$PercentDelta.median.household.income, xlab = "Incumbent Win?", ylab = "% Change in Median Household Income")
plot(modelData$IncWin, modelData$Delta.unemployment.Rate, xlab = "Incumbent Win?", ylab = "Change in unemployment rate")

modelData$IncWin <- as.factor(modelData$IncWin)
levels(modelData$IncWin)[levels(modelData$IncWin)=="1"] <- "Incumbent Win"
levels(modelData$IncWin)[levels(modelData$IncWin)=="0"] <- "Incumbent Loss"
ggplot(data = modelData, aes(ideologyEffect, IncPct2)) + geom_point(aes(color = as.factor(IncWin)), size = 4, alpha = .25) + xlab("Ideology Effect") + ylab("Incumbent Vote Share") + scale_colour_discrete() + geom_hline(yintercept = .5, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") + theme_bw() + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12, face = "bold"), legend.text = element_text(size=12),legend.position = c(.7,.1), legend.title = element_blank(), legend.background = element_rect(size=.5, linetype="solid", color = "black"))

ggplot(data = modelData, aes(ideologyEffect*Repub, dPct2)) + geom_point(aes(color = as.factor(dWin)), size = 4, alpha = .25) + xlab("Ideology Effect") + ylab("Incumbent Vote Share") + scale_colour_discrete() + geom_hline(yintercept = .5, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") + theme_bw() + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12, face = "bold"), legend.text = element_text(size=12),legend.position = c(.7,.1), legend.title = element_blank(), legend.background = element_rect(size=.5, linetype="solid", color = "black"))


#################################################
############## EDA of dataset ###################
#################################################
summary(electionsRaw)
summary(modelDataAll)

# Notes
# NAs from PrevElectionIncumbParty, Unemployment Rate, and Delta Unemployment Rate are either:
# 1) district number is 0 when it should be 1 in the previous election results tab of Working Dataset
# 2) district didn't exist in previous election

# NAs from IncCont and logIncCont -- FEC campaign spending data missing 2012NY11 for some reason

# NAs from PrevPres -- district didn't exist in previous year

# NAs for ECI or PresApprov -- only have Gallup polls for certain years

# Registration NAs -- only have data for 2016

# Numeric variable histograms

numeric_vars <- c(data.frame(which(sapply(modelDataAll, is.numeric)))[,1])
par(mfrow = c(3,3))
for (i in numeric_vars[2:length(numeric_vars)]){
  hist(modelDataAll[,i], main = names(modelDataAll)[i], xlab = "", ylab = "")
}

# Correlation matrix
library(PerformanceAnalytics)
par(mfrow = c(1,1))
chart.Correlation(modelDataAll[,numeric_vars], histogram = TRUE)


tapply(modelDataAll$IncPct, modelDataAll$midtermEffect, mean)
# -1: Midterm Election, Candidate/Prez different parties, 0: Non-midterm election, 1: Midterm Election, Candidate/Prez same party
par(mfrow = c(1,1))
levels(modelDataAll$midtermEffect) <- c("Midterm election; \n incumbent and President \n opposite party", "On-cycle election", "Midterm election; \n incumbent and President \n same party")
ggplot(data = modelDataAll, aes(midtermEffect, IncPct2)) + geom_boxplot() + theme_bw() + ylab("Incumbent Vote Share") + theme(axis.title.x=element_blank(), axis.text = element_text(size = 14), axis.title = element_text(size = 14, face = "bold")) + scale_y_continuous(breaks=seq(0,1,.1))

ggplot(data = electionsRaw, aes(Dreg - Rreg, dPct2)) + geom_point(aes(color = Winner)) + xlab("Dem Registration Difference") + ylab("Dem Vote Share") + geom_hline(yintercept = .5, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") + geom_smooth(method = lm)
ggplot(data = electionsRaw, aes(IncReg - NIncReg, IncPct)) + geom_point(aes(color = IncWin)) + xlab("Inc Registration Difference") + ylab("Inc Vote Share") + geom_hline(yintercept = .5, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") + geom_smooth(method = lm)

ggplot(data = electionsRaw, aes(rPct2, dPct2)) + geom_point(aes(color = SabatoNum))

levels(modelData$Winner)[levels(modelData$Winner)=="D"] <- "Democrat"
levels(modelData$Winner)[levels(modelData$Winner)=="R"] <- "Republican"
modelData$Sabato <- factor(modelData$Sabato, levels = c("D", "Solid D", "Likely D", "Leans D", "D Toss-up", "R Toss-up", "Leans R", "Likely R", "Solid R", "R"))
ggplot(data = modelData, aes(runif(2706,0,1), dPct2)) + geom_point(aes(color = Winner), size = 4, alpha = .3) + facet_grid(.~as.factor(Sabato), switch = "both") + ylab("Democrat Vote Share") + xlab("Sabato Forecast") + theme_bw() + theme(strip.text.x = element_text(size = 12, face = "bold"), axis.text.x=element_blank(), axis.text.y=element_text(size = 12), axis.ticks.x=element_blank(), axis.title = element_text(size = 12, face = "bold"), legend.text = element_text(size=12),legend.position = c(.75,.75), legend.title = element_text(size = 12, face = "bold"), legend.title.align = 0.5, legend.background = element_rect(size=.5, linetype="solid", color = "black")) + scale_x_continuous(breaks = NULL) + geom_hline(aes(yintercept = .5), linetype = "dashed")

ggplot(data = electionsRaw, aes(as.factor(SabatoNum), dPct)) + geom_boxplot()
ggplot(data = electionsRaw, aes(as.factor(SabatoNum))) + geom_bar()

c(45,59,78,80,84,85)
par(mfrow = c(3,2))
hist(modelData[,85], main = "Incumbent 2-party vote share", xlab = "", ylab = "")
hist(modelData[,78], main = "Incumbent expenditures (log)", xlab = "", ylab = "", xlim = c(10,18), breaks = 32)
hist(modelData[,84], main = "Challenger expenditures (log)", xlab = "", ylab = "")
hist(modelData[,80], main = "PVI effect", xlab = "", ylab = "", breaks = 20)
hist(modelData[,59], main = "Change in unemployment rate", xlab = "", ylab = "", breaks = 32, xlim = c(-5,5))
hist(modelData[,45], main = "Years incumbent party control", xlab = "", ylab = "")

