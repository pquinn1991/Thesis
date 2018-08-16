library(nlstools)

#BLPdata <- read.csv("BLPdata.csv")

# Define the formula from (Vote Empirics paper)
formulaBLP <- as.formula(dPct2 ~ (exp(Dreg*(Democrat*bDincumbent + logDcont*bDcont + c0) + Oreg*(Democrat*bDincumbent + logDcont*bDcont + c0) + Rreg*(Democrat*bDincumbent + logDcont*bDcont + c0)))/
                           (1 + 
                              (exp(Dreg*(Democrat*bDincumbent + logDcont*bDcont + c0) + Oreg*(Democrat*bDincumbent + logDcont*bDcont + c0) + Rreg*(Democrat*bDincumbent + logDcont*bDcont + c0))) + 
                              (exp(Dreg*(Repub2*bRincumbent + logRcont*bRcont + c1) + Oreg*(Repub2*bRincumbent + logRcont*bRcont + c1) + Rreg*(Repub2*bRincumbent + logRcont*bRcont + c1)))
                           ))

# Use preview to check if starting values are close enough
preview(formulaBLP, data = BLPdata, start = list(bDincumbent = 1.629, 
                                                 bDcont = 0.0458, 
                                                 bRincumbent = -6, 
                                                 bRcont = 0.3555, 
                                                 c0 = -1, 
                                                 c1 = -4.229))

# Build model
nlsBLP <- nls(formulaBLP, start = list(bDincumbent = 1.65, 
                                       bDcont = 0.042, 
                                       bRincumbent = 0, 
                                       bRcont = 0.16, 
                                       c0 = -.8, 
                                       c1 = -1.5), 
              upper = c(6, 
                           1, 
                           6, 
                           1, 
                           10, 
                           10),
              lower = c(-6, 
                           -1, 
                           -6, 
                           -1, 
                           -10, 
                           -10),
              algorithm = "port", trace = TRUE, data = BLPdata)

### Check the model results with plots and summary output ###
# Fitted versus actual values
plot(nlsBLP$m$fitted(), nlsBLP$m$fitted() + nlsBLP$m$resid(), xlab = "Predicted Dem. Vote Pct", ylab = "Actual Dem. Vote Pct", main = "Predicted versus Actual")
abline(0,1)
# Mean squared error
mean(((nlsBLP$m$fitted() + nlsBLP$m$resid())-nlsBLP$m$fitted())^2)
# Histogram of errors
hist(nlsBLP$m$fitted() - (nlsBLP$m$fitted()  + nlsBLP$m$resid()), breaks = 10, xlab = "Predicted Dem. Vote Pct - Actual Dem. Vote Pct", main = "Histogram of Errors")
# Table of predictions versus actual results
table(nlsBLP$m$fitted() >= 0.5, nlsBLP$m$fitted() + nlsBLP$m$resid() >= 0.5)
# Total sum of squares
sum((BLPdata$dPct - mean(BLPdata$dPct))^2) #TSS
# Model results
overview(nlsBLP)

# Goodness of fit
nlsBLPres <- nlsResiduals(nlsBLP)
plot(nlsBLPres)
test.nlsResiduals(nlsBLPres)

# Add fitted values to original data
fitted <- as.numeric(nlsBLP$m$fitted())
BLPdata <- cbind(BLPdata, fitted)


## Create counterfactual data
library(dplyr)
# Create "counterfactuals" dataframe
# Add column for "marginal fitted" -- change in predicted vote share for a one-unit increase in log of Spending
# Column for goal of log of Spending -- how much spending needed for 50-50 split in vote
# Column for winning amount of log of Spending -- how much additional spending needed for 50-50 split in vote
cf <- BLPdata %>% 
  mutate(marginalDFitted = ((exp(Dreg*(Democrat*1.629669 + (logDcont+1)*.045785 + -1.003600) + Oreg*(Democrat*1.629669 + (logDcont+1)*.045785 + -1.003600) + Rreg*(Democrat*1.629669 + (logDcont+1)*.045785 + -1.003600)))/
                                        (1 + 
                                           (exp(Dreg*(Democrat*1.629669 + (logDcont+1)*.045785 + -1.003600) + Oreg*(Democrat*1.629669 + (logDcont+1)*.045785 + -1.003600) + Rreg*(Democrat*1.629669 + (logDcont+1)*.045785 + -1.003600))) + 
                                           (exp(Dreg*(Repub2*-6 + logRcont*.355507 + -4.229785) + Oreg*(Repub2*-6 + logRcont*.355507 + -4.229785) + Rreg*(Repub2*-6 + logRcont*.355507 + -4.229785)))
                                        )) - fitted) %>% 
  mutate(goalLogDCont = logDcont + ((.5 - dPct2)/marginalDFitted)) %>%
  mutate(winDContDiff = exp(goalLogDCont) - 1 - Dcont)

# Fix some errors (if the "goal" spending amount is negative, say the candidate should've spent no money)
#                 (if the goal amount and actual result dont agree, put NA. Happens when we say the candidate needed to spend more to win, but they won in reality, or the opposite)
cf$winDContDiff[cf$goalLogDCont <= 0] <- cf$Dcont[cf$goalLogDCont <= 0] * -1
cf$winDContDiff[exp(cf$goalLogDCont)-1-cf$Dcont < 0 & cf$dPct2 <= .5] <- NA
cf$winDContDiff[exp(cf$goalLogDCont)-1-cf$Dcont > 0 & cf$dPct2 >= .5] <- NA

# Calcualte percent of spending difference to win or lose
cf$winDContDiffPct <- cf$winDContDiff/cf$Dcont

# Do the same as above, but for the Republican candidate
cf <- cf %>% 
  mutate(marginalRFitted = ((exp(Dreg*(Democrat*1.629669 + (logDcont)*.045785 + -1.003600) + Oreg*(Democrat*1.629669 + (logDcont)*.045785 + -1.003600) + Rreg*(Democrat*1.629669 + (logDcont)*.045785 + -1.003600)))/
                              (1 + 
                                 (exp(Dreg*(Democrat*1.629669 + (logDcont)*.045785 + -1.003600) + Oreg*(Democrat*1.629669 + (logDcont)*.045785 + -1.003600) + Rreg*(Democrat*1.629669 + (logDcont)*.045785 + -1.003600))) + 
                                 (exp(Dreg*(Repub2*-6 + (logRcont+1)*.355507 + -4.229785) + Oreg*(Repub2*-6 + (logRcont+1)*.355507 + -4.229785) + Rreg*(Repub2*-6 + (logRcont+1)*.355507 + -4.229785)))
                              )) - fitted) %>% 
  mutate(goalLogRCont = logRcont + ((.5 - dPct2)/marginalRFitted)) %>%
  mutate(winRContDiff = exp(goalLogRCont) - 1 - Rcont)

cf$winRContDiff[cf$goalLogRCont <= 0] <- cf$Rcont[cf$goalLogRCont <= 0] * -1
cf$winRContDiff[exp(cf$goalLogRCont)-1-cf$Rcont < 0 & cf$dPct2 >= .5] <- NA
cf$winRContDiff[exp(cf$goalLogRCont)-1-cf$Rcont > 0 & cf$dPct2 <= .5] <- NA

cf$winRContDiffPct <- cf$winRContDiff/cf$Rcont


###############################
### Explore counterfactuals ###
###############################
library(ggplot2)

# Histograms
ggplot(data = cf, aes(x = winDContDiff)) + geom_density() + xlim(-10000000,10000000)
ggplot(data = cf, aes(x = winDContDiff, fill = as.factor(dWin))) + geom_histogram(aes(group = as.factor(dWin))) + xlim(-15000000,15000000)

ggplot(data = cf, aes(x = winRContDiff)) + geom_density() + xlim(-10000000,10000000)
ggplot(data = cf, aes(x = winRContDiff, fill = as.factor(dWin))) + geom_histogram(aes(group = as.factor(dWin))) + xlim(-15000000,15000000)

ggplot(data = cf, aes(x = winDContDiffPct)) + geom_histogram() + xlim(-1.1,2)
ggplot(data = cf, aes(x = winRContDiffPct)) + geom_histogram() + xlim(-1.1,2)

# How much more/less should each candidate have spent?
ggplot(data = cf, aes(x = Dcont, y = winDContDiffPct)) + geom_point(aes(color = as.factor(dWin))) + ylim(-1,2) + xlim(0,10000000)
ggplot(data = cf, aes(x = Dcont, y = winDContDiff)) + geom_point(aes(color = as.factor(dWin))) + ylim(-5000000,5000000) + xlim(0,10000000)
ggplot(data = cf, aes(x = dPct2, y = winDContDiffPct)) + geom_point(aes(color = as.factor(dWin), size = Dcont)) + ylim(-1,2)
ggplot(data = cf, aes(x = dPct2, y = winDContDiff)) + geom_point(aes(color = as.factor(dWin), size = Dcont)) + ylim(-5000000,5000000)


ggplot(data = cf, aes(x = Rcont, y = winRContDiffPct)) + geom_point(aes(color = as.factor(dWin))) + ylim(-1,10) + xlim(0,20000000)
ggplot(data = cf, aes(x = Rcont, y = winRContDiff)) + geom_point(aes(color = as.factor(dWin))) + ylim(-10000000,10000000) + xlim(0,20000000)
ggplot(data = cf, aes(x = dPct2, y = winRContDiffPct)) + geom_point(aes(color = as.factor(dWin), size = Rcont)) + ylim(-1,2)
ggplot(data = cf, aes(x = dPct2, y = winRContDiff)) + geom_point(aes(color = as.factor(dWin), size = Rcont)) + ylim(-10000000,10000000)


# Which are the most valuable "next dollars"
ggplot(data = cf, aes(x = marginalDFitted)) + geom_density()
ggplot(data = cf, aes(x = marginalRFitted)) + geom_density()

ggplot(data = cf, aes(x = dPct2, y = marginalDFitted)) + geom_point(aes(color = as.factor(dWin), size = Dcont))
ggplot(data = cf, aes(x = Dcont, y = Rcont)) + geom_point(aes(color = as.factor(dWin), size = marginalDFitted))
ggplot(data = cf, aes(x = Dcont, y = Rcont)) + geom_point(aes(color = as.factor(dWin), size = winDContDiffPct)) + xlim(0,15000000) + ylim(0,15000000)
ggplot(data = cf, aes(x = Dcont, y = Rcont)) + geom_point(aes(color = as.factor(dWin), size = winRContDiffPct)) + xlim(0,15000000) + ylim(0,15000000)


ggplot(data = cf, aes(x = winDContDiff, y = marginalDFitted)) + geom_point(aes(color = as.factor(dWin), size = Dcont)) + xlim(-20000000,20000000) + ylim(0,.011)
ggplot(data = cf, aes(x = winRContDiff, y = marginalRFitted)) + geom_point(aes(color = as.factor(dWin), size = Rcont)) + xlim(-20000000,20000000)


# What were the outcomes of the 2016 elections not included in this model?
table(electionsRaw$dWin[is.na(electionsRaw$Dreg) | electionsRaw$dPct2 %in% c(1,0)], electionsRaw$year[is.na(electionsRaw$Dreg) | electionsRaw$dPct2 %in% c(1,0)])


### Code for testing initial model parameters ###
### Can ignore unless initial estimates are not sufficiently close for nlstools package ###
library(nls2)
nlsBLP1 <- nls2(formulaBLP, start = expand.grid(bDincumbent = seq(0,3,.5), 
                                                bDcont = seq(-.1,.3,.1), 
                                                bRincumbent = seq(-3,0,.5), 
                                                bRcont = seq(-.5,.3,.1), 
                                                c0 = seq(-1,1,.5), 
                                                c1 = seq(-1,1,.5), 
                data = BLPdata, algorithm = "brute-force"))
overview(nlsBLP1)
