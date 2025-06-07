setwd("C:/Users/Pauline/Dropbox/Studie/Stage 3/Experiment/Data")

install.packages("ggpubr")
install.packages("ggplot2")
install.packages("quickpsy")
#install.packages("factoextra")
install.packages("onls")
#install.packages("nlstools")
#library(factoextra)
library(data.table)
library(dplyr)
library(quickpsy)
library(ggplot2)
library(ggpubr)
library(onls)
#library(nlstools)

# Load & store data-----
files = list.files(pattern="*.csv")
Initials <- gsub("*.csv$", "", files)
DataList = lapply(setNames(files, make.names(Initials)), fread)

# Add collumn with name of observer
DataList <- mapply(cbind, DataList, "Participant" = Initials, SIMPLIFY = F)

# Combine all dataframes into one
AllData <- do.call(rbind, DataList)

# Pre-processing ----- 
# 1. change answers from 190 and 188, to 1 and 0
AllData$key_press <- (AllData$key_press - 190) / -2

# 2. arrange data by stimulus
DataArrByStim <- arrange(AllData,stimulus)

# 3. convert stimuli on right side to correspond to question: left larger? i.e. swap answers
DataArrByStim$key_press <- ifelse(grepl("R", DataArrByStim$stimulus) == TRUE,
                                  1 - DataArrByStim$key_press, DataArrByStim$key_press)

# 4. Add seperate collumns for size, stimulus type, screen side & direct or indirect comparison
DataArrByStim$Stimulustype <- 
  as.factor(with(DataArrByStim,ifelse(grepl("V", stimulus) == TRUE,"Soccer Ball",
                                      ifelse(grepl("T", stimulus) == TRUE,"Tennis Ball",
                                             ifelse(grepl("N", stimulus) == TRUE,"Neutral Reference", "10 euro cent coin")))))

DataArrByStim$Size <- as.numeric(gsub("^.*L|^.*R|(\\.png$)","",DataArrByStim$stimulus)) # size
DataArrByStim$Size <- DataArrByStim$Size - 100

DataArrByStim$ScreenSide <- 
  ifelse (grepl("L", DataArrByStim$stimulus) == TRUE, "L", "R")
DataArrByStim$ScreenSide <- as.factor(DataArrByStim$ScreenSide)

DataArrByStim$Direct <- as.factor(with(DataArrByStim, ifelse(grepl("Coins",stimulus) == TRUE, "Direct",
                                                             ifelse(grepl("Direct", stimulus) == TRUE, "Direct", "Indirect"))))

DataArrByStim$Participant <- as.factor(DataArrByStim$Participant)


# 5. remove some unnecessary collumns
CleanDataArr <- subset(DataArrByStim, select = -c(stimulus, test_part,trial_type, 
                                                  correct_response, trial_index,
                                                  time_elapsed, internal_node_id, correct) )
summary(CleanDataArr)

# Quickpsy-----
# 6. group per participant to check std's for possible exclusions
DatPerP <- group_by_at(CleanDataArr, vars(Participant, Size))
AveragesPerP <- summarise(DatPerP, n=n(), nYes = sum(key_press), nNo = n - nYes, p = nYes/n)

fitPerP <- quickpsy(AveragesPerP, Size, nYes, n, grouping = .(Participant))
plotpar(fitPerP) + facet_wrap(vars(Participant))
guilty <- fitPerP$par[which(fitPerP$par$par > 20 | fitPerP$par$par < -20),]
guilty

# remove participant RA (37.6), MB (58), FS (p2 = 25) & SB (p2 = 217) and check plot again
DatPerP <- DatPerP[!(DatPerP$Participant == "RA" | DatPerP$Participant == "MB" |
                       DatPerP$Participant == "FS" | DatPerP$Participant == "FS" |
                       DatPerP$Participant == "SB"),]
AveragesPerP2 <- summarise(DatPerP, n=n(), nYes = sum(key_press), nNo = n - nYes, p = nYes/n)
fitPerP2 <- quickpsy(AveragesPerP2, Size, nYes, n, grouping = .(Participant))
plot(fitPerP2)
plotpar(fitPerP2)

rm(guilty)
# 7. groeperen per conditie & persoon
DatPerC <- group_by_at(DatPerP, vars(Stimulustype, Direct), .add = TRUE)
Averagespp <- summarise(DatPerC, n=n(), nYes = sum(key_press), nNo = n - nYes, p = nYes/n)
View(Averagespp)
summary(DatPerC)

fitpp <- quickpsy(Averagespp, Size, nYes, n, grouping = .(Stimulustype, Direct, Participant),
                  xmin = -20, xmax = 15)

plot(fitpp)
plotpar(fitpp, color = Participant)
View(fitpp$par)

guilty2 <- fitpc$par[which(fitpp$par$par > 10 | fitpc$par$par < -10),]
guilty2
# turns out RS has a very large std for the 10 cent stimulus (par = 41.8   prinf = 14.8
# parup = 68841232.), delete participant? Same for PF with the tennis stimulus
# (par = 22.4, pinf = 1.00e+1, parsup = 27372480.), and for JOK for soccer stimuli (p1 = -41. with parinf
# -2.067272e+08	) Participant TK: gemiddelden voet en tennis rond -20, ci's tot -80

DatPerC <- DatPerC[!(DatPerC$Participant == "RS" | DatPerC$Participant == "PF" |
                       DatPerC$Participant == "JOK"),]
rm(guilty2)
Averagespc <- summarise(DatPerC, n=n(), nYes = sum(key_press), nNo = n - nYes, p = nYes/n)
Averagespc <- droplevels(Averagespc)
fitpc <- quickpsy(Averagespc, Size, nYes, n, grouping = .(Participant, Direct, Stimulustype),
                  xmin = -20, xmax = 15)

ppc <- plot(fitpc)
ppc
ppc + theme_classic() + theme(legend.position = "none")

View(fitpc$thresholds)
plthre <- plotthresholds(fitpc, x = Stimulustype, color = Participant, xpanel = Direct) +
  theme_classic()
plthre

plotpar(fitpc, x = Stimulustype, ypanel = Direct, color = Participant) +
  theme_classic() + theme(legend.position = "none")

# # 8. Gemiddelde
# AllDatByContrast <- ungroup(DatPerC, Participant)
# AllAverages <- summarise(AllDatByContrast, n=n(), nYes = sum(key_press), nNo = n - nYes, p = nYes/n)
# summary(AllDatByContrast)
# 
# fit <- quickpsy(AllAverages, Size, nYes, n, grouping = .(Stimulustype, Direct),
#                 xmin = -20, xmax = 15)
# View(fit$par)
# 
# pall <- plot(fit) + theme_classic() + 
#   scale_color_brewer(type = "qual", palette = "Set1")
# pall + ggtitle("average among participants")
# #pall +   geom_segment(data = Avp1p2[which(Avp1p2[,1] == "p1"),], 
#  #                     aes(x = par, y = 0, xend = par, yend = .5)) 
# 
# plotthresholds(fit)
# plotpar(fit)
# (fit$par)

# # compare averages of fitpc$par with fit$par
# parpc <- group_by_at(fitpc$par, vars(parn, Stimulustype, Direct))
# Avp1p2 <- summarise_at(parpc, vars(-Participant), funs(mean(.)))
# View(Avp1p2)
# (diffp1toRef <- Avp1p2$par[1:4])
# plotpar(fitpc)
# Avp1p2fit <- rbind(Avp1p2, fit$par)
# 
# DiffFitAndFitpp <- aggregate(Avp1p2fit[, 4:6], list(Stimulustype = Avp1p2fit$Stimulustype, 
#                                                     parn = Avp1p2fit$parn,
#                                                     Direct = Avp1p2fit$Direct), diff)


# pOne----
# voorbeeld pp: KB, JB of IV zijn duidelijkst
OneParticipant <- list()
OneParticipant$curves <- data.frame(filter(fitpc$curves, Participant == "IV"))
OneParticipant$thresholds <- filter(fitpc$thresholds, Participant == "IV")
# correct coin: was small stimulus compared to big ref -> switch around to 
#compare to large soccer stimulus to small tennis reference
OneParticipant$curves$x[1:300] <- OneParticipant$curves$x[1:300] + 
  (2 *abs(OneParticipant$thresholds$thre[1]))
OneParticipant$thresholds$thre[1] <- OneParticipant$thresholds$thre[1] *-1
TempInf <- OneParticipant$thresholds$thresup[1] *-1
OneParticipant$thresholds$thresup[1] <- OneParticipant$thresholds$threinf[1] *-1 
OneParticipant$thresholds$threinf[1] <- TempInf
OneParticipant$Averages <- filter(Averagespc, Participant == "IV")

StimCol <- c("#E41A1C", "#984EA3", "#377EB8", "#4DAF4A")
pOne <- ggplot(data = OneParticipant$curves, x = x, y = y) + 
  geom_point(data = OneParticipant$Averages, 
             aes(x = Size, y = p, color = Direct:Stimulustype)) +
  geom_line(aes(x = x, y = y, color = Direct:Stimulustype)) +
  facet_wrap(vars(Direct, Stimulustype), drop = TRUE) +
  geom_segment(data = OneParticipant$thresholds, 
               aes(x = thre, y = -.1, xend = thre, yend = prob,
                   col = Direct:Stimulustype)) +
  geom_errorbarh(data = OneParticipant$thresholds, 
                 aes(xmin = threinf, xmax = thresup, y = .5, color = Direct:Stimulustype,
                    height = .05)) +
  geom_vline(xintercept = 0, lty = 2, col = "grey") +
  coord_cartesian(xlim = c(-15,10), ylim = c(0,1)) +
  labs(title = "Psychometric Curves of One Participant", 
       x = "\nSize Difference (%)", y = "Probability\n",
       color = "") +
  scale_color_manual(labels = c("Coins", "Balls", "Soccer", "Tennis"),
                                values = StimCol) +
  theme_classic() +
  theme(strip.text = element_blank(),
        strip.background = element_blank(), axis.title = element_text(size = 18), legend.title = element_text(size = 14),
        plot.title = element_text(size = 24), axis.text = element_text(size = 16),
        legend.text = element_text(size = 18)) +
     annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
pOne

# Extra plot presentation----
ggplot(data = OneParticipant$curves, x = x, y = y) + 
  geom_point(data = OneParticipant$Averages, 
             aes(x = Size, y = p, color = Direct:Stimulustype), size = 1.5) +
  geom_line(aes(x = x, y = y, color = Direct:Stimulustype), lwd = 1.5) +
  facet_wrap(vars(Direct, Stimulustype), drop = TRUE) +
  coord_cartesian(xlim = c(-15,10), ylim = c(0,1)) +
  labs(x = "\nSize Difference (%)", y = "Probability\n",
       color = "") +
  scale_color_manual(labels = c("Coins", "Balls", "Soccer", "Tennis"),
                     values = StimCol) +
  geom_segment(data = OneParticipant$thresholds, 
               aes(x = thre, y = -.1, xend = thre, yend = prob,
                   col = Direct:Stimulustype), lwd = 1) +
  geom_errorbarh(data = OneParticipant$thresholds, 
                 aes(xmin = threinf, xmax = thresup, y = .5, color = Direct:Stimulustype,
                     height = .05), lwd = 1) +
  geom_vline(xintercept = 0, lty = 2, col = "grey", lwd = 1) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
  theme_classic() +
  theme(strip.text = element_blank(),
        strip.background = element_blank(), axis.title = element_text(size = 32),
        plot.title = element_text(size = 40), axis.text = element_text(size = 26),
        legend.text = element_text(size = 28))


# prepare Data frames----
# get PSE and std's in seperate data frame, add age, volunteer and gender
Age <- as.numeric(c(22, 68, 24,45, 17, 20, 28, 33, 19, 21, 34, 58, 20,
                    34, 72, 17, 70, 28, 32, 24, 24, 23, 18, 24, 26, 61,
                    24, 22, 20, 24, 19, 21, 47, 40, 37, 37, 53, 34, 18, 25, 
                    30, 18, 34, 61, 30, 27, 25, 24, 29, 25, 23, 21,
                    60, 34, 23, 18, 19, 18, 21, 50, 73, 46, 25))
Volunteer <- as.logical(c("FALSE", "TRUE", "TRUE", "TRUE", "TRUE", "FALSE",
                          "TRUE", "TRUE", "FALSE", "TRUE", "TRUE", "TRUE", 
                          "FALSE", "FALSE", "TRUE", "TRUE", "TRUE","TRUE", 
                          "TRUE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE",
                          "TRUE", "TRUE", "FALSE", "FALSE", "FALSE", "FALSE", 
                          "FALSE", "FALSE", "TRUE", "TRUE", "TRUE", "TRUE", 
                          "TRUE", "TRUE", "FALSE", "TRUE", "TRUE", "FALSE", 
                          "TRUE", "TRUE", "FALSE", "TRUE", "TRUE", "FALSE", 
                          "TRUE", "FALSE","FALSE", "FALSE", "TRUE", "TRUE",
                          "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "TRUE",
                          "TRUE", "FALSE", "TRUE"))

Gender <- as.factor(c("f", "m", "f", "f", "f", "m", "f", "f", "m", "m", "m",
                      "m", "f", "m", "m", "f", "m", "f", "f", "m", "f", "f", 
                      "m", "m", "f", "f", "m", "m", "f", "m", "f", "m", "f",
                      "f", "f", "m", "m", "f", "f", "f", "m", "f", "m", "m", 
                      "f", "f", "m", "m", "f", "f", "f", "m", "f", "m", "m", 
                      "f", "f", "m", "f", "m", "m", "f", "m"))

TennisPSE <- filter(fitpc$thresholds, Stimulustype == "Tennis Ball")
SoccerDPSE <- filter(fitpc$thresholds, Stimulustype == "Soccer Ball" & Direct == "Direct")
SoccerIPSE <- filter(fitpc$thresholds, Stimulustype == "Soccer Ball" & Direct == "Indirect")
CoinPSE <- filter(fitpc$thresholds, Stimulustype == "10 euro cent coin")
CoinPSE$thre <- CoinPSE$thre * -1   # correct for comparing small stim to large ref, instead of large stim to small ref (balls)
TempInf <- CoinPSE$thresup * -1
CoinPSE$thresup <- CoinPSE$threinf * -1
CoinPSE$threinf <- TempInf
rm(TempInf)

(median(TennisPSE$Tennis.thre))
(median(SoccerDPSE$SoccerD.thre))
(median(SoccerIPSE$SoccerI.thre))
(median(CoinPSE$Coin.thre))
SoccerDPSE$Stimulustype <- as.factor("Balls")
SoccerDPSE <- cbind(SoccerDPSE, Age, Volunteer, Gender)
SoccerIPSE$Stimulustype <- as.factor("Soccer")
SoccerIPSE <- cbind(SoccerIPSE, Age, Volunteer, Gender)
CoinPSE$Stimulustype <- as.factor("Coins")
CoinPSE <- cbind(CoinPSE, Age, Volunteer, Gender)
TennisPSE$Stimulustype <- as.factor("Tennis")
TennisPSE <- cbind(TennisPSE, Age, Volunteer, Gender)
AllPSEs1 <- rbind(CoinPSE, SoccerDPSE, SoccerIPSE, TennisPSE)

lijst <- list(TennisPSE, SoccerDPSE, SoccerIPSE)
lijst <- lapply(lijst, function(x) x[!(names(x) %in% c("Age", "Participant", "Volunteer", "Gender", "prob"))])
names(lijst) <- c("TennisPSE", "SoccerDPSE", "SoccerIPSE")
list2env(lijst, .GlobalEnv)
rm(lijst)
colnames(TennisPSE) <- paste("Tennis", colnames(TennisPSE), sep = ".")
colnames(SoccerDPSE) <- paste("SoccerD", colnames(SoccerDPSE), sep = ".")
colnames(SoccerIPSE) <- paste("SoccerI", colnames(SoccerIPSE), sep = ".")
colnames(CoinPSE) <- paste("Coin", colnames(CoinPSE), sep = ".")

AllPSEs <- cbind(CoinPSE, SoccerDPSE, SoccerIPSE, TennisPSE)
AllPSEs <- rename(AllPSEs, Participant = Coin.Participant, prob = Coin.prob,
                  Age = Coin.Age, Volunteer = Coin.Volunteer, Gender = Coin.Gender)

TennisSTD <- filter(fitpc$par, 
                    Stimulustype == "Tennis Ball" & parn == "p2")
SoccerISTD <- filter(fitpc$par, 
                     Stimulustype == "Soccer Ball" & parn == "p2" & Direct == "Indirect")
SoccerDSTD <- filter(fitpc$par, 
                     Stimulustype == "Soccer Ball" & parn == "p2" & Direct == "Direct")
CoinSTD <- filter(fitpc$par, 
                  Stimulustype == "10 euro cent coin" & parn == "p2")

(median(TennisSTD$Tennis.par))
(median(SoccerDSTD$SoccerD.par))
(median(SoccerISTD$SoccerI.par))
(median(CoinSTD$Coin.par))

AllSTDs <- rbind(CoinSTD, SoccerDSTD, SoccerISTD,TennisSTD)
AllSTDs <- subset(AllSTDs, select =  -c(Stimulustype, parn))
AllSTDs <- cbind(AllSTDs, AllPSEs1[c("Stimulustype", "Age", "Gender", "Volunteer")],)

AvSTD <- aggregate(AllSTDs$par, list(Participant = AllSTDs$Participant), mean)
AvSTD <- rename(AvSTD, p2 = x)

lijst <- list(CoinSTD, TennisSTD, SoccerDSTD, SoccerISTD)
lijst <- lapply(lijst, function(x) x[!(names(x) %in% c("Participant", "parn"))])
names(lijst) <- c("CoinSTD", "TennisSTD", "SoccerDSTD", "SoccerISTD")
list2env(lijst, .GlobalEnv)
rm(lijst)

TennisSTD$Stimulustype <- as.factor("Tennis")
SoccerDPSE$Stimulustype <- as.factor("Balls")
SoccerIPSE$Stimulustype <- as.factor("Soccer")
CoinPSE$Stimulustype <- as.factor("Coins")

colnames(TennisSTD) <- paste("Tennis", colnames(TennisSTD), sep = ".")
colnames(SoccerDSTD) <- paste("SoccerD", colnames(SoccerDSTD), sep = ".")
colnames(SoccerISTD) <- paste("SoccerI", colnames(SoccerISTD), sep = ".")
colnames(CoinSTD) <- paste("Coin", colnames(CoinSTD), sep = ".")

AllSTDs2 <- cbind(CoinSTD, SoccerDSTD, SoccerISTD, TennisSTD, 
                  AllPSEs[, c(1, 8:10)])
AllSTDs2$p2 <- AvSTD$p2
AllPSEs$p2 <- AvSTD$p2
rm(AvSTD)
rm(NewAv)

TennisPSE$Tennis.par <- TennisSTD$Tennis.par
SoccerDPSE$SoccerD.par <- SoccerDSTD$SoccerD.par
SoccerIPSE$SoccerI.par <- SoccerISTD$SoccerI.par
CoinPSE$Coin.par <- CoinSTD$Coin.par

# compute effect size direct & indirect comparisons 
# ( = diff tennis & soccer or diff soccer & 100% ref)
EffectIndirect <- SoccerIPSE$SoccerI.thre - TennisPSE$Tennis.thre
EffectDirect <- SoccerDPSE$SoccerD.thre
Effect <- data.frame(Direct = EffectDirect, Indirect = EffectIndirect,
                     SoccerD.thresup = SoccerDPSE$SoccerD.thresup,
                     SoccerD.threinf = SoccerDPSE$SoccerD.threinf,
                     SoccerI.thresup = EffectIndirect + (SoccerIPSE$SoccerI.thresup - SoccerIPSE$SoccerI.thre),
                     SoccerI.threinf  = EffectIndirect + (SoccerIPSE$SoccerI.threinf - SoccerIPSE$SoccerI.thre),
                     Participant = AllSTDs2$Participant, 
                     p2 = AvSTD$p2)
(median(EffectIndirect))
(median(EffectDirect))
LengthCIEffect <- Effect$SoccerI.thresup - Effect$SoccerI.threinf
LengthCISoccerD <- AllPSEs$SoccerD.thresup - AllPSEs$SoccerD.threinf

# Statistics - check distributions----

# check normal distribution of PSE's
qqnorm(AllPSEs$Tennis.thre)
shapiro.test(AllPSEs$Tennis.thre) # p < .01, 
qqnorm(AllPSEs$SoccerD.thre)
shapiro.test(AllPSEs$SoccerD.thre) # p = .14, 
qqnorm(AllPSEs$SoccerI.thre)
shapiro.test(AllPSEs$SoccerI.thre) # p < .01
qqnorm(AllPSEs$Coin.thre)
shapiro.test(AllPSEs$Coin.thre) # p = .04

# And of the effect sizes
qqnorm(EffectIndirect)
shapiro.test(EffectIndirect) # p = .42
qqnorm(EffectDirect)
shapiro.test(EffectDirect) # p = 0.14

qqnorm(AllSTDs2$SoccerD.par)
shapiro.test(AllSTDs2$SoccerD.par) # p < .01
qqnorm(AllSTDs2$SoccerI.par)
shapiro.test(AllSTDs2$SoccerI.par) # p < .01
qqnorm(AllSTDs2$Tennis.par)
shapiro.test(AllSTDs2$Tennis.par) # p < .01
qqnorm(AllSTDs2$Coin.par)
shapiro.test(AllSTDs2$Coin.par) # p < .01

# TEST 1: wilcoxon's signed rank test for direct/ indirect trials
(TestID <- wilcox.test(EffectDirect, EffectIndirect, paired = TRUE))
    # p-value < .57 -> no effect of comparison method on effect size
# post hoc power analysis
#mean(c(TennisPSE$thre, SoccerIPSE$thre))
#mean(SoccerDPSE$thre)
#mean(c(SoccerISTD$par, TennisSTD$par))
#mean(SoccerDSTD$par)
# power = 1?

# Orthogonal Model Effect Comparison ---- 

WeightFactor <- sqrt(LengthCIEffect) + sqrt(LengthCISoccerD)
# orthogonal regression
modmeth <- onls(Indirect ~ a + b * Direct, data = Effect,
                start = list(a = -1, b = 1), window = 22,
                weights = c(1/WeightFactor))
modmeth
check_o(modmeth)
# obtain ci's
CImeth <- confint(modmeth, method = "boot")
#get intercept given the slope and mean of data
Intercept2.5 <- mean(EffectIndirect) - CImeth[2,1] * mean(EffectDirect)
Intercept97.5 <- mean(EffectIndirect) - CImeth[2,2] * mean(EffectDirect)
xCI <- seq(from = -13, to = 4, length.out = 63)
yCImin <- Intercept2.5 + (CImeth[2, 1] * xCI)
yCImax <- Intercept97.5 + (CImeth[2,2] * xCI)
pmodmeth <- ggscatter(Effect, y = "Indirect", x = "Direct",
                     group = "p2", col = "p2",
                     title = "Illusion Effect of Two Different Comparison Methods",
                     xlab = "Direct (%)", ylab = "Indirect (%)") +
  coord_fixed(1, xlim =c(-12, 2), ylim = c(-12, 2)) +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  geom_ribbon(aes(x = xCI, ymin = yCImin, ymax = yCImax, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = modmeth$model$a, slope = modmeth$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  scale_color_gradient(low =  "blue", high = "#E41A1C") + 
  theme(legend.position = "right", legend.key.width = unit(0.2, "cm"), 
        axis.title = element_text(size = 25), legend.title = element_text(size = 20),
        plot.title = element_text(size = 30, hjust = 0.5), axis.text = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  labs(col = "") +
  geom_errorbar(aes(ymin = SoccerI.threinf , ymax = SoccerI.thresup, col = p2,
                    width = .2)) + 
  geom_errorbarh(aes(xmin = SoccerD.threinf, xmax = SoccerD.thresup, col = p2,
                     height = .2))
pmodmeth <- pmodmeth +
  geom_errorbar(data = subset(Effect, Participant == "IV"),
             aes(ymin = SoccerI.threinf , ymax = SoccerI.thresup,
             width = .2)) + 
  geom_errorbarh(data = subset(Effect, Participant == "IV"),
             aes(xmin = SoccerD.threinf, xmax = SoccerD.thresup,
             height = .2)) + 
  geom_point(data = subset(Effect, Participant == "IV"), 
             aes(y = Indirect, x = Direct), 
             shape = 16, color = "green", size = 4, fill = "green")
pmodmeth

  # b. same on std's of soccer direct/ soccer indirect
  (TestSTDID <- wilcox.test(SoccerISTD$SoccerI.par, SoccerDSTD$SoccerD.par, paired = TRUE))
# p = 0.48 (dus vergelijkingsmethode heeft geen invloed op onze precisie) 
sd(SoccerDSTD$par)
sd(SoccerISTD$par)
mean(SoccerDSTD$par)
mean(SoccerISTD$par)
# Power = .99
  
# Statistics and boxplot----
# TEST 2: wilcoxon's signed rank test for difference 10 cent and tennis to reference
  (TestTennis <- wilcox.test(TennisPSE$Tennis.thre, mu = 0, 
                             alternative = "greater")) #p < .01 Power = .54
#mean(TennisPSE$thre)
#mean(TennisSTD$par)
  (TestCoin <- wilcox.test(CoinPSE$Coin.thre, mu = 0, alternative = "greater")) 
#p < 0.001. Power = .99
#mean(CoinPSE$thre)
#mean(CoinSTD$par)
# tennis ball and 10 cent coin perceived as having different size than reference

# Difference between volunteers in tennis and soccer condition
median(AllPSEs1[AllPSEs1$Stimulustype == "Tennis" & AllPSEs1$Volunteer == "TRUE",4])
median(AllPSEs1[AllPSEs1$Stimulustype == "Tennis" & AllPSEs1$Volunteer == "FALSE",4])
wilcox.test(AllPSEs1[AllPSEs1$Stimulustype == "Tennis" & AllPSEs1$Volunteer == "TRUE",4], mu = 0)
wilcox.test(AllPSEs1[AllPSEs1$Stimulustype == "Tennis" & AllPSEs1$Volunteer == "FALSE",4], mu = 0)

median(AllPSEs1[AllPSEs1$Stimulustype == "Soccer" & AllPSEs1$Volunteer == "TRUE", 4])
median(AllPSEs1[AllPSEs1$Stimulustype == "Soccer" & AllPSEs1$Volunteer == "FALSE", 4])
wilcox.test(AllPSEs1[AllPSEs1$Stimulustype == "Soccer" & AllPSEs1$Volunteer == "TRUE",4], mu = 0)
wilcox.test(AllPSEs1[AllPSEs1$Stimulustype == "Soccer" & AllPSEs1$Volunteer == "FALSE",4], mu = 0)

# difference between volunteers and students in std's
median(sapply(AllSTDs[AllSTDs$Volunteer == "TRUE", 3], as.numeric))
median(sapply(AllSTDs[AllSTDs$Volunteer == "FALSE", 3], as.numeric))

  # boxplot
AllPSEs1$p2 <- AvSTD$p2

plotx2 <- ggplot(data = AllPSEs1, aes(x = Stimulustype, y = thre,
                                      fill = Stimulustype)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(pch = 21, position = position_jitterdodge(), size = 2) +
  geom_hline(yintercept = 0, col = "grey", lty = 2, lwd = 1) +
  labs(title = "Bias of each Participant per Condition", 
       x = "", y = "Size Difference (%)",
       color = "") +
  scale_fill_manual(labels = c("Coins", "Balls", "Soccer", "Tennis"),
                     values = StimCol) +
  theme_classic() + theme(legend.position = "none", axis.title.y = element_text(size = 20), 
                          plot.title = element_text(size = 24, hjust = .5), 
                          axis.text = element_text(size = 20))

plotx2 + geom_point(data = subset(AllPSEs1, Participant == "IV"), aes(x = Stimulustype,
                                                                      y = thre, col = Participant), size = 2) +
  scale_color_manual(values = "green")
plotx2

plots <- ggplot(data = AllSTDs, aes(x = Stimulustype, y = par,
                                      fill = Stimulustype)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(pch = 21, position = position_jitterdodge(), size = 2) +
  labs( 
       x = "", y = "Size Difference (%)",
       color = "") +
  scale_fill_manual(labels = c("Coins", "Balls", "Soccer", "Tennis"),
                    values = StimCol) +
  geom_point(data = subset(AllSTDs, Participant == "IV"), aes(x = Stimulustype,
                                                                        y = par, col = Participant), size = 2) +
  scale_color_manual(values = "green") +
  theme_classic() + theme(legend.position = "none",
                          axis.title.y = element_text(size = 28),
                          plot.title = element_text(size = 30, hjust = .5), 
                          axis.text = element_text(size = 28))

plots

# Orthogonal Linear Distance Regression of Biases----
LengthCICoin <- AllPSEs$Coin.thresup - AllPSEs$Coin.threinf
LengthCITennis <- AllPSEs$Tennis.thresup - AllPSEs$Tennis.threinf
LengthCISoccerD <- AllPSEs$SoccerD.thresup - AllPSEs$SoccerD.threinf
LengthCISoccerI <- AllPSEs$SoccerI.thresup - AllPSEs$SoccerI.threinf

LengthCICoinstd <- AllSTDs2$Coin.parsup - AllSTDs2$Coin.parinf
LengthCITennisstd <- AllSTDs2$Tennis.parsup - AllSTDs2$Tennis.parinf
LengthCISoccerDstd <- AllSTDs2$SoccerD.parsup - AllSTDs2$SoccerD.parinf
LengthCISoccerIstd <- AllSTDs2$SoccerI.parsup - AllSTDs2$SoccerI.parinf

WeightFactor1 <- sqrt(LengthCISoccerI) + sqrt(LengthCITennis)
modts <- onls(SoccerI.thre ~ a + b * Tennis.thre, data = AllPSEs,
             start = list(a = -4, b = 1), window = 25, 
             weights = c(1/WeightFactor1))
modts
#check_o(modts)
#qqnorm(residuals(modts))
#plot(AllPSEs$Tennis.thre, predict(modts))
#laarsts <- nlsBoot(modts, niter = 1000) #a(-14.83, -3.67) & b(1.23, 8.18)
CIparts <- confint(modts, method = "boot")
Intercept2.5ts <- mean(AllPSEs$SoccerI.thre) - CIparts[2,1] * mean(AllPSEs$Tennis.thre)
Intercept97.5ts <- mean(AllPSEs$SoccerI.thre) - CIparts[2,2] * mean(AllPSEs$Tennis.thre)
xCIts <- seq(from = -23, to = 8, length.out = 63)
yCImints <- Intercept2.5ts + (CIparts[2,1] * xCIts)
yCImaxts <- Intercept97.5ts + (CIparts[2,2] * xCIts)

WeightFactor2 <- sqrt(LengthCICoin) + sqrt(LengthCITennis)
modtc <- onls(Coin.thre ~ a + b * Tennis.thre, data = AllPSEs,
              start = list(a = 1, b = 0),
              weights = c(1/WeightFactor2))
modtc
check_o(modtc)
qqnorm(residuals(modtc))

CIpartc <- confint(modtc, method = "boot")
Intercept2.5tc <- mean(AllPSEs$Coin.thre) - CIpartc[2,1] * mean(AllPSEs$Tennis.thre)
Intercept97.5tc <- mean(AllPSEs$Coin.thre) - CIpartc[2,2] * mean(AllPSEs$Tennis.thre)
yCImintc <- Intercept2.5tc + (CIpartc[2,1] * xCIts)
yCImaxtc <- Intercept97.5tc + (CIpartc[2,2] * xCIts)

WeightFactor3 <- sqrt(LengthCISoccerD) + sqrt(LengthCICoin)
modsc <- onls(Coin.thre ~ a + b * SoccerD.thre, data = AllPSEs,
              start = list(a = 2, b = 0), 
              weights = c(1/WeightFactor3))
modsc
check_o(modsc)
qqnorm(residuals(modsc))

overview(modsc) # a(.21, 4.29), b(-.30, .51)
CIparsc <- confint(modsc, method = "boot")
Intercept2.5sc <- mean(AllPSEs$Coin.thre) - CIparsc[2,1] * mean(AllPSEs$SoccerD.thre)
Intercept97.5sc <- mean(AllPSEs$Coin.thre) - CIparsc[2,2] * mean(AllPSEs$SoccerD.thre)
yCIminsc <- Intercept2.5sc + (CIparsc[2,1] * xCIts)
yCImaxsc <- Intercept97.5sc + (CIparsc[2,2] * xCIts)

WeightFactor4 <- sqrt(LengthCISoccerD) + sqrt(LengthCISoccerI)
modss <- onls(SoccerD.thre ~ a + b * SoccerI.thre, data = AllPSEs,
              start = list(a = 6, b = 2), window = 31,
              weights = c(1/WeightFactor4))
modss
check_o(modss)
qqnorm(residuals(modss))

#overview(modss) #a(-5.10, -1.02), b(-0.05, 0.92)
CIparss <- confint(modss, method = "boot")
Intercept2.5ss <- mean(AllPSEs$SoccerD.thre) - CIparss[2,1] * mean(AllPSEs$SoccerI.thre)
Intercept97.5ss <- mean(AllPSEs$SoccerD.thre) - CIparss[2,2] * mean(AllPSEs$SoccerI.thre)
yCIminss <- Intercept2.5ss + (CIparss[2,1] * xCIts)
yCImaxss <- Intercept97.5ss + (CIparss[2,2] * xCIts)

WeightFactor5 <- sqrt(LengthCISoccerD) + sqrt(LengthCITennis)
modtsd <- onls(SoccerD.thre ~ a + b * Tennis.thre, data = AllPSEs,
              start = list(a = -3, b = -1), 
              weights = c(1/WeightFactor5))
modtsd
check_o(modtsd)
qqnorm(residuals(modtsd))

#overview(modtsd) #a(-5.96, -3.77), b(-.17, .57)
CIpartsd <- confint(modtsd, method = "boot")
Intercept2.5tsd <- mean(AllPSEs$SoccerD.thre) - CIpartsd[2,1] * mean(AllPSEs$Tennis.thre)
Intercept97.5tsd <- mean(AllPSEs$SoccerD.thre) - CIpartsd[2,2] * mean(AllPSEs$Tennis.thre)
yCImintsd <- Intercept2.5tsd + (CIpartsd[2,1] * xCIts)
yCImaxtsd <- Intercept97.5tsd + (CIpartsd[2,2] * xCIts)

WeightFactor6 <- sqrt(LengthCISoccerI) + sqrt(LengthCICoin)
modscI <- onls(Coin.thre ~ a + b * SoccerI.thre, data = AllPSEs,
              start = list(a = 2, b = 1), 
              weights = c(1/WeightFactor6))
modscI
check_o(modscI)
qqnorm(residuals(modscI))

#overview(modscI) #a(.83, 3.5), b(-.2, .42)
CIparscI <- confint(modscI, method = "boot")
Intercept2.5scI <- mean(AllPSEs$Coin.thre) - CIparscI[2,1] * mean(AllPSEs$SoccerI.thre)
Intercept97.5scI <- mean(AllPSEs$Coin.thre) - CIparscI[2,2] * mean(AllPSEs$SoccerI.thre)
yCIminscI <- Intercept2.5scI + (CIparscI[2,1] * xCIts)
yCImaxscI <- Intercept97.5scI + (CIparscI[2,2] * xCIts)

# Plots Ortho Biases----
pmodts <- ggscatter(AllPSEs, y = "SoccerI.thre", x = "Tennis.thre", group = "p2",
                   col = "p2",
                   ylab = "Soccer", xlab = "Tennis") +
  coord_fixed(1, xlim = c(-21, 6), ylim = c(-21, 6)) +
  geom_abline(aes(intercept = modts$model$a, slope = modts$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCIts, ymin = yCImints, ymax = yCImaxts, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") + 
  geom_errorbar(aes(ymin = SoccerI.threinf, ymax = SoccerI.thresup, col = p2, width = .2)) +
  geom_errorbarh(aes(xmin = Tennis.threinf, xmax = Tennis.thresup, col = p2, height = .2)) +
  theme(legend.position = "right", legend.title = element_blank(), 
      legend.key.width = unit(0.2, "cm"), legend.text = element_text(size = 12))
legm <- get_legend(pmodts)
pmodts <- pmodts + theme(legend.position = "none", axis.title = element_text(size = 16))
pmodts <- pmodts +
  geom_errorbar(data = subset(AllPSEs, Participant == "IV"),
                aes(ymin = SoccerI.threinf , ymax = SoccerI.thresup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllPSEs, Participant == "IV"),
                 aes(xmin = Tennis.threinf, xmax = Tennis.thresup,
                     height = .6)) + 
  geom_point(data = subset(AllPSEs, Participant == "IV"), 
             aes(y = SoccerI.thre, x = Tennis.thre), 
             shape = 16, color = "green", size = 2, fill = "green") 
pmodts

pmodtc <- ggscatter(AllPSEs, x = "Tennis.thre", y = "Coin.thre", group = "p2",
                    col = "p2",
                    xlab = "Tennis", ylab = "Coins") +
  coord_fixed(1, ylim = c(-21, 6), xlim = c(-21, 6)) +
  geom_abline(aes(intercept = modtc$model$a, slope = modtc$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCIts, ymin = yCImintc, ymax = yCImaxtc, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") + 
  geom_errorbar(aes(ymin = Coin.threinf, ymax = Coin.thresup, col = p2, width = .2)) +
  geom_errorbarh(aes(xmin = Tennis.threinf, xmax = Tennis.thresup, col = p2, height = .2)) +
  theme(legend.position = "none", axis.title = element_text(size = 16))
pmodtc <- pmodtc +
  geom_errorbar(data = subset(AllPSEs, Participant == "IV"),
                aes(ymin = Coin.threinf , ymax = Coin.thresup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllPSEs, Participant == "IV"),
                 aes(xmin = Tennis.threinf, xmax = Tennis.thresup,
                     height = .6)) +
  geom_point(data = subset(AllPSEs, Participant == "IV"), 
             aes(y = Coin.thre, x = Tennis.thre), 
             shape = 16, color = "green", size = 2, fill = "green")
pmodtc

pmodsc <- ggscatter(AllPSEs, 
                    x = "SoccerD.thre", y = "Coin.thre", group = "p2",
                    col = "p2", 
                    xlab = "Balls", ylab = "Coins") +
  coord_fixed(1, ylim = c(-20, 7), xlim = c(-20, 7)) + 
  geom_abline(aes(intercept = modsc$model$a, slope = modsc$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCIts, ymin = yCIminsc, ymax = yCImaxsc, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") +
  geom_errorbar(aes(ymin = Coin.threinf, ymax = Coin.thresup, col = p2,
                    width = .2)) + 
  geom_errorbarh(aes(xmin = SoccerD.threinf, xmax = SoccerD.thresup, col = p2,
                     height = .2)) +
  theme(legend.position = "none", axis.title = element_text(size = 16))
pmodsc <- pmodsc +
  geom_errorbar(data = subset(AllPSEs, Participant == "IV"),
                aes(ymin = Coin.threinf , ymax = Coin.thresup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllPSEs, Participant == "IV"),
                 aes(xmin = SoccerD.threinf, xmax = SoccerD.thresup,
                     height = .6)) + 
  geom_point(data = subset(AllPSEs, Participant == "IV"), 
             aes(y = Coin.thre, x = SoccerD.thre), 
             shape = 16, color = "green", size = 2, fill = "green")
pmodsc

pmodss <- ggscatter(AllPSEs, x = "SoccerI.thre", y = "SoccerD.thre", group = "p2",
                    col = "p2",
                    xlab = "Soccer", ylab = "Balls") +
  coord_fixed(1, ylim = c(-22, 5), xlim = c(-22, 5)) +
  geom_abline(aes(intercept = modss$model$a, slope = modss$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCIts, ymin = yCIminss, ymax = yCImaxss, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") + 
  geom_errorbar(aes(ymin = SoccerD.threinf, ymax = SoccerD.thresup, col = p2, width = .2)) +
  geom_errorbarh(aes(xmin = SoccerI.threinf, xmax = SoccerI.thresup, col = p2, height = .2)) +
  theme(legend.position = "none", axis.title = element_text(size = 16))
pmodss <- pmodss +
  geom_errorbar(data = subset(AllPSEs, Participant == "IV"),
                aes(ymin = SoccerD.threinf , ymax = SoccerD.thresup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllPSEs, Participant == "IV"),
                 aes(xmin = SoccerI.threinf, xmax = SoccerI.thresup,
                     height = .6)) + 
  geom_point(data = subset(AllPSEs, Participant == "IV"), 
             aes(x = SoccerI.thre, y = SoccerD.thre), 
             shape = 16, color = "green", size = 2, fill = "green")
pmodss

pmodtsd <- ggscatter(AllPSEs, y = "SoccerD.thre", x = "Tennis.thre", group = "p2",
                    col = "p2",
                    ylab = "Balls", xlab = "Tennis") +
  coord_fixed(1, xlim = c(-20, 7), ylim = c(-20, 7)) +
  geom_abline(aes(intercept = modtsd$model$a, slope = modtsd$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCIts, ymin = yCImintsd, ymax = yCImaxtsd, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") + 
  geom_errorbar(aes(ymin = SoccerD.threinf, ymax = SoccerD.thresup, col = p2, width = .2)) +
  geom_errorbarh(aes(xmin = Tennis.threinf, xmax = Tennis.thresup, col = p2, height = .2)) +
  theme(legend.position = "none", axis.title = element_text(size = 16))
pmodtsd <- pmodtsd +
  geom_errorbar(data = subset(AllPSEs, Participant == "IV"),
                aes(ymin = SoccerD.threinf , ymax = SoccerD.thresup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllPSEs, Participant == "IV"),
                 aes(xmin = Tennis.threinf, xmax = Tennis.thresup,
                     height = .6)) + 
  geom_point(data = subset(AllPSEs, Participant == "IV"), 
             aes(y = SoccerD.thre, x = Tennis.thre), 
             shape = 16, color = "green", size = 2, fill = "green")
pmodtsd

pmodscI <- ggscatter(AllPSEs, 
                    x = "SoccerI.thre", y = "Coin.thre", group = "p2",
                    col = "p2", 
                    xlab = "Soccer", ylab = "Coins") +
  coord_fixed(1, ylim = c(-22, 5), xlim = c(-22, 5)) + 
  geom_abline(aes(intercept = modscI$model$a, slope = modscI$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCIts, ymin = yCIminscI, ymax = yCImaxscI, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") +
  geom_errorbar(aes(ymin = Coin.threinf, ymax = Coin.thresup, col = p2,
                    width = .2)) + 
  geom_errorbarh(aes(xmin = SoccerI.threinf, xmax = SoccerI.thresup, col = p2,
                     height = .2)) +
  theme(legend.position = "none", axis.title = element_text(size = 16))
pmodscI <- pmodscI +
  geom_errorbar(data = subset(AllPSEs, Participant == "IV"),
                aes(ymin = Coin.threinf , ymax = Coin.thresup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllPSEs, Participant == "IV"),
                 aes(xmin = SoccerI.threinf, xmax = SoccerI.thresup,
                     height = .6)) + 
  geom_point(data = subset(AllPSEs, Participant == "IV"), 
             aes(x = SoccerI.thre, y = Coin.thre), 
             shape = 16, color = "green", size = 2, fill = "green")
pmodscI

CombiMods <- ggarrange(pmodtc, pmodsc, pmodscI, pmodts, pmodtsd, pmodss,
                   labels = c("A", "B", "C", "D", "E", "F"),
                   ncol = 3, nrow = 2, common.legend = TRUE, legend.grob = legm,
                   legend = "right")
CombiMods
annotate_figure(CombiMods,  top = text_grob("Correlations between Biases for Different Stimuli", size = 20))

# Ortho models of STDs ----
WeightFactor7 <- sqrt(LengthCISoccerIstd) + sqrt(LengthCITennisstd)
modtsSTD <- onls(SoccerI.par ~ a + b * Tennis.par, data = AllSTDs2,
              start = list(a = 0, b = 1), window = 19,
              weights = c(1/WeightFactor7))
modtsSTD
#check_o(modtsSTD)
#qqnorm(residuals(modtsSTD))
#laarstsSTD <- nlsBoot(modtsSTD, niter = 1000)
CIpartsSTD <- confint(modtsSTD, method = "boot")
xCItsSTD <- seq(from = -2, to = 20, length.out = 63)
yCImintsSTD <- CIpartsSTD[1,1] + (CIpartsSTD[2,1] * xCItsSTD)
yCImaxtsSTD <- CIpartsSTD[1,2] + (CIpartsSTD[2,2] * xCItsSTD)

WeightFactor8 <- sqrt(LengthCICoinstd) + sqrt(LengthCITennisstd)
modtcSTD <- onls(Coin.par ~ a + b * Tennis.par, data = AllSTDs2,
              start = list(a = 1, b = 0.5), window = 30,
              weights = c(1/WeightFactor8))
modtcSTD
#check_o(modtcSTD)
#qqnorm(residuals(modtcSTD))
#laarstcSTD <- nlsBoot(modtcSTD, niter = 1000)
CIpartcSTD <- confint(modtcSTD, method = "boot")
Intercept2.5tcSTD <- mean(AllSTDs2$Coin.par) - CIpartcSTD[2,1] * mean(AllSTDs2$Tennis.par)
Intercept97.5tcSTD <- mean(AllSTDs2$Coin.par) - CIpartcSTD[2,2] * mean(AllSTDs2$Tennis.par)
yCImintcSTD <- Intercept2.5tcSTD + (CIpartcSTD[2,1] * xCItsSTD)
yCImaxtcSTD <- Intercept97.5tcSTD + (CIpartcSTD[2,2] * xCItsSTD)

WeightFactor9 <- sqrt(LengthCISoccerDstd) + sqrt(LengthCICoinstd)
modscSTD <- onls(Coin.par ~ a + b * SoccerD.par, data = AllSTDs2,
              start = list(a = 0, b = 1), window = 31,
              weights = c(1/WeightFactor9))
modscSTD
#check_o(modscSTD)
#qqnorm(residuals(modscSTD))
#laarsscSTD <- nlsBoot(modscSTD, niter = 1000)
CIparscSTD <- confint(modscSTD, method = "boot")
Intercept2.5scSTD <- mean(AllSTDs2$Coin.par) - CIparscSTD[2,1] * mean(AllSTDs2$SoccerD.par)
Intercept97.5scSTD <- mean(AllSTDs2$Coin.par) - CIparscSTD[2,2] * mean(AllSTDs2$SoccerD.par)
yCIminscSTD <- Intercept2.5scSTD + (CIparscSTD[2,1] * xCItsSTD)
yCImaxscSTD <- Intercept97.5scSTD + (CIparscSTD[2,2] * xCItsSTD)

WeightFactor10 <- sqrt(LengthCISoccerDstd) + sqrt(LengthCISoccerIstd)
modssSTD <- onls(SoccerD.par ~ a + b * SoccerI.par, data = AllSTDs2,
                 start = list(a = 2, b = 0.5), window = 24,
                 weights = c(1/WeightFactor10))
modssSTD
#check_o(modssSTD)
#qqnorm(residuals(modssSTD))
#laarsssSTD <- nlsBoot(modssSTD, niter = 1000)
CIparssSTD <- confint(modssSTD, method = "boot")
Intercept2.5ssSTD <- mean(AllSTDs2$SoccerD.par) - CIparssSTD[2,1] * mean(AllSTDs2$SoccerI.par)
Intercept97.5ssSTD <- mean(AllSTDs2$SoccerD.par) - CIparssSTD[2,2] * mean(AllSTDs2$SoccerI.par)
yCIminssSTD <- Intercept2.5ssSTD + (CIparssSTD[2,1] * xCItsSTD)
yCImaxssSTD <- Intercept97.5ssSTD + (CIparssSTD[2,2] * xCItsSTD)

WeightFactor11 <- sqrt(LengthCISoccerDstd) + sqrt(LengthCITennisstd)
modtsdSTD <- onls(SoccerD.par ~ a + b * Tennis.par, data = AllSTDs2,
                 start = list(a = 0, b = 1), window = 21,
                 weights = c(1/WeightFactor11))
modtsdSTD
#check_o(modtsdSTD)
#qqnorm(residuals(modtsdSTD))
#laarstsdSTD <- nlsBoot(modtsdSTD, niter = 1000)
CIpartsdSTD <- confint(modtsdSTD, method = "boot")
Intercept2.5tsdSTD <- mean(AllSTDs2$SoccerD.par) - CIpartsdSTD[2,1] * mean(AllSTDs2$Tennis.par)
Intercept97.5tsdSTD <- mean(AllSTDs2$SoccerD.par) - CIpartsdSTD[2,2] * mean(AllSTDs2$Tennis.par)
yCImintsdSTD <- Intercept2.5tsdSTD + (CIpartsdSTD[2,1] * xCItsSTD)
yCImaxtsdSTD <- Intercept97.5tsdSTD + (CIpartsdSTD[2,2] * xCItsSTD)

WeightFactor12 <- sqrt(LengthCICoinstd) + sqrt(LengthCISoccerIstd)
modscISTD <- onls(Coin.par ~ a + b * SoccerI.par, data = AllSTDs2,
                  start = list(a = -1, b = 1), window = 31,
                  weights = c(1/WeightFactor12))
modscISTD
#check_o(modscISTD)
#qqnorm(residuals(modscISTD))
#laarsscISTD <- nlsBoot(modscISTD, niter = 1000)
CIparscISTD <- confint(modscISTD, method = "boot")
Intercept2.5scISTD <- mean(AllSTDs2$Coin.par) - CIparscISTD[2,1] * mean(AllSTDs2$SoccerI.par)
Intercept97.5scISTD <- mean(AllSTDs2$Coin.par) - CIparscISTD[2,2] * mean(AllSTDs2$SoccerI.par)
yCIminscISTD <- Intercept2.5scISTD + (CIparscISTD[2,1] * xCItsSTD)
yCImaxscISTD <- Intercept97.5scISTD + (CIparscISTD[2,2] * xCItsSTD)

# Plots Ortho STD's----
pmodtsSTD <- ggscatter(AllSTDs2, y = "SoccerI.par", x = "Tennis.par", group = "p2",
                    col = "p2",
                    ylab = "Soccer", xlab = "Tennis") +
  coord_fixed(1, xlim= c(0, 16), ylim = c(0, 16)) +
  geom_abline(aes(intercept = modtsSTD$model$a, slope = modtsSTD$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCItsSTD, ymin = yCImintsSTD, ymax = yCImaxtsSTD, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") + 
  geom_errorbar(aes(ymin = SoccerI.parinf, ymax = SoccerI.parsup, col = p2, width = .2)) +
  geom_errorbarh(aes(xmin = Tennis.parinf, xmax = Tennis.parsup, col = p2, height = .2)) +
  theme(legend.position = "none", axis.title = element_text(size = 16)) 
pmodtsSTD <- pmodtsSTD +
  geom_errorbar(data = subset(AllSTDs2, Participant == "IV"),
                aes(ymin = SoccerI.parinf, ymax = SoccerI.parsup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllSTDs2, Participant == "IV"),
                 aes(xmin = Tennis.parinf, xmax = Tennis.parsup,
                     height = .6)) + 
  geom_point(data = subset(AllSTDs2, Participant == "IV"),
             aes(y = SoccerI.par, x = Tennis.par), 
             shape = 16, color = "green", size = 2, fill = "green")
pmodtsSTD

pmodtcSTD <- ggscatter(AllSTDs2, x = "Tennis.par", y = "Coin.par", group = "p2",
                    col = "p2",
                    xlab = "Tennis", ylab = "Coins") +
  coord_fixed(1, ylim = c(0, 16), xlim = c(0, 16)) +
  geom_abline(aes(intercept = modtcSTD$model$a, slope = modtcSTD$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCItsSTD, ymin = yCImintcSTD, ymax = yCImaxtcSTD, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") + 
  geom_errorbar(aes(ymin = Coin.parinf, ymax = Coin.parsup, col = p2, width = .2)) +
  geom_errorbarh(aes(xmin = Tennis.parinf, xmax = Tennis.parsup, col = p2, height = .2)) +
  theme(legend.position = "none", axis.title = element_text(size = 16))
pmodtcSTD <- pmodtcSTD +
  geom_errorbar(data = subset(AllSTDs2, Participant == "IV"),
                aes(ymin = Coin.parinf, ymax = Coin.parsup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllSTDs2, Participant == "IV"),
                 aes(xmin = Tennis.parinf, xmax = Tennis.parsup,
                     height = .6)) + 
  geom_point(data = subset(AllSTDs2, Participant == "IV"), 
             aes(y = Coin.par, x = Tennis.par), 
             shape = 16, color = "green", size = 2, fill = "green")
pmodtcSTD

pmodscSTD <- ggscatter(AllSTDs2, 
                    x = "SoccerD.par", y = "Coin.par", group = "p2",
                    col = "p2", 
                    xlab = "Balls", ylab = "Coins") +
  coord_fixed(1, ylim = c(0, 16), xlim = c(0, 16)) + 
  geom_abline(aes(intercept = modscSTD$model$a, slope = modscSTD$model$b),  
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCItsSTD, ymin = yCIminscSTD, ymax = yCImaxscSTD, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") +
  geom_errorbar(aes(ymin = Coin.parinf, ymax = Coin.parsup, col = p2,
                    width = .2)) + 
  geom_errorbarh(aes(xmin = SoccerD.parinf, xmax = SoccerD.parsup, col = p2,
                     height = .2)) +
  theme(legend.position = "none", axis.title = element_text(size = 16))
pmodscSTD <- pmodscSTD +
  geom_errorbar(data = subset(AllSTDs2, Participant == "IV"),
                aes(ymin = Coin.parinf, ymax = Coin.parsup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllSTDs2, Participant == "IV"),
                 aes(xmin = SoccerD.parinf, xmax = SoccerD.parsup,
                     height = .6)) + 
  geom_point(data = subset(AllSTDs2, Participant == "IV"), 
             aes(y = Coin.par, x = SoccerD.par), 
             shape = 16, color = "green", size = 2, fill = "green")
pmodscSTD

pmodssSTD <- ggscatter(AllSTDs2, x = "SoccerI.par", y = "SoccerD.par", group = "p2",
                       col = "p2",
                       xlab = "Soccer", ylab = "Balls") +
  coord_fixed(1, xlim = c(0, 16), ylim = c(0, 16)) +
  geom_abline(aes(intercept = modssSTD$model$a, slope = modssSTD$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCItsSTD, ymin = yCIminssSTD, ymax = yCImaxssSTD, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") + 
  geom_errorbar(aes(ymin = SoccerD.parinf, ymax = SoccerD.parsup, col = p2, width = .2)) +
  geom_errorbarh(aes(xmin = SoccerI.parinf, xmax = SoccerI.parsup, col = p2, height = .2)) +
  theme(legend.position = "none", axis.title = element_text(size = 16)) 
pmodssSTD <- pmodssSTD +
  geom_errorbar(data = subset(AllSTDs2, Participant == "IV"),
                aes(ymin = SoccerD.parinf, ymax = SoccerD.parsup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllSTDs2, Participant == "IV"),
                 aes(xmin = SoccerI.parinf, xmax = SoccerI.parsup,
                     height = .6)) + 
  geom_point(data = subset(AllSTDs2, Participant == "IV"), 
             aes(y = SoccerD.par, x = SoccerI.par), 
             shape = 16, color = "green", size = 2, fill = "green") 
pmodssSTD

pmodtsdSTD <- ggscatter(AllSTDs2, y = "SoccerD.par", x = "Tennis.par", group = "p2",
                       col = "p2",
                       ylab = "Balls", xlab = "Tennis") +
  coord_fixed(1, xlim = c(0, 16), ylim = c(0, 16)) +
  geom_abline(aes(intercept = modtsdSTD$model$a, slope = modtsdSTD$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCItsSTD, ymin = yCImintsdSTD, ymax = yCImaxtsdSTD, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") + 
  geom_errorbar(aes(ymin = SoccerD.parinf, ymax = SoccerD.parsup, col = p2, width = .2)) +
  geom_errorbarh(aes(xmin = Tennis.parinf, xmax = Tennis.parsup, col = p2, height = .2)) +
  theme(legend.position = "none", axis.title = element_text(size = 16)) 
pmodtsdSTD <- pmodtsdSTD +
  geom_errorbar(data = subset(AllSTDs2, Participant == "IV"),
                aes(ymin = SoccerD.parinf, ymax = SoccerD.parsup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllSTDs2, Participant == "IV"),
                 aes(xmin = Tennis.parinf, xmax = Tennis.parsup,
                     height = .6)) + 
  geom_point(data = subset(AllSTDs2, Participant == "IV"), 
             aes(y = SoccerD.par, x = Tennis.par), 
             shape = 16, color = "green", size = 2, fill = "green")
pmodtsdSTD

pmodscISTD <- ggscatter(AllSTDs2, 
                       x = "SoccerI.par", y = "Coin.par", group = "p2",
                       col = "p2", 
                       xlab = "Soccer", ylab = "Coins") +
  coord_fixed(1, ylim = c(0, 16), xlim = c(0, 16)) + 
  geom_abline(aes(intercept = modscISTD$model$a, slope = modscISTD$model$b),  
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCItsSTD, ymin = yCIminscISTD, ymax = yCImaxscISTD, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") +
  geom_errorbar(aes(ymin = Coin.parinf, ymax = Coin.parsup, col = p2,
                    width = .2)) + 
  geom_errorbarh(aes(xmin = SoccerI.parinf, xmax = SoccerI.parsup, col = p2,
                     height = .2)) +
  theme(legend.position = "none", axis.title = element_text(size = 16))
pmodscISTD <- pmodscISTD +
  geom_errorbar(data = subset(AllSTDs2, Participant == "IV"),
                aes(ymin = Coin.parinf, ymax = Coin.parsup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllSTDs2, Participant == "IV"),
                 aes(xmin = SoccerI.parinf, xmax = SoccerI.parsup,
                     height = .6)) + 
  geom_point(data = subset(AllSTDs2, Participant == "IV"), 
             aes(y = Coin.par, x = SoccerI.par), 
             shape = 16, color = "green", size = 2, fill = "green") 
pmodscISTD

CombiModsSTD <- ggarrange(pmodtcSTD, pmodscSTD, pmodscISTD, 
                          pmodtsSTD, pmodtsdSTD, pmodssSTD,
                       labels = c("A", "B", "C", "D", "E", "F"),
                       ncol = 3, nrow = 2, common.legend = TRUE, legend = "right",
                       legend.grob = legm) 
CombiModsSTD
annotate_figure(CombiModsSTD,  top = text_grob("Correlations between Variability for Different Stimuli", size = 20))

# Additional Analysis----

pAge <- ggplot(data = subset(AllPSEs1, Volunteer == TRUE), aes(x = Age, y = thre, col = Stimulustype)) +
  geom_point() +   geom_smooth(method = "lm", show_guide=FALSE) +
  geom_smooth(method = "lm", fill = NA) + 
  scale_color_manual(values = StimCol) +
  labs(y = "Bias") +
  theme_classic() + theme(legend.title = element_blank(), 
                          legend.background = element_rect(fill = "white"),
                          axis.title = element_text(size = 25),
                          axis.text = element_text(size = 20),
                          legend.text = element_text(size = 18))
pAge

pAgeSTD <- ggplot(data = subset(AllSTDs, Volunteer ==  TRUE), aes(x = Age, y = par, col = Stimulustype)) +
  geom_point() +   geom_smooth(method = "lm", show_guide=FALSE) +
  geom_smooth(method = "lm", fill = NA) + 
  scale_color_manual(values = StimCol) +
  labs(y = "Variability") +
  theme_classic() + theme(legend.title = element_blank(), 
                          legend.background = element_rect(fill = "white"),
                          axis.title = element_text(size = 25),
                          axis.text = element_text(size = 20),
                          legend.text = element_text(size = 18))
pAgeSTD

IntCol <- c("#E41A1C",  "#984EA3", "#377EB8", "#4DAF4A", "#9f1213", "#6a3672",
            "#265880", "#357a33")
pVol <- ggplot(data = AllPSEs1, aes(x = Stimulustype, y = thre, 
                            fill = interaction(Stimulustype, Volunteer))) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(pch = 21, position = position_jitterdodge(), size = 2) + 
  geom_hline(yintercept = 0, lty = 3, col = "grey30") +
  labs(y = "Bias", title = "Difference in Bias between Volunteers and Students") +
  theme_classic() + scale_fill_manual(values = IntCol, guide = "none") +
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 20),
        plot.title = element_text(size = 24, hjust = 0.5))
pVol
# dark = volunteer

pVolSTD <- ggplot(data = AllSTDs, aes(x = Stimulustype, y = par, 
                                    fill = interaction(Stimulustype, Volunteer))) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(pch = 21, position = position_jitterdodge(), size = 2) +  
  labs(y = "Variability", title = "Difference in Variability between Volunteers and Students") +
  theme_classic() + scale_fill_manual(values = IntCol, guide = "none") +
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 20),
        plot.title = element_text(size = 24, hjust = 0.5))
pVolSTD



# Plots biases presentation----
pmodts <- ggscatter(AllPSEs, y = "SoccerI.thre", x = "Tennis.thre", group = "p2",
                    col = "p2",
                    ylab = "Soccer", xlab = "Tennis") +
  coord_fixed(1, xlim = c(-21, 6), ylim = c(-21, 6)) +
  geom_abline(aes(intercept = modts$model$a, slope = modts$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCIts, ymin = yCImints, ymax = yCImaxts, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") + 
  geom_errorbar(aes(ymin = SoccerI.threinf, ymax = SoccerI.thresup, col = p2, width = .2)) +
  geom_errorbarh(aes(xmin = Tennis.threinf, xmax = Tennis.thresup, col = p2, height = .2)) +
  theme(legend.position = "right", legend.title = element_blank(), 
        legend.key.width = unit(0.3, "cm"), legend.key.height = unit(1.2, "cm"), 
        legend.text = element_text(size = 22), 
        axis.title = element_text(size = 34), axis.text = element_text(size = 26))
pmodts <- pmodts +
  geom_errorbar(data = subset(AllPSEs, Participant == "IV"),
                aes(ymin = SoccerI.threinf , ymax = SoccerI.thresup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllPSEs, Participant == "IV"),
                 aes(xmin = Tennis.threinf, xmax = Tennis.thresup,
                     height = .6)) + 
  geom_point(data = subset(AllPSEs, Participant == "IV"), 
             aes(y = SoccerI.thre, x = Tennis.thre), 
             shape = 16, color = "green", size = 2, fill = "green") 
pmodts

pmodtc <- ggscatter(AllPSEs, x = "Tennis.thre", y = "Coin.thre", group = "p2",
                    col = "p2",
                    xlab = "Tennis", ylab = "Coins") +
  coord_fixed(1, ylim = c(-21, 6), xlim = c(-21, 6)) +
  geom_abline(aes(intercept = modtc$model$a, slope = modtc$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCIts, ymin = yCImintc, ymax = yCImaxtc, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") + 
  geom_errorbar(aes(ymin = Coin.threinf, ymax = Coin.thresup, col = p2, width = .2)) +
  geom_errorbarh(aes(xmin = Tennis.threinf, xmax = Tennis.thresup, col = p2, height = .2)) +
  theme(legend.position = "none", axis.title = element_text(size = 16))
pmodtc <- pmodtc +
  geom_errorbar(data = subset(AllPSEs, Participant == "IV"),
                aes(ymin = Coin.threinf , ymax = Coin.thresup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllPSEs, Participant == "IV"),
                 aes(xmin = Tennis.threinf, xmax = Tennis.thresup,
                     height = .6)) +
  geom_point(data = subset(AllPSEs, Participant == "IV"), 
             aes(y = Coin.thre, x = Tennis.thre), 
             shape = 16, color = "green", size = 2, fill = "green")
pmodtc

pmodsc <- ggscatter(AllPSEs, 
                    x = "SoccerD.thre", y = "Coin.thre", group = "p2",
                    col = "p2", 
                    xlab = "Balls", ylab = "Coins") +
  coord_fixed(1, ylim = c(-20, 7), xlim = c(-20, 7)) + 
  geom_abline(aes(intercept = modsc$model$a, slope = modsc$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCIts, ymin = yCIminsc, ymax = yCImaxsc, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") +
  geom_errorbar(aes(ymin = Coin.threinf, ymax = Coin.thresup, col = p2,
                    width = .2)) + 
  geom_errorbarh(aes(xmin = SoccerD.threinf, xmax = SoccerD.thresup, col = p2,
                     height = .2)) +
  theme(legend.position = "right", legend.title = element_blank(), 
        legend.key.width = unit(0.3, "cm"), legend.key.height = unit(1.2, "cm"), 
        legend.text = element_text(size = 22), 
        axis.title = element_text(size = 34), axis.text = element_text(size = 26))
  
pmodsc <- pmodsc +
  geom_errorbar(data = subset(AllPSEs, Participant == "IV"),
                aes(ymin = Coin.threinf , ymax = Coin.thresup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllPSEs, Participant == "IV"),
                 aes(xmin = SoccerD.threinf, xmax = SoccerD.thresup,
                     height = .6)) + 
  geom_point(data = subset(AllPSEs, Participant == "IV"), 
             aes(y = Coin.thre, x = SoccerD.thre), 
             shape = 16, color = "green", size = 2, fill = "green")
pmodsc

pmodss <- ggscatter(AllPSEs, x = "SoccerI.thre", y = "SoccerD.thre", group = "p2",
                    col = "p2",
                    xlab = "Soccer", ylab = "Balls") +
  coord_fixed(1, ylim = c(-22, 5), xlim = c(-22, 5)) +
  geom_abline(aes(intercept = modss$model$a, slope = modss$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCIts, ymin = yCIminss, ymax = yCImaxss, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") + 
  geom_errorbar(aes(ymin = SoccerD.threinf, ymax = SoccerD.thresup, col = p2, width = .2)) +
  geom_errorbarh(aes(xmin = SoccerI.threinf, xmax = SoccerI.thresup, col = p2, height = .2)) +
  theme(legend.position = "none", axis.title = element_text(size = 16))
pmodss <- pmodss +
  geom_errorbar(data = subset(AllPSEs, Participant == "IV"),
                aes(ymin = SoccerD.threinf , ymax = SoccerD.thresup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllPSEs, Participant == "IV"),
                 aes(xmin = SoccerI.threinf, xmax = SoccerI.thresup,
                     height = .6)) + 
  geom_point(data = subset(AllPSEs, Participant == "IV"), 
             aes(x = SoccerI.thre, y = SoccerD.thre), 
             shape = 16, color = "green", size = 2, fill = "green")
pmodss

pmodtsd <- ggscatter(AllPSEs, y = "SoccerD.thre", x = "Tennis.thre", group = "p2",
                     col = "p2",
                     ylab = "Balls", xlab = "Tennis") +
  coord_fixed(1, xlim = c(-20, 7), ylim = c(-20, 7)) +
  geom_abline(aes(intercept = modtsd$model$a, slope = modtsd$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCIts, ymin = yCImintsd, ymax = yCImaxtsd, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") + 
  geom_errorbar(aes(ymin = SoccerD.threinf, ymax = SoccerD.thresup, col = p2, width = .2)) +
  geom_errorbarh(aes(xmin = Tennis.threinf, xmax = Tennis.thresup, col = p2, height = .2)) +
  theme(legend.position = "right", legend.title = element_blank(), 
        legend.key.width = unit(0.3, "cm"), legend.key.height = unit(1.2, "cm"), 
        legend.text = element_text(size = 22), 
        axis.title = element_text(size = 34), axis.text = element_text(size = 26))
pmodtsd <- pmodtsd +
  geom_errorbar(data = subset(AllPSEs, Participant == "IV"),
                aes(ymin = SoccerD.threinf , ymax = SoccerD.thresup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllPSEs, Participant == "IV"),
                 aes(xmin = Tennis.threinf, xmax = Tennis.thresup,
                     height = .6)) + 
  geom_point(data = subset(AllPSEs, Participant == "IV"), 
             aes(y = SoccerD.thre, x = Tennis.thre), 
             shape = 16, color = "green", size = 2, fill = "green")
pmodtsd

pmodscI <- ggscatter(AllPSEs, 
                     x = "SoccerI.thre", y = "Coin.thre", group = "p2",
                     col = "p2", 
                     xlab = "Soccer", ylab = "Coins") +
  coord_fixed(1, ylim = c(-22, 5), xlim = c(-22, 5)) + 
  geom_abline(aes(intercept = modscI$model$a, slope = modscI$model$b), 
              col = "grey30", lty = 1, lwd = 1) + 
  geom_ribbon(aes(x = xCIts, ymin = yCIminscI, ymax = yCImaxscI, alpha = .2, fill = "red"),
              show.legend = FALSE) +
  scale_fill_manual(values = "grey76") +
  geom_abline(aes(intercept = 0, slope = 1), col = "grey30", lty = 2) +
  scale_color_gradient(low="blue", high="red") +
  geom_errorbar(aes(ymin = Coin.threinf, ymax = Coin.thresup, col = p2,
                    width = .2)) + 
  geom_errorbarh(aes(xmin = SoccerI.threinf, xmax = SoccerI.thresup, col = p2,
                     height = .2)) +
  theme(legend.position = "none", axis.title = element_text(size = 16))
pmodscI <- pmodscI +
  geom_errorbar(data = subset(AllPSEs, Participant == "IV"),
                aes(ymin = Coin.threinf , ymax = Coin.thresup,
                    width = .6)) + 
  geom_errorbarh(data = subset(AllPSEs, Participant == "IV"),
                 aes(xmin = SoccerI.threinf, xmax = SoccerI.thresup,
                     height = .6)) + 
  geom_point(data = subset(AllPSEs, Participant == "IV"), 
             aes(x = SoccerI.thre, y = Coin.thre), 
             shape = 16, color = "green", size = 2, fill = "green")
pmodscI

CombiMods <- ggarrange(pmodtc, pmodsc, pmodscI, pmodts, pmodtsd, pmodss,
                       labels = c("A", "B", "C", "D", "E", "F"),
                       ncol = 3, nrow = 2, common.legend = TRUE, legend.grob = legm,
                       legend = "right")
