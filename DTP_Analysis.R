#####################################################################################################################################################################################
# Script number: 06 - Data analysis
# Input: 01.Final_Dataframe.txt
# 25Jan2021 - Jennifer Monereo 
# Version 1
#####################################################################################################################################################################################

## Set paths

setwd ('C:/Users/Jennifer\ Monereo/Desktop')
plotsfolder<-'C:/Users/Jennifer\ Monereo/Dropbox/04_CovVac/Plots/'
rawfolder<-'C:/Users/Jennifer\ Monereo/Desktop/04_Vac_COVID/raw/'

library(data.table)
library(ggplot2)
library(writexl)
library(data.table)
library(ggplot2)
library(writexl)
library(survival)
library(magick)
library(beepr)
library(survminer)

## Import files ____________________________________________________________________________________________________________________________________________________________________
DF <-fread(paste0(rawfolder, "01.Final_Dataframe.txt"), sep=" ", sep2="auto", dec=".", quote="\"", header="auto",na.strings = "NA") ;
name = "_2010_NoBCG"; plotsfolder<-'C:/Users/Jennifer\ Monereo/Dropbox/04_CovVac/Plots_2010_NoBCG/' ; poplevel = " hierarchical"; lim = c(0.2, 7.7)

## Format DF
V_numeric = c("ID", "Age", "BMI", "Townsend", "Lagtime_D", "Lagtime_P", "Lagtime_O", "Lagtime_T")
V_dates = c("Date_D", "Date_P", "Date_O", "Date_T", "start", "Date_COV_TEST")
V_categorical = c("Diphtheria", "Pertussis", "Other", "Tetanus", "group", "Sex", "Respiratory", "Cardiovascular", "blood_group", "Origin_COV_test",
                  "Result_COV_test", "Acute_COV_test", "Cause_Death", "tested", "COVID", "COVIDsevere")

## Make factors
for (i in V_numeric) {
  options(warn=1) # Print warnings as they occur
  DF[[i]]<-as.numeric(DF[[i]])
}
## Make dates
for (i in V_dates) {
  options(warn=1) # Print warnings as they occur
  DF[[i]]<-as.Date(DF[[i]], format =  "%d/%m/%Y")
}
## Make numeric
for (i in V_categorical) {
  options(warn=1) # Print warnings as they occur
  DF[[i]]<-as.factor(DF[[i]])
  print(levels(DF[[i]]))
}

##  ANALYSIS 1 #######################################################################################################################################################################
## Is people more often tested, had more often covid, or had more often severe covid depending on their vaccines? 
######################################################################################################################################################################################
table(is.na(DF$Respiratory))
table(is.na(DF$Cardiovascular))

table(DF$Respiratory)
table(DF$Cardiovascular)
table(DF$Cardiovascular,DF$Respiratory )

## Run Logistic regression ___________________________________________________________________________________________________________________________________________________________
colnames(DF)
vaccines = c( "Other", "Diphtheria", "Tetanus",  "Pertussis" )
analysis = c ("tested", "COVID", "COVIDsevere")

for (j in analysis) {
  temp1 <- data.frame(boxLabels=vaccines,Estimate=NA,SD=NA, Zval=NA, pval=NA, boxCILow=NA, boxOdds=NA, boxCIHigh=NA, p_label=NA, labY=NA)
  for (i in vaccines) {
    print(i)
    model<- glm(DF[[i]] ~ DF[[j]] + DF$Age + DF$Sex + DF$Townsend   , data = DF, na.action="na.exclude", family = binomial)
    templm<- summary(model)
    temp1$Estimate[temp1$boxLabels==i] <- templm$coefficients[2,1]
    temp1$SD[temp1$boxLabels==i] <- templm$coefficients[2,2]
    temp1$Zval[temp1$boxLabels==i] <- templm$coefficients[2,3]
    temp1$pval[temp1$boxLabels==i] <- templm$coefficients[2,4]
    temp2<- exp(templm$coefficients[2,1] + qnorm(c(0.025,0.5,0.975)) * templm$coefficients[2,2])
    temp1$boxCILow[temp1$boxLabels==i]<-temp2[1]
    temp1$boxOdds[temp1$boxLabels==i]<-temp2[2]
    temp1$boxCIHigh[temp1$boxLabels==i]<-temp2[3]
    temp1$p_label[temp1$boxLabels==i] <- format(temp1$pval[temp1$boxLabels==i], digits = 2, scientific = T)
    #temp1$p_label[temp1$boxLabels==i] <- gsub("e-", " x 10^", temp1$p_label[temp1$boxLabels==i])
    temp1$labY[temp1$boxLabels==i]  <- temp1$boxCIHigh[temp1$boxLabels==i]  + 0.2
    ## Reformat the p_label to make preatty plots cause I have a nerdy lovely bf
    if (temp1$pval[temp1$boxLabels==i]>0.01) { 
      temp1$p_label[temp1$boxLabels==i]<-round(temp1$pval[temp1$boxLabels==i], 2)}
    
  }
  write_xlsx(format_headers = F, temp1, paste0(plotsfolder, j, name, ".xlsx") , col_names = T) ## Place into excel
  ## Create the file for the plots
  temp1$boxLabels <- gsub('Other', 'Other\nvaccines', temp1$boxLabels)
  temp1$boxLabels <-as.factor(temp1$boxLabels)
  levels(temp1$boxLabels)
  temp1$boxLabels <- factor(temp1$boxLabels,levels(temp1$boxLabels)[c(2, 3, 1,4)])
  
  assign(paste0("model_", j), temp1)
}

## Plot the results __________________________________________________________________________________________________________________________________________________________________

## Create table for the plots
Test_other<-rbind(model_tested[1,], model_COVID[1,], model_COVIDsevere[1,])
Test_other$test <- c("tested", "positive", "severe")
Test_other$test<-as.factor(Test_other$test)
Test_other$test <- factor(Test_other$test,levels(Test_other$test)[c(3, 1, 2)])

Test_Diphtheria<-rbind(model_tested[2,], model_COVID[2,], model_COVIDsevere[2,])
Test_Diphtheria$test <- c("tested", "positive", "severe")
Test_Diphtheria$test<-as.factor(Test_Diphtheria$test)
Test_Diphtheria$test <- factor(Test_Diphtheria$test,levels(Test_Diphtheria$test)[c(3, 1, 2)])

Test_Tetanus<-rbind(model_tested[3,], model_COVID[3,], model_COVIDsevere[3,])
Test_Tetanus$test <- c("tested", "positive", "severe")
Test_Tetanus$test<-as.factor(Test_Tetanus$test)
Test_Tetanus$test <- factor(Test_Tetanus$test,levels(Test_Tetanus$test)[c(3, 1, 2)])

Test_Pertussis<-rbind(model_tested[4,], model_COVID[4,], model_COVIDsevere[4,])
Test_Pertussis$test <- c("tested", "positive", "severe")
Test_Pertussis$test<-as.factor(Test_Pertussis$test)
Test_Pertussis$test <- factor(Test_Pertussis$test,levels(Test_Pertussis$test)[c(3, 1, 2)])

## Plot it 
plot<-
  ggplot(data = Test_other,mapping = aes(y = test)) +
  geom_vline(xintercept = 1, linetype="dashed", color = "grey20", size = 1) +
  geom_errorbarh(mapping = aes(xmin = boxCILow, xmax = boxCIHigh, colour= test), size = 1.5, linetype = 7, height = 0.4, alpha = .7,  show.legend=FALSE) +
  geom_point(mapping = aes(x = boxOdds,  fill= test), colour="black",pch=23, size=2.5) +
  theme_bw() +
  coord_flip(xlim = lim) +
  geom_text(data = Test_other, aes(y=test, x=labY, label=p_label), color="black", vjust = -1, size=3) +
  theme(
    legend.position = "none",
    plot.title = element_text(color="grey20", size=12, face="bold.italic", hjust = 0.5),
    axis.text.x = element_text(size=9),
    axis.text.y = element_text(size=9),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10)) +
  labs(fill = "") +
  ggtitle("Other vaccines") +
  xlab("Odds Ration") +
  ylab(paste0("\n\nCOVID status", poplevel))
print(plot)
ggsave(plot, filename = paste0(plotsfolder, "A.OtherVaccines_", name, ".png"), width = 85, height = 85, units = "mm")


plot<-
  ggplot(data = Test_Diphtheria,mapping = aes(y = test)) +
  geom_vline(xintercept = 1, linetype="dashed", color = "grey20", size = 1) +
  geom_errorbarh(mapping = aes(xmin = boxCILow, xmax = boxCIHigh, colour= test), size = 1.5, linetype = 7, height = 0.4, alpha = .7,  show.legend=FALSE) +
  geom_point(mapping = aes(x = boxOdds,  fill= test), colour="black",pch=23, size=2.5) +
  theme_bw() +
  coord_flip(xlim = lim) +
  geom_text(data = Test_Diphtheria, aes(y=test, x=labY, label=p_label), color="black", vjust = -1, size=3) +
  theme(
    legend.position = "none",
    plot.title = element_text(color="grey20", size=12, face="bold.italic", hjust = 0.5),
    axis.text.x = element_text(size=9),
    axis.text.y = element_text(size=9),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10)) +
  labs(fill = "") +
  ggtitle(paste0("\n\nDiphtheria")) +
  xlab("") +
  ylab("")
print(plot)
ggsave(plot, filename = paste0(plotsfolder, "B.Diphtheria_", name, ".png"), width = 85, height = 85, units = "mm")



plot<-
  ggplot(data = Test_Tetanus,mapping = aes(y = test)) +
  geom_vline(xintercept = 1, linetype="dashed", color = "grey20", size = 1) +
  geom_errorbarh(mapping = aes(xmin = boxCILow, xmax = boxCIHigh, colour= test), size = 1.5, linetype = 7, height = 0.4, alpha = .7,  show.legend=FALSE) +
  geom_point(mapping = aes(x = boxOdds,  fill= test), colour="black",pch=23, size=2.5) +
  theme_bw() +
  coord_flip(xlim = lim) +
  geom_text(data = Test_Tetanus, aes(y=test, x=labY, label=p_label), color="black", vjust = -1, size=3) +
  theme(
    legend.position = "none",
    plot.title = element_text(color="grey20", size=12, face="bold.italic", hjust = 0.5),
    axis.text.x = element_text(size=9),
    axis.text.y = element_text(size=9),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10)) +
  labs(fill = "") +
  ggtitle("\n\nTetanus") +
  xlab("") +
  ylab("")
print(plot)
ggsave(plot, filename = paste0(plotsfolder, "C.Tetanus_", name, ".png"), width = 85, height = 85, units = "mm")


plot<-
  ggplot(data = Test_Pertussis,mapping = aes(y = test)) +
  geom_vline(xintercept = 1, linetype="dashed", color = "grey20", size = 1) +
  geom_errorbarh(mapping = aes(xmin = boxCILow, xmax = boxCIHigh, colour= test), size = 1.5, linetype = 7, height = 0.4, alpha = .7,  show.legend=FALSE) +
  geom_point(mapping = aes(x = boxOdds,  fill= test), colour="black",pch=23, size=2.5) +
  theme_bw() +
  coord_flip(xlim = lim) +
  geom_text(data = Test_Pertussis, aes(y=test, x=labY, label=p_label), color="black", vjust = -1, size=3) +
  theme(
    legend.position = "none",
    plot.title = element_text(color="grey20", size=12, face="bold.italic", hjust = 0.5),
    axis.text.x = element_text(size=9),
    axis.text.y = element_text(size=9),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10)) +
  labs(fill = "") +
  ggtitle("\n\nPertussis") +
  xlab("") +
  ylab("")
print(plot)
ggsave(plot, filename = paste0(plotsfolder, "D.Pertussis_", name, ".png"), width = 85, height = 85, units = "mm")


## Merge the plots with magic _______________________________________________________________________________________________________________________________________________________
OtherVaccines <- image_read(paste0(plotsfolder,"A.OtherVaccines_", name, ".png"))
Diphtheria <- image_read(paste0(plotsfolder,"B.Diphtheria_", name, ".png"))
Tetanus <- image_read(paste0(plotsfolder,"C.Tetanus_", name, ".png"))
Pertussis <- image_read(paste0(plotsfolder,"D.Pertussis_", name, ".png"))

comb3 <- image_append(c(Diphtheria,Tetanus, Pertussis),stack = F)
comb4 <- image_transparent(comb3, "white", fuzz = 0)
image_write(comb4,paste0(plotsfolder,"Fig1_ComboVacc", name, ".png"))


## Merge the plots hierarchical and full sample
Basic_10BGC <- image_read("C:/Users/Jennifer Monereo/Dropbox/04_CovVac/Plots_2010_NoBCG/Fig1_ComboVacc_2010_NoBCG.png")
NAto0_10BGC <- image_read("C:/Users/Jennifer Monereo/Dropbox/04_CovVac/Plots_2010_NoBCG/NAto0/Fig1_ComboVacc_2010_NoBCG_NAto0.png")

## Title
Basic_10BGC <- image_annotate(Basic_10BGC, paste0("COVID status, hierarchical"), size = 70, gravity = "northwest", color = "black", location = "+0+0", )
NAto0_10BGC <- image_annotate(NAto0_10BGC, paste0("COVID status, among all participants"), size = 70, gravity = "northwest", color = "black", location = "+0+0", )

## Merge
comb3 <- image_append(c(Basic_10,NAto0_10),stack = T)
comb4 <- image_transparent(comb3, "white", fuzz = 0)
image_write(comb4, "C:/Users/Jennifer Monereo/Dropbox/04_CovVac/Plots_2010/Fig1_AllMerged.png")

comb3 <- image_append(c(Basic_10BGC,NAto0_10BGC),stack = T)
comb4 <- image_transparent(comb3, "white", fuzz = 0)
image_write(comb4, "C:/Users/Jennifer Monereo/Dropbox/04_CovVac/Plots_2010_NoBCG/Fig1_AllMerged_NoBGC.png")


##  ANALYSIS 2 #######################################################################################################################################################################
## Cox regression
######################################################################################################################################################################################
DF_long<-DF

## Create group: type of vaccine
DF_long$type[(DF_long$Tetanus==1 | DF_long$Diphtheria==1) ] <- "DandorT"
DF_long$type[(DF_long$Tetanus==0 & DF_long$Diphtheria==0) ] <- "No_DandorT"
DF_long$type <- as.factor(DF_long$type)
DF_long$type <- factor(DF_long$type,levels(DF_long$type)[c(2, 1)])

## Event/censor
DF_long$status <- 1 ## End of FU
DF_long$status[DF_long$COVID==1 & !is.na(DF_long$COVID) & DF_long$COVIDsevere==0 & !is.na(DF_long$COVIDsevere)] <- 1 ## Censored
DF_long$status[                                           DF_long$COVIDsevere==1 & !is.na(DF_long$COVIDsevere)] <- 2 ## Event
DF_long$statusKM <-DF_long$status

## Time to event/censor
## Event: severe FU -> Date test (91 do not have date because died untestet)
## Censoring: Non-severe COVID (Date test) or end of study (date end)
DF_long$DATE <- as.Date("01/04/2021", format =  "%d/%m/%Y") ## Date end of FU

DF_long$DATE         [DF_long$COVID==1 & !is.na(DF_long$COVID) & DF_long$COVIDsevere==0 & !is.na(DF_long$COVIDsevere)] <- 
  DF_long$Date_COV_TEST[DF_long$COVID==1 & !is.na(DF_long$COVID) & DF_long$COVIDsevere==0 & !is.na(DF_long$COVIDsevere)] ## Date censoring

DF_long$DATE         [                                           DF_long$COVIDsevere==1 & !is.na(DF_long$COVIDsevere)] <- 
  DF_long$Date_COV_TEST[                                           DF_long$COVIDsevere==1 & !is.na(DF_long$COVIDsevere)] ## Date event

table(is.na(DF_long$DATE)) 
DF_long<-DF_long[!is.na(DF_long$DATE)] ## Remove those 91 with no date

table(DF_long$COVID)

## Survival days: difference between start pandemic and DATE
DF_long$time <- difftime(DF_long$DATE , DF_long$start,  units = "days") ## Surv.days

table(is.na(DF_long$statusKM))
table(is.na(DF_long$time))


## All participans or hierarchical
## _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
## Among everyone
Y_lim = c(0.985, 1);
surv.med = "hv"
p = F
type_ev = "Amongeveryone_EventSevere_CensoringNonSevere_or_endOfFU";
legend = "none"

## _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_

# # ## _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
## Only among people with COVID yes
DF_long<-DF_long[DF_long$COVID ==1]
type_ev = "AmongCOVID_EventSevere_CensoringNonSevere_or_endOfFU";
Y_lim = c(0, 1);
legend = "top"
p = F
surv.med = "hv"

# # # ## _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_

## KM curve  _________________________________________________________________________________________________________________________________________________________________________
bla <-survfit(Surv(time, statusKM) ~ type, data = DF_long)
print(bla)

survp <- 
  ggsurvplot(bla, data = DF_long, 
             conf.int = TRUE, #cumevents = T, cumcensor = T, risk.table = T,
             pval = p,
             
             ggtheme = theme_bw(), palette = c("#0072B2", "#E69F00"),
             ylim = Y_lim, 
             surv.median.line = surv.med,
             font.tickslab =10, 
             
             title = "", 
             font.title = 12,
             
             xlab = "\nDays since start of the pandemic", 
             font.x = 12,
             ylab = "Survival probability\n", 
             font.y = 12,
             
             legend = legend,
             legend.title = "Vaccination:",
             legend.labs = c( "No Diphtheria nor Tetanus; ", "Diphtheria and/or Tetanus"),
             font.legend = 12)

print(survp)
ggsave(print(survp), filename = paste0(plotsfolder, "KMcurve_", type_ev,  name, ".png"), width = 200, height = 150, units = "mm")


## Merge the plots hierarchical and full sample

Everyone <- image_read(paste0(plotsfolder, "KMcurve_", "Amongeveryone_EventSevere_CensoringNonSevere_or_endOfFU",  name, ".png"))
COVIDpos <- image_read(paste0(plotsfolder, "KMcurve_", "AmongCOVID_EventSevere_CensoringNonSevere_or_endOfFU",  name, ".png"))

COVIDpos <- image_annotate(COVIDpos, paste0("COVID severity, among COVID positive"), size = 70, gravity = "northwest", color = "black", location = "+0+0", )
Everyone <- image_annotate(Everyone, paste0("COVID severity, among all participants"), size = 70, gravity = "northwest", color = "black", location = "+0+0", )


comb3 <- image_append(c(COVIDpos,Everyone),stack = T)
comb4 <- image_transparent(comb3, "white", fuzz = 0)
image_write(comb4, paste0(plotsfolder, "Survival_AllMerged", name, ".png"))



## Cox regression ___________________________________________________________________________________________________________________________________________________________________
res.cox<-coxph(Surv(time, statusKM) ~ type + Age + Sex + Townsend + Respiratory, data = DF_long, id = ID)
print(res.cox)

sink(paste0(plotsfolder, "Res_Cox", type_ev, name, ".txt"))
print(summary(res.cox))
sink()  # returns output to the console


## End _____________________________________________________________________________________________________________________________________________________________________________
rm(list=setdiff(ls(), c( "plotsfolder", "rawfolder")))
print("End of part 05 - Analysis")
beep()
## End _____________________________________________________________________________________________________________________________________________________________________________
