#####################################################################################################################################################################################
## Project: DTP
## Cross immunity for COVID of DTP vaccines:  diphtheria, pertussis (whooping cough), and tetanus 
#####################################################################################################################################################################################
# Script number: 01 - Get the codes for the vaccines
# Input: all_lkps_maps_v2.xlsx
# 23Jan2021 - Jennifer Monereo 
# Version 1
#####################################################################################################################################################################################
#####################################################################################################################################################################################

rm(list=setdiff(ls(), c("DF")))

## Set paths
setwd ('C:/Users/Jennifer\ Monereo/Desktop')
plotsfolder<-'C:/Users/Jennifer\ Monereo/Dropbox/04_CovVac/Plots/'
rawfolder<-'C:/Users/Jennifer\ Monereo/Desktop/04_Vac_COVID/raw/'

## Load libraries
library(data.table)
library(beepr)
library(dplyr)
library(future) 
library(openxlsx)
library(tidyr)
library(ggplot2)
library(ggpubr)
library( writexl)


## Import file ______________________________________________________________________________________________________________________________________________________________________

# Import sheets, look for any vacc term in the full sheet and remove the rows with no term 
sheetsDF = c("read_ctv3_read_v2",  "read_v2_lkp", "read_v2_drugs_lkp", "read_v2_read_ctv3", "read_ctv3_lkp") 

for (i in sheetsDF) {
  CODEBOOK <- read.xlsx(paste0(rawfolder, "all_lkps_maps_v2.xlsx"), sheet = i, colNames = TRUE, 
                        rowNames = FALSE ,na.strings = "NA")
  
  CODEBOOK$Vacc_info <- apply(CODEBOOK, 1, function(x)as.integer(any(grep("vacc|immunisation|booster",x, ignore.case=TRUE)))) # Is vacc present? 
  
  temp<-CODEBOOK %>% filter(Vacc_info == 1) # Remove when vacc not present
  temp<-distinct(temp, .keep_all = TRUE)
  assign( i, temp) # Create a CB for each sheet that includes only codes with vacc
}

## Remove dataframes with 0 observations, meaning they don't mention vaccines ________________________________________________________________________________________________________
isEmpty <- function(x) {is.data.frame(x) && nrow(x) == 0L} # create a function that returns a logical value
empty <- unlist(eapply(.GlobalEnv, isEmpty)) # apply it over the environment
rm(list = names(empty)[empty]) # remove the empties

sheetsDF1<-subset(sheetsDF, !sheetsDF %in% names(empty)[empty]) ## Create list of dataframes that only include the ones not removed

## Separate DF for type of code and merge it in unique DFs __________________________________________________________________________________________________________________________
colnames(read_ctv3_lkp)
READ3_read_ctv3_lkp<-read_ctv3_lkp[colnames(read_ctv3_lkp) %in% c("read_code","term_description")]
colnames(READ3_read_ctv3_lkp)<-c("READ3_CODE", "DESCRIPTION")
rm(read_ctv3_lkp)

colnames(read_ctv3_read_v2)
diff<-read_ctv3_read_v2[!read_ctv3_read_v2$READV2_DESC == read_ctv3_read_v2$TERMV2_DESC,]
diff<-diff[rowSums(is.na(diff)) != ncol(diff), ]
READ2_read_ctv3_read_v2<-read_ctv3_read_v2[colnames(read_ctv3_read_v2) %in% c("READV2_CODE","READV2_DESC")]
colnames(READ2_read_ctv3_read_v2)<-c("READ2_CODE", "DESCRIPTION")
READ3_read_ctv3_read_v2<-read_ctv3_read_v2[colnames(read_ctv3_read_v2) %in% c("READV3_CODE","READV2_DESC")]
colnames(READ3_read_ctv3_read_v2)<-c("READ3_CODE", "DESCRIPTION")
rm(read_ctv3_read_v2)

colnames(read_v2_drugs_lkp)
READ2_read_v2_drugs_lkp<-read_v2_drugs_lkp[colnames(read_v2_drugs_lkp) %in% c("read_code","term_description")]
colnames(READ2_read_v2_drugs_lkp)<-c("READ2_CODE", "DESCRIPTION")
rm(read_v2_drugs_lkp)

colnames(read_v2_lkp)
READ2_read_v2_lkp<-read_v2_lkp[colnames(read_v2_lkp) %in% c("read_code","term_description")]
colnames(READ2_read_v2_lkp)<-c("READ2_CODE", "DESCRIPTION")
rm(read_v2_lkp)

colnames(read_v2_read_ctv3)
READ2_read_v2_read_ctv3<-read_v2_read_ctv3[colnames(read_v2_read_ctv3) %in% c("READV2_CODE","READV2_DESC")]
colnames(READ2_read_v2_read_ctv3)<-c("READ2_CODE", "DESCRIPTION")
READ3_read_v2_read_ctv3<-read_v2_read_ctv3[colnames(read_v2_read_ctv3) %in% c("TERMV3_CODE","TERMV3_DESC")]
colnames(READ3_read_v2_read_ctv3)<-c("READ3_CODE", "DESCRIPTION")
rm(read_v2_read_ctv3)

sheetsDF1 = c("READ3_read_ctv3_lkp", "READ2_read_ctv3_read_v2", "READ2_read_v2_drugs_lkp", "READ3_read_ctv3_read_v2",
  "READ2_read_v2_lkp", "READ2_read_v2_read_ctv3", "READ3_read_v2_read_ctv3") 

## Merge those with the same code type
READ2_merged <- rbind(READ2_read_ctv3_read_v2, READ2_read_v2_drugs_lkp, READ2_read_v2_lkp, READ2_read_v2_read_ctv3)
READ3_merged <- rbind(READ3_read_ctv3_lkp, READ3_read_ctv3_read_v2, READ3_read_v2_read_ctv3)
rm(list = sheetsDF1)

sheetsDF1 = c( #"ALTCO_merged", "ICD09_merged", "ICD10_merged", TERM3_merged",
  "READ2_merged", "READ3_merged" ) 

## Look for TDP terms  _____________________________________________________________________________________________________________________________________________________________
for (j in sheetsDF1) {
  temp<-get(j)
  
  temp$Tet <- as.numeric(apply(get(j), 1, function(x)as.integer(any(grep("etanus|Tet|Tet/|DT |DT-|DTBIS|clostet",x, ignore.case=TRUE)))))
  temp$Dip <- as.numeric(apply(get(j), 1, function(x)as.integer(any(grep("iphtheria|dipther|DT |DT-|DTBIS",x, ignore.case=TRUE)))))
  temp$Per <- as.numeric(apply(get(j), 1, function(x)as.integer(any(grep("ertussis|whooping|DTaP",x, ignore.case=TRUE)))))
  temp$DTP <- as.numeric(apply(get(j), 1, function(x)as.integer(any(grep("DTP|DTaP|infanrix",x, ignore.case=TRUE)))))
  temp$BCG <- as.numeric(apply(get(j), 1, function(x)as.integer(any(grep("Calmette|BCG",x, ignore.case=TRUE)))))
  
  temp$Oth <- as.numeric(apply(get(j), 1, function(x)as.integer(any(grep("cholera|typhoid|mening|pneumococcal|plague|tularaemia|anthrax|haemophilus|polio|smallpox|measles|rubella|yellow fever|rabies|influenza|hepatitis|mumps|encephalit|arthropod|leishmaniasis|MMR|flu|cowpox|lactobacill acidophil|corynebacterium parvum|botulism|engerix|fluarix|begrivac|agrippal|APV|Pnu-Imune|Pneumovax|prevenar|imuvax|rabipur|typhim|Vivotif|Stamaril|Priorix|NeisVac-C|Menjugate|Havrix|VAQTA|Epaxal|Act-HIB|Hibtiter|Hiberix|Eolarix|Trivax-Hib|Twinrix|Hepatyrix|Vari|ViATIM|Vari|rota|MENITORIX|Protozoal|Cat allergy|Dog allergy|Horse allergy|House dust allergy|House mite allergy|Vivotif|Arilvax|HibTiter|Rouvax|Trivax|ACT-HIB|Almevax|Ervevax|Pneumovax|Engerix|Mengivac|Fluvirin|Fluzone|Havrix|Influvac|MFV-Ject|Typhim|Avaxim|Begrivac|Priorix|Hiberix|Imovax|Epaxal|Invivac|Agrippal|Enzira|Prevenar|Eczema|Hydroa|Mycobacterium",x, ignore.case=TRUE)))))
  
  temp$remove <- as.numeric(apply(get(j), 1, function(x)as.integer(any(grep("No consent|antitoxin |Reaction to |at home|vacciniforme|Mycobacterium vaccae| call|History of|certificate|Encephalitis, myelitis and encephalomyelitis|Eczema vaccinatum|complication|at surgery|at hospital|Needs|at work|at hosp.|Post-immunisation|Postimmunisation |Viral vaccinations: [other]|advised|single antigen |Did not attend|DNA |Requires|refused|vaccine allergy|vaccines allergy|advice|declined|invitation|Recommend|contra-indicated|consent given|products|Storage|Provision|offered|Education|service|No previous bacillus|administration|Pseudocowpox|^Viral infection NOS$|Infectious disease:prevent|indication|status|invite|requested|High priority|Need for|contraindicated|exc|P22|unsucces|contraind|Infection after|Accidental|Foreign object|Failure|not indicated|Normal immunoglobulin|Contaminated substance|email|text|Pre-school|Post |poisoning|adverse reaction|adverse effects|low dose|-associated",x, ignore.case=TRUE)))))
  temp1<-temp
  temp1 <- temp1[grepl(1, temp1$remove),]
  temp <- temp[!grepl(1, temp$remove),]
  
  assign(paste0(j), temp)
  assign(paste0("Remov_", j), temp1)
}

## create vectors for each vaccine description and save it into a file (to check we are grabbing the correct codes) _________________________________________________________________
for (i in sheetsDF1) {
  temp <- get(i)
  vec <- as.vector(unlist(temp$DESCRIPTION[temp$Tet==1]))
  assign(paste0("Tet_", substr(i,0, 5)), vec)}
V_Tet<-unlist(c(unique(mget(ls(pattern = 'Tet_+')))))
V_Tet <- unique(V_Tet)
write.table(x = V_Tet, file =  paste0(plotsfolder, "Descriptions_tetanus.txt"), quote = F, row.names = F)

for (i in sheetsDF1) {
  temp <- get(i)
  vec <- as.vector(unlist(temp$DESCRIPTION[temp$Dip==1]))
  assign(paste0("Dip_", substr(i,0, 5)), vec)}
V_Dip<-unlist(c(unique(mget(ls(pattern = 'Dip_+')))))
V_Dip <- unique(V_Dip)
write.table(x = V_Dip, file =  paste0(plotsfolder, "Descriptions_Diphtheria.txt"), quote = F, row.names = F)

for (i in sheetsDF1) {
  temp <- get(i)
  vec <- as.vector(unlist(temp$DESCRIPTION[temp$Per==1]))
  assign(paste0("Per_", substr(i,0, 5)), vec)}
V_Per<-unlist(c(unique(mget(ls(pattern = 'Per_+')))))
V_Per <- unique(V_Per)

write.table(x = V_Per, file =  paste0(plotsfolder, "Descriptions_Pertussis.txt"), quote = F, row.names = F)

for (i in sheetsDF1) {
  temp <- get(i)
  vec <- as.vector(unlist(temp$DESCRIPTION[temp$DTP==1]))
  assign(paste0("DTP_", substr(i,0, 5)), vec)}
V_DTP<-unlist(c(unique(mget(ls(pattern = 'DTP_+')))))
V_DTP <- unique(V_DTP)

write.table(x = V_DTP, file =  paste0(plotsfolder, "Descriptions_DTP.txt"), quote = F, row.names = F)

for (i in sheetsDF1) {
  temp <- get(i)
  vec <- as.vector(unlist(temp$DESCRIPTION[temp$Oth==1]))
  assign(paste0("Oth_", substr(i,0, 5)), vec)}
V_Oth<-unlist(c(unique(mget(ls(pattern = 'Oth_+')))))
V_Oth <- unique(V_Oth)

write.table(x = V_Oth, file =  paste0(plotsfolder, "Descriptions_Other.txt"), quote = F, row.names = F)

for (i in sheetsDF1) {
  temp <- get(i)
  vec <- as.vector(unlist(temp$DESCRIPTION[temp$BCG==1]))
  assign(paste0("BCG_", substr(i,0, 5)), vec)}
V_BCG<-unlist(c(unique(mget(ls(pattern = 'BCG_+')))))
V_BCG <- unique(V_BCG)

write.table(x = V_BCG, file =  paste0(plotsfolder, "Descriptions_BCG.txt"), quote = F, row.names = F)

## Once all codes are right, create vectors for each vaccine CODE  _________________________________________________________________________________________________________________
V_Tet_READ3 <- as.vector(unique(unlist(READ3_merged$READ3_CODE[READ3_merged$Tet==1])))
V_Dip_READ3 <- as.vector(unique(unlist(READ3_merged$READ3_CODE[READ3_merged$Dip==1])))
V_Per_READ3 <- as.vector(unique(unlist(READ3_merged$READ3_CODE[READ3_merged$Per==1])))
V_DTP_READ3 <- as.vector(unique(unlist(READ3_merged$READ3_CODE[READ3_merged$DTP==1])))
V_Oth_READ3 <- as.vector(unique(unlist(READ3_merged$READ3_CODE[READ3_merged$Oth==1])))
V_BCG_READ3 <- as.vector(unique(unlist(READ3_merged$READ3_CODE[READ3_merged$BCG==1])))

V_Tet_READ2 <- as.vector(unique(unlist(READ2_merged$READ2_CODE[READ2_merged$Tet==1])))
V_Dip_READ2 <- as.vector(unique(unlist(READ2_merged$READ2_CODE[READ2_merged$Dip==1])))
V_Per_READ2 <- as.vector(unique(unlist(READ2_merged$READ2_CODE[READ2_merged$Per==1])))
V_DTP_READ2 <- as.vector(unique(unlist(READ2_merged$READ2_CODE[READ2_merged$DTP==1])))
V_Oth_READ2 <- as.vector(unique(unlist(READ2_merged$READ2_CODE[READ2_merged$Oth==1])))
V_BCG_READ2 <- as.vector(unique(unlist(READ2_merged$READ2_CODE[READ2_merged$BCG==1])))

## Clean
rm(list=setdiff(ls(), c( "plotsfolder", "rawfolder",
                         "V_Tet_READ2", "V_Tet_READ3", 
                         "V_Dip_READ2", "V_Dip_READ3", 
                         "V_Per_READ2", "V_Per_READ3", 
                         "V_DTP_READ2", "V_DTP_READ3" , 
                         "V_BCG_READ2", "V_BCG_READ3" , 
                         "V_Oth_READ2", "V_Oth_READ3")))

## End _____________________________________________________________________________________________________________________________________________________________________________
print("End of part 01")
beep()


#####################################################################################################################################################################################
#####################################################################################################################################################################################
# Script number: 02 - Aplly codes to gp file, and identify everyone with vaccine info
# Input: Clinical gp data from UKbb
# 23Jan2021 - Jennifer Monereo 
# Version 1
#####################################################################################################################################################################################
#####################################################################################################################################################################################
memory.size(max = FALSE)
memory.limit(size = 400000000)
plan(multisession)

## Import file  ____________________________________________________________________________________________________________________________________________________________________
raw <-fread(paste0(rawfolder, "gp_clinical.txt"), sep="auto", sep2="auto", dec=".", quote="\"", header="auto", na.strings = "NA", drop = c(2, 6, 7 ,8))

## Define col names
colnames(raw)<- c("eid", "event_dt", "read_2", "read_3")          

## Make NA into NA
raw[raw=="NA"]<-NA
raw[raw==""]<-NA

## Remove duplicated
raw1 <- raw %>% distinct ## Same as unique but faster

## Add an index column
raw1$Index <- seq.int(nrow(raw1))

## Remove people with no events
raw1 <- raw1[!(is.na(raw1$read_2) & is.na(raw1$read_3)), ]

## Loop for each row and look for the codes
raw1$Diphtheria <- NA
raw1$Pertussis <- NA
raw1$Dtp <- NA
raw1$Other <- NA
raw1$Tetanus <- NA
raw1$BCG <- NA

raw1$Diphtheria <- ifelse((raw1$read_2 %in% V_Dip_READ2) | (raw1$read_3 %in% V_Dip_READ3), 1,0)
raw1$Pertussis <- ifelse((raw1$read_2 %in% V_Per_READ2) | (raw1$read_3 %in% V_Per_READ3), 1,0)
raw1$Dtp <- ifelse((raw1$read_2 %in% V_DTP_READ2) | (raw1$read_3 %in% V_DTP_READ3), 1,0)
raw1$Other <- ifelse((raw1$read_2 %in% V_Oth_READ2) | (raw1$read_3 %in% V_Oth_READ3), 1,0)
raw1$Tetanus <- ifelse((raw1$read_2 %in% V_Tet_READ2) | (raw1$read_3 %in% V_Tet_READ3), 1,0)
raw1$BCG <- ifelse((raw1$read_2 %in% V_BCG_READ2) | (raw1$read_3 %in% V_BCG_READ3), 1,0)

## Remove unnecessary cols and rename
raw1<-as.data.frame(raw1)
raw1<-raw1[colnames(raw1) %in% c("eid", "event_dt",  "Diphtheria", "Pertussis", "Dtp", "Other",  "Tetanus",  "BCG")]

colnames(raw1)<-c("ID", "Date", "Diphtheria"  , "Pertussis"  ,  "Dtp"    ,      "Other"    ,    "Tetanus", "BCG" )

raw2 <- raw1 %>% distinct

rm(raw1)

## Save into files
write.table(x = raw2, file =  paste0(rawfolder, "raw.txt"), quote = F, row.names = F)

## Clean
rm(list=setdiff(ls(), c( "plotsfolder", "rawfolder",
                         "V_Tet_READ2", "V_Tet_READ3", 
                         "V_Dip_READ2", "V_Dip_READ3", 
                         "V_Per_READ2", "V_Per_READ3", 
                         "V_DTP_READ2", "V_DTP_READ3" , 
                         "V_BCG_READ2", "V_BCG_READ3" , 
                         "V_Oth_READ2", "V_Oth_READ3")))

## End _____________________________________________________________________________________________________________________________________________________________________________
print("End of part 02")
beep()



#####################################################################################################################################################################################
#####################################################################################################################################################################################
# Script number: 03 - create final vaccine dataset.
# Input: output from script 02
# 25Jan2021 - Jennifer Monereo 
# Version 1
#####################################################################################################################################################################################
#####################################################################################################################################################################################

## Import files ____________________________________________________________________________________________________________________________________________________________________
bla1   <- fread(paste0(rawfolder, "raw.txt"), sep="auto", sep2="auto", dec=".", quote="\"", header="auto",na.strings = "NA")

## When DTP is =1 then D, T and P = 1. Remove DTP column ___________________________________________________________________________________________________________________________
bla1$Diphtheria[bla1$Dtp == 1] <-1
bla1$Pertussis[bla1$Dtp == 1] <-1
bla1$Tetanus[bla1$Dtp == 1] <-1

## Get last 10 years record ______________________________________________________________________________________________________________________________________________________
tooOld <- c("2010-03-01")
tooOld <- as.Date(tooOld, format = "%Y-%m-%d")
tooNew <- c("2020-01-01")
tooNew <- as.Date(tooNew, format = "%Y-%m-%d")

bla1$Date <- as.Date(bla1$Date, format =  "%d/%m/%Y")
bla1<-subset(bla1, Date > (tooOld))
bla1<-subset(bla1, Date < (tooNew))


## Separate by grup _________________________________________________________________________________________________________________________________________________________________
colnames(bla1)
Diphtheria<-as.data.frame(bla1[,c("ID", "Date", "Diphtheria")])
Pertussis<-as.data.frame(bla1[,c("ID", "Date", "Pertussis")])
Other<-as.data.frame(bla1[,c("ID", "Date", "Other")])
Tetanus<-as.data.frame(bla1[,c("ID", "Date", "Tetanus")])
BCG<-as.data.frame(bla1[,c("ID", "Date", "BCG")])

##Rename
colnames(Diphtheria)<-c("ID", "Date_D", "Diphtheria")
colnames(Pertussis)<-c("ID", "Date_P", "Pertussis")
colnames(Other)<-c("ID", "Date_O", "Other")
colnames(Tetanus)<-c("ID", "Date_T", "Tetanus")
colnames(BCG)<-c("ID", "Date_B", "BCG")

## Order for newest date, then order for whether they have the vaccine (1 first). Then remove duplicated. ___________________________________________________________________________
Diphtheria<-Diphtheria[order(Diphtheria$Date_D, decreasing = T),]
Diphtheria<-Diphtheria[order(Diphtheria$Diphtheria , decreasing = T),]
Diphtheria<-Diphtheria[!duplicated(Diphtheria$ID, fromLast = F), ]

Pertussis<-Pertussis[order(Pertussis$Date_P, decreasing = T),]
Pertussis<-Pertussis[order(Pertussis$Pertussis , decreasing = T),]
Pertussis<-Pertussis[!duplicated(Pertussis$ID, fromLast = F), ]

Other<-Other[order(Other$Date_O, decreasing = T),]
Other<-Other[order(Other$Other , decreasing = T),]
Other<-Other[!duplicated(Other$ID, fromLast = F), ]

Tetanus<-Tetanus[order(Tetanus$Date_T, decreasing = T),]
Tetanus<-Tetanus[order(Tetanus$Tetanus , decreasing = T),]
Tetanus<-Tetanus[!duplicated(Tetanus$ID, fromLast = F), ]

BCG<-BCG[order(BCG$Date_B, decreasing = T),]
BCG<-BCG[order(BCG$BCG , decreasing = T),]
BCG<-BCG[!duplicated(BCG$ID, fromLast = F), ]

## Merge them in a DF
bla1<-full_join(Diphtheria, Pertussis, by = "ID", all = T )
bla2<-full_join(bla1, Other, by = "ID", all = T )
bla3<-full_join(bla2, Tetanus, by = "ID", all = T )
bla4<-full_join(bla3, BCG, by = "ID", all = T )

## Remove people that has 0000 in all vaccines
bla4$remove<-bla4$Diphtheria + bla4$Pertussis + bla4$Other + bla4$Tetanus + bla4$BCG
table(bla4$remove)

bla5 <- bla4[!grepl(0, bla4$remove),]
table(bla5$remove)

par(mfrow=c(2, 3))
lapply(bla4[c(2, 4, 6, 8, 10)], FUN=hist, breaks = 100)
dev.off()

## Eexclude people with BCG
table(bla5$BCG)
table(bla5$BCG, bla5$Tetanus)

bla5 <- bla5[!grepl(1, bla5$BCG),] ; name = "_2010_NoBCG"

colnames(bla5)
table(bla5$remove)
bla5 <- bla5[ , -which(names(bla5) %in% c("Date_B","BCG", "remove"))]


write.table(x = bla5, file =  paste0(rawfolder, "GP_Dataframe.txt"), quote = F, row.names = F)

## End _____________________________________________________________________________________________________________________________________________________________________________
rm(list=setdiff(ls(), c( "plotsfolder", "rawfolder")))
print("End of part 03")
beep()


#####################################################################################################################################################################################
#####################################################################################################################################################################################
# Script number: 04 - Merge with other datasets
# Input: GP_Dataframe_all.txt
# 25Jan2021 - Jennifer Monereo 
# Version 1
#####################################################################################################################################################################################
#####################################################################################################################################################################################

## Import files _____________________________________________________________________________________________________________________________________________________________________

## Vaccines 
## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

vaccines <-fread(paste0(rawfolder, "GP_Dataframe.txt"), sep=" ", sep2="auto", dec=".", quote="\"", header="auto",na.strings = "NA") ; name = "_2010_NoBCG"

# Create groups
colnames(vaccines)
sum(is.na(vaccines))

vaccines$group <- 
  ifelse((vaccines$Diphtheria %in% 1) & (vaccines$Tetanus %in% 1) & (vaccines$Pertussis %in% 1) , "DTP",
         
         ifelse((vaccines$Diphtheria %in% 1) & (vaccines$Tetanus %in% 0) & (vaccines$Pertussis %in% 0) , "Dip",
                ifelse((vaccines$Diphtheria %in% 1) & (vaccines$Tetanus %in% 1) & (vaccines$Pertussis %in% 0) , "Dip_tet",
                       ifelse((vaccines$Diphtheria %in% 1) & (vaccines$Tetanus %in% 0) & (vaccines$Pertussis %in% 1) , "Dip_Per",
                              
                              ifelse((vaccines$Diphtheria %in% 0) & (vaccines$Tetanus %in% 1) & (vaccines$Pertussis %in% 0) , "Tet",
                                     ifelse((vaccines$Diphtheria %in% 0) & (vaccines$Tetanus %in% 1) & (vaccines$Pertussis %in% 1) , "Tet_Per",
                                            
                                            ifelse((vaccines$Diphtheria %in% 0) & (vaccines$Tetanus %in% 0) & (vaccines$Pertussis %in% 1) , "Per",
                                                   
                                                   ifelse((vaccines$Diphtheria %in% 0) & (vaccines$Tetanus %in% 0) & (vaccines$Pertussis %in% 0)  & (vaccines$Other %in% 1), "NoDTP_Other",
                                                          
                                                          ifelse((vaccines$Diphtheria %in% 0) & (vaccines$Tetanus %in% 0) & (vaccines$Pertussis %in% 0)  , "NoInfo", "CHECK"
                                                          )))))))))


table(vaccines$Pertussis)                                                                                             
table(vaccines$Diphtheria)                                                                                             

table(vaccines$group)
length(unique(vaccines$ID)) == nrow(vaccines)


## Covid test results  
## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

covid_test <-fread(paste0(rawfolder, "covid19_result_110421.txt"), sep="auto", sep2="auto", dec=".", quote="\"", header="auto",na.strings = "NA")

nrow(covid_test) ## data contained information on 157,884 tests
length(unique(covid_test$eid)) ## unique individuals

colnames(covid_test)
covid_test<-as.data.frame(covid_test[,c("eid", "specdate", "origin", "result", "acute")])
covid_test<-unique(covid_test)

colnames(covid_test)<-c("ID", "Date_COV_TEST", "Origin_COV_test", "Result_COV_test", "Acute_COV_test")
table(covid_test$Result_COV_test)

length(unique(covid_test$ID)) == nrow(covid_test) #unique ID? No, clean
# Some people has several tests. Remove duplicated ID, preserving for each person the one that was positive test, and in addition, with an Origin=1
covid_test1 <- covid_test[order(covid_test$Result_COV_test, covid_test$Origin_COV_test, decreasing=TRUE),]
covid_test1 <- covid_test1[!duplicated(covid_test1$ID),]
length(unique(covid_test1$ID)) == nrow(covid_test1) #unique ID? No, clean


## Death information 
## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

death <-fread(paste0(rawfolder, "death_cause.txt"), sep="auto", sep2="auto", dec=".", quote="\"", header="auto",na.strings = "NA")
death<-unique(death)

deathCodes <- death[grepl("U07", death$cause_icd10),]
table(deathCodes$cause_icd10)

death$CauseDeath <- ifelse(death$cause_icd10 == "U071", "1", ## Covid identified
                           ifelse(death$cause_icd10 == "U072", "0", ## Covid not identified
                                  "0")) ## No covid related
table(death$CauseDeath)
death<-as.data.frame(death[,c("eid", "CauseDeath")])
colnames(death)<- c("ID", "Cause_Death")
death<-unique(death)
length(unique(death$ID)) == nrow(death) 

table(death$Cause_Death)

death1 <- death[order(death$Cause_Death, decreasing=TRUE),]
death1 <- death1[!duplicated(death1$ID),]
table(death1$Cause_Death)

length(unique(death1$ID)) == nrow(death1) 

## Demographiscs
## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
demog <-fread(paste0(rawfolder, "CovidVacCovars_130421.txt"), sep="auto", sep2="auto", dec=".", quote="\"", header="auto",na.strings = "NA")
colnames(demog)
demog<-as.data.frame(demog[,c("ID", "Age", "Sex", "BMI", "Townsend", "Respiratory", "Cardiovascular")])
demog<-unique(demog)

length(unique(demog$ID)) == nrow(demog) 


## Merge  ____________________________________________________________________________________________________________________________________________________________________________
bla<-full_join(vaccines, demog, by = "ID")
bla1<-full_join(bla, blood, by = "ID")
bla2<-full_join(bla1, covid_test1, by = "ID")
bla3<-full_join(bla2, death1, by = "ID")

bla3<-unique(bla3)
length(unique(bla3$ID)) == nrow(bla3) 

## Export combined DF ________________________________________________________________________________________________________________________________________________________________
write.table(x = bla3, file =  paste0(rawfolder, "00.Combined_Dataframe.txt"), quote = F, row.names = F)

## End _____________________________________________________________________________________________________________________________________________________________________________
rm(list=setdiff(ls(), c( "plotsfolder", "rawfolder")))
print("End of part 04 - Merge with other datasets")
beep()


######################################################################################################################################################################################
######################################################################################################################################################################################
# Script number: 05 - Prepare final dataset
# Input: 00.Combined_Dataframe.txt
# 25Jan2021 - Jennifer Monereo 
# Version 1
######################################################################################################################################################################################
######################################################################################################################################################################################

## Import files ____________________________________________________________________________________________________________________________________________________________________
raw <-fread(paste0(rawfolder, "00.Combined_Dataframe.txt"), sep=" ", sep2="auto", dec=".", quote="\"", header="auto",na.strings = "NA") ; name = "_2010_NoBCG"
length(unique(raw$ID)) == nrow(raw) ## Be sure there are no duplicated ID
DF<-raw

## Create new variables ____________________________________________________________________________________________________________________________________________________________
## Was the participant tested for Covid? 
table(is.na(DF$Result_COV_test))
DF$tested<-0
DF$tested[!is.na(DF$Result_COV_test)]<-1
DF$tested[DF$Cause_Death ==1]<-1
table(DF$tested)

## Have the person had COVID?  
## COVID==1 includes people with a positive test and people who died of COVID
DF$COVID<-NA
DF$COVID[DF$tested==1]<-0
DF$COVID[DF$Result_COV_test==1]<-1
DF$COVID[DF$Cause_Death==1]<-1
table(DF$COVID)

## Have the person had SEVERE COVID?  
## COVIDsevere <- among people with COVID, how many had severe or died?
DF$COVIDsevere <- NA
DF$COVIDsevere[DF$COVID==1] <- 0
DF$COVIDsevere[DF$COVID==1 & DF$Cause_Death==1] <- 1
DF$COVIDsevere[DF$COVID==1 & DF$Origin_COV_test==1] <- 1
table(DF$COVIDsevere)


## Remove people with missing covariates ___________________________________________________________________________________________________________________________________________
DF<-DF[!is.na(DF$Townsend),] ## Only TOwnshed
length(complete.cases(DF))==nrow(DF)

## Format DF ______________________________________________________________________________________________________________________________________________________________________

## Flip Townsend sigh
DF$Townsend<-(-DF$Townsend)

##  as categorical
DF$Sex<-as.factor(DF$Sex)
DF$group<-as.factor(DF$group)

## as dates
DF$Date_COV_TEST <- as.Date(DF$Date_COV_TEST, format =  "%d/%m/%Y")
DF$Date_D <- as.Date(DF$Date_D, format =  "%d/%m/%Y")
DF$Date_P <- as.Date(DF$Date_P, format =  "%d/%m/%Y")
DF$Date_O <- as.Date(DF$Date_O, format =  "%d/%m/%Y")
DF$Date_T <- as.Date(DF$Date_T, format =  "%d/%m/%Y")
DF$start <- as.Date("01/03/2020", format =  "%d/%m/%Y")
DF$end <- as.Date("01/04/2021", format =  "%d/%m/%Y")
DF<-as.data.frame(DF)

## Remove dates where no vaccine
DF$Date_D[DF$Diphtheria==0]<-NA
DF$Date_P[DF$Pertussis==0]<-NA
DF$Date_T[DF$Tetanus==0]<-NA
DF$Date_O[DF$Other==0]<-NA

## Calculate delay time between vaccination and start of pandemic
V_Options = c("Date_D", "Date_P", "Date_D" ,"Date_O" ,"Date_T")

for (i in colnames(DF[colnames(DF) %in% V_Options])) {
  options(warn=1) 
  print(i)
  DF$temp <- difftime(DF[[i]] , DF$start,  units = "days") ## MRI - PHQ# dates
  DF$temp <- abs(as.numeric(DF$temp)) ## Make it absolute and numeric
  names(DF)[names(DF) == "temp"]  <- paste0("Lagtime", substr(i, 5, nchar(i))) ## Save as "diff_in_days_old_FU##"
}

## Remove cases with no data about vaccines
bla<-DF$ID[is.na(DF$Diphtheria) & is.na(DF$Pertussis) & is.na(DF$Tetanus) & is.na(DF$Other)]
DF<-DF[!DF$ID %in% bla,]

## Export DF _______________________________________________________________________________________________________________________________________________________________________
write.table(x = DF, file =  paste0(rawfolder, "01.Final_Dataframe.txt"), quote = F, row.names = F)

## End _____________________________________________________________________________________________________________________________________________________________________________
rm(list=setdiff(ls(), c( "plotsfolder", "rawfolder")))
print("End of part 05 - Create final DF")
beep()
## End _____________________________________________________________________________________________________________________________________________________________________________
