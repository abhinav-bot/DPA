#MEDICATION.CSV

medication <- read.csv("medications.csv")
medication[medication== ""] <- NA
module <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
medication_new<- module(medication,"RXDDRUG")
medication_new<- module(medication_new,"RXDDRGID")
medication_new<- module(medication_new,"RXDRSD1")
medication_new<- module(medication_new,"RXQSEEN")
medication_new<- module(medication_new,"RXDRSC1")
medication_new<- module(medication_new,"RXDCOUNT")
medication_new <- subset(medication_new, select = -RXDRSC2)
View(medication_new)
medication_new <- subset(medication_new, select = -RXDRSC3)
medication_new <- subset(medication_new, select = -RXDRSD2)
medication_new <- subset(medication_new, select = -RXDRSD3)
medication_new["Is_Diabetic"] <- NA
medication_new$Is_Diabetic  <- ifelse(medication_new$RXDRSC1 == "E11", 1 , 0)
medication_new <- subset(medication_new, select = c(SEQN,Is_Diabetic))


library(plyr)
meds <- ddply(medication_new,.(SEQN), summarize, Is_Diabetic = paste(Is_Diabetic, collapse = ","))
library(splitstackshape)
medication_1 <- cSplit(meds, "Is_Diabetic", ",")

library(tidyr)
library(dplyr)

medication_2 <- mutate(medication_1, final_IsDia = ifelse(Is_Diabetic_01 | Is_Diabetic_02 |Is_Diabetic_03 |Is_Diabetic_04 |Is_Diabetic_05 |Is_Diabetic_06 |Is_Diabetic_07|Is_Diabetic_08 |Is_Diabetic_09 |Is_Diabetic_10 |Is_Diabetic_11 |Is_Diabetic_12 |Is_Diabetic_13 |Is_Diabetic_14 |Is_Diabetic_15 |Is_Diabetic_16 |Is_Diabetic_17 |Is_Diabetic_18 |Is_Diabetic_19 |Is_Diabetic_20 |Is_Diabetic_20 |Is_Diabetic_21 |Is_Diabetic_22 |Is_Diabetic_23  == 1, 1 , 0))

medication_2 <- subset(medication_2, select = c(SEQN,final_IsDia))

medication_2[is.na(medication_2)] <- 0

View(medication_2)

write.csv(medication_2, 'medication_new.csv')

#Labs file
labs <- read.csv("labs.csv")
install.packages("SASxport")
install.packages("Hmisc")
library(Hmisc)
library(SASxport)
lookup.xport("PBCD_H.XPT")
labs1 <- read.xport("PBCD_H.XPT")
lookup.xport("TCHOL_H.XPT")
labs2 <- read.xport("TCHOL_H.XPT")
lookup.xport("CBC_H.XPT")
labs3 <- read.xport("CBC_H.XPT")
lookup.xport("GHB_H.XPT")
labs4 <- read.xport("GHB_H.XPT")
lookup.xport("INS_H.XPT")
labs5<- read.xport("INS_H.XPT")
lookup.xport("OGTT_H.XPT")
labs6<- read.xport("OGTT_H.XPT")
labs1_new <- subset(labs1, select = c (SEQN,LBXBCD,LBXBPB,LBXBMN,LBXTHG,LBXBSE))
labs2_new <- subset(labs2, select = c(SEQN,LBXTC))
labs3_new <- subset(labs3, select = c(SEQN,LBDLYMNO,LBDMONO,LBDNENO,LBDEONO,LBDBANO,LBXRBCSI,LBXHGB,LBXPLTSI))
labs5_new <- subset(labs5, select = c(SEQN,LBXIN,PHAFSTHR))
labs6_new <- subset(labs6, select = c(SEQN,LBXGLT,GTXDRANK))
labs_new <- merge(labs1_new,labs2_new, by = "SEQN")
labs_new <- merge(labs_new,labs3_new, by = "SEQN")
labs_new <- merge(labs_new,labs4, by = "SEQN")
labs_new <- merge(labs_new,labs5_new, by = "SEQN")
labs_new <- merge(labs_new,labs6_new, by = "SEQN")
for(i in 1:ncol(labs_new)){
  labs_new[is.na(labs_new[,i]), i] <- mean(labs_new[,i], na.rm = TRUE)
}
labs_new["Is_Diabetic"] <- NA
labs_new$Is_Diabetic  <- ifelse(labs_new$LBXGH >= 6.0, 1 , 0)
write.csv(labs_new,'labs_new.csv')

#Diet File
lookup.xport("DSQTOT_H.xpt")
a<-read.xport("DSQTOT_H.xpt")
write.csv(a,"dietary.csv")
lookup.xport("DSQIDS_H.xpt")
b<-read.xport("DSQIDS_H.xpt")
write.csv(b,"dietary1.csv")
c<-merge(a,b)
write.csv(c,"FINAL_DIET_FILE.csv")
FINAL_DIET_FILE <- read.csv("FINAL_DIET_FILE.csv")
View(FINAL_DIET_FILE)
FINAL_DIET_FILE<-subset(FINAL_DIET_FILE,select=c(SEQN,DSDCOUNT,DSDANCNT,DSD010,DSD010AN,DSQIKCAL,DSQIPROT,DSQICARB,DSQISUGR,DSQIFIBE,DSQITFAT,DSQISFAT,DSQIMFAT,DSQIPFAT,DSQICHOL,DSQILYCO,DSQILZ,DSQIVB1,DSQIVB2,DSQINIAC,DSQIVB6,DSQIFA,DSQIFDFE,DSQICHL,DSQIVB12,DSQIVC,DSQIVK,DSQIVD,DSQICALC,DSQIPHOS,DSQIMAGN,DSQIIRON,DSQIZINC,DSQICOPP,DSQISODI,DSQIPOTA,DSQISELE,DSQICAFF,DSQIIODI))
module(FINAL_DIET_FILE,C("DSQIKCAL","DSQIPROT","DSQICARB","DSQISUGR","DSQIFIBE","DSQITFAT","DSQISFAT","DSQIMFAT","DSQIPFAT","DSQICHOL","DSQILYCO","DSQILZ","DSQIVB1","DSQIVB2","DSQINIAC","DSQIVB6","DSQIFA","DSQIFDFE","DSQICHL","DSQIVB12","DSQIVC","DSQIVK","DSQIVD","DSQICALC","DSQIPHOS","DSQIMAGN","DSQIIRON","DSQIZINC","DSQICOPP","DSQISODI","DSQIPOTA","DSQISELE","DSQICAFF","DSQIIODI"))

FINAL_DIET_FILE[colSums(!is.na(FINAL_DIET_FILE))>0]
for(i in 1:ncol(FINAL_DIET_FILE)){
  data[is.na(FINAL_DIET_FILE[,i]), i] <- round(mean(FINAL_DIET_FILE[,i], na.rm = TRUE))
}
FINAL_DIET_FILE$DSQILYCO<-round(FINAL_DIET_FILE$DSQILYCO)
FINAL_DIET_FILE$DSQICAFF<-round(FINAL_DIET_FILE$DSQICAFF)
head(FINAL_DIET_FILE)
cols<-names(FINAL_DIET_FILE)[1:39]
d<-FINAL_DIET_FILE %>% mutate_each_(funs(round(.,1)), cols)
write.csv(d,"FINAL_DIET_FILE.csv")

###############################################################################################################

#Demographic file

demographicData = read.csv("demographic.csv")  
colnames(demographicData)
str(demographicData)
sapply(demographicData, class)

# Changing Gender to Factor and revaluing Levels from 1,2 to Male, Female respectively

names(demographicData)[4]<-"Gender"
class(demographicData["Gender"])
demographicData$Gender <- as.factor(demographicData$Gender)
levels(demographicData$Gender)
levels(demographicData$Gender)[levels(demographicData$Gender)=="1"] <- "Male"
levels(demographicData$Gender)[levels(demographicData$Gender)=="2"] <- "Female"
levels(demographicData$Gender)

#Cleaning AgeinYears and AgeinMonths
names(demographicData)[5]<-"AgeYears"
names(demographicData)[6]<-"AgeMonths"
names(demographicData)[10]<-"AgeMonthsU19"
demographicData[c(5:6,10)] <- lapply(demographicData[c(5:6,10)], as.numeric)


demographicData$AgeMonthsU19<-demographicData$AgeMonthsU19/12
demographicData$AgeMonthsU19<-round(demographicData$AgeMonthsU19,1)

demographicData$AgeMonths<-demographicData$AgeMonths/12
demographicData$AgeMonths<-round(demographicData$AgeMonths,1)

indicesMonths19<-which(!is.na(demographicData$AgeMonthsU19))
demographicData$AgeYears[indicesMonths19]<-demographicData$AgeMonthsU19[indicesMonths19]

indicesYears<-which(is.na(demographicData$AgeYears))
demographicData$AgeYears[indicesYears]<-demographicData$AgeMonths[indicesYears]


#Ethnic variables
names(demographicData)[7]<-"Race"
names(demographicData)[8]<-"RaceNHA"
demographicData$Race <- as.factor(demographicData$Race)
demographicData$RaceNHA <- as.factor(demographicData$RaceNHA)
levels(demographicData$Race)
levels(demographicData$RaceNHA)
levels(demographicData$Race) <- c("MexAmerican","OtherHisp","NHWhite","NHBlack","Other")
levels(demographicData$RaceNHA) <- c("NHA-MexAmerican","NHA-OtherHisp","NHA-NHWhite","NHA-NHBlack","NHA-NHAsian","Other")
levels(demographicData$Race)
levels(demographicData$RaceNHA)

#Birth and Citizen Columns
names(demographicData)[13]<-"BirthCountry"
names(demographicData)[14]<-"Citizenship"
demographicData$BirthCountry <- as.factor(demographicData$BirthCountry)
demographicData$Citizenship <- as.factor(demographicData$Citizenship)
levels(demographicData$BirthCountry) <- c("USA","NON-USA","Refused","DontKnow")
levels(demographicData$Citizenship) <- c("USCitizen","NonUSCitizen","Refused","DontKnow")
levels(demographicData$BirthCountry)
levels(demographicData$Citizenship)


names(demographicData)[15]<-"USStayLength"
demographicData$USStayLength <- as.factor(demographicData$USStayLength)
levels(demographicData$USStayLength) <- c("<1","1-5","5-10","10-15","15-20","20-30","30-40","40-50",">50","Refused","DontKnow")
levels(demographicData$USStayLength)

#Education Status Columns

names(demographicData)[16]<-"EduLevelChild"
names(demographicData)[17]<-"EduLevelAdult"
demographicData$EduLevelChild <- as.factor(demographicData$EduLevelChild)
demographicData$EduLevelAdult <- as.factor(demographicData$EduLevelAdult)
levels(demographicData$EduLevelChild)
levels(demographicData$EduLevelAdult)
levels(demographicData$EduLevelChild) <- c("NoEdu/KG","1Grade","2Grade","3Grade","4Grade","5Grade","6Grade","7Grade","8Grade","9Grade","10Grade","11Grade","12Grade","HighSchool","GED",">HighSchool",">5Grade",">9Grade","DontKnow")
levels(demographicData$EduLevelAdult) <- c(">9Grade","9-11Grade","HighSchool/GED","College/AA","College/Above","Refused","DontKnow")
levels(demographicData$EduLevelAdult) <- c(">9Grade","9-11Grade","HighSchool/GED","College/AA","College/Above","Refused","DontKnow","NoEdu/KG","1Grade","2Grade","3Grade","4Grade","5Grade","6Grade","7Grade","8Grade","9Grade","10Grade","11Grade","12Grade","HighSchool","GED",">HighSchool",">5Grade",">9Grade","Elementary")
levels(demographicData$EduLevelAdult) <- c("Elementary","HighSchool/GED","HighSchool/GED","College/AA","College/AA","Refused","DontKnow","NoEdu/KG","Elementary","Elementary","Elementary","Elementary","Elementary","Elementary","Elementary","Elementary","HighSchool/GED","HighSchool/GED","HighSchool/GED","HighSchool/GED","HighSchool/GED","HighSchool/GED","HighSchool/GED","Elementary","HighSchool/GED","Elementary")

indicesNAEdu<-which(is.na(demographicData$EduLevelAdult))
demographicData$EduLevelAdult[indicesNAEdu]<-demographicData$EduLevelChild[indicesNAEdu]


#Marital Status/Pregnancy Columns

names(demographicData)[18]<-"MaritalStatus"
names(demographicData)[19]<-"PregnancyStatus"
demographicData$MaritalStatus <- as.factor(demographicData$MaritalStatus)
demographicData$PregnancyStatus <- as.factor(demographicData$PregnancyStatus)
levels(demographicData$MaritalStatus)
levels(demographicData$PregnancyStatus)
levels(demographicData$MaritalStatus) <- c("Married","Widowed","Divorced","Separated","NeverMarried","LivingIn","Refused","DontKnow")
levels(demographicData$PregnancyStatus) <- c("Pregnant","NotPregnant","CannotTell")
indicesMale<-which(demographicData$Gender=="Male")
demographicData$PregnancyStatus[indicesMale]==demographicData$Gender[indicesNAEdu]

#Prepping Family related variables

names(demographicData)[30]<-"PeopleHH"
names(demographicData)[31]<-"PeopleFamily"
names(demographicData)[45]<-"HHIncome"
names(demographicData)[46]<-"FamilyIncome"
names(demographicData)[47]<-"PovertyRatio"
demographicData$PeopleHH <- as.integer(demographicData$PeopleHH)
demographicData$PeopleFamily <- as.integer(demographicData$PeopleFamily)
demographicData$HHIncome <- as.factor(demographicData$HHIncome)
demographicData$FamilyIncome <- as.factor(demographicData$FamilyIncome)
demographicData$PovertyRatio <- as.numeric(demographicData$PovertyRatio)

levels(demographicData$PeopleHH)
levels(demographicData$PeopleFamily)
levels(demographicData$HHIncome)
levels(demographicData$FamilyIncome)

levels(demographicData$HHIncome) <- c("<5k","5-10k","10-15k","15-20k","20-25k","25-35k","35-45k","45-55k","55-65k","65-75k",">20k","<20k","75-99k",">100k","Refused","DontKnow")
levels(demographicData$FamilyIncome) <- c("<5k","5-10k","10-15k","15-20k","20-25k","25-35k","35-45k","45-55k","55-65k","65-75k",">20k","<20k","75-99k",">100k","Refused","DontKnow")


# Cleaning the file to remove the columns not used for analysis

demographicData<-subset(demographicData, select=-c(2:3,9,11:12,20:29,32:44))
colnames(demographicData)

demographicData<-subset(demographicData, select=-c(4,7,11))
colnames(demographicData)

demographicData["EduLevelAdult"=="3Grade"]<-"Elementary"


levels(demographicData$EduLevelAdult)[demographicData$EduLevelAdult=="4Grade"]<-"Elementary"


write.csv(demographicData,"demographic_new.csv")


