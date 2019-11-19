med <- read.csv("/Users/srivatsasrinathmurthy/Desktop/medications.csv")
med[med== ""] <- NA
cf <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
m_n<- cf(med,"RXDDRUG")
m_n<- cf(m_n,"RXDDRGID")
m_n<- cf(m_n,"RXDRSD1")
m_n<- cf(m_n,"RXQSEEN")
m_n<- cf(m_n,"RXDRSC1")
m_n<- cf(m_n,"RXDCOUNT")
m_n <- subset(m_n, select = -RXDRSC2)
View(m_n)
m_n <- subset(m_n, select = -RXDRSC3)
m_n <- subset(m_n, select = -RXDRSD2)
m_n <- subset(m_n, select = -RXDRSD3)
m_n["Is_Diabetic"] <- NA
m_n$Is_Diabetic  <- ifelse(m_n$RXDRSC1 == "E11", 1 , 0)
m_n <- subset(m_n, select = c(SEQN,Is_Diabetic))



library(plyr)
meds <- ddply(m_n,.(SEQN), summarize, Is_Diabetic = paste(Is_Diabetic, collapse = ","))
library(splitstackshape)
medication_1 <- cSplit(meds, "Is_Diabetic", ",")

library(tidyr)
library(dplyr)

medication_2 <- mutate(medication_1, final_IsDia = ifelse(Is_Diabetic_01 | Is_Diabetic_02 |Is_Diabetic_03 |Is_Diabetic_04 |Is_Diabetic_05 |Is_Diabetic_06 |Is_Diabetic_07|Is_Diabetic_08 |Is_Diabetic_09 |Is_Diabetic_10 |Is_Diabetic_11 |Is_Diabetic_12 |Is_Diabetic_13 |Is_Diabetic_14 |Is_Diabetic_15 |Is_Diabetic_16 |Is_Diabetic_17 |Is_Diabetic_18 |Is_Diabetic_19 |Is_Diabetic_20 |Is_Diabetic_20 |Is_Diabetic_21 |Is_Diabetic_22 |Is_Diabetic_23  == 1, 1 , 0))

medication_2 <- subset(medication_2, select = c(SEQN,final_IsDia))

medication_2[is.na(medication_2)] <- 0

View(medication_2)

write.csv(medication_2, 'm_n.csv')


library(Hmisc)
library(SASxport)
lookup.xport("/Users/srivatsasrinathmurthy/Desktop/PBCD_H.XPT")
l1 <- read.xport("/Users/srivatsasrinathmurthy/Desktop/PBCD_H.XPT")
lookup.xport("/Users/srivatsasrinathmurthy/Desktop/TCHOL_H.XPT")
l2 <- read.xport("/Users/srivatsasrinathmurthy/Desktop/TCHOL_H.XPT")
lookup.xport("/Users/srivatsasrinathmurthy/Desktop/CBC_H.XPT")
l3 <- read.xport("/Users/srivatsasrinathmurthy/Desktop/CBC_H.XPT")
lookup.xport("/Users/srivatsasrinathmurthy/Desktop/GHB_H.XPT")
l4 <- read.xport("/Users/srivatsasrinathmurthy/Desktop/GHB_H.XPT")
lookup.xport("/Users/srivatsasrinathmurthy/Desktop/INS_H.XPT")
l5<- read.xport("/Users/srivatsasrinathmurthy/Desktop/INS_H.XPT")
lookup.xport("/Users/srivatsasrinathmurthy/Desktop/OGTT_H.XPT")
l6<- read.xport("/Users/srivatsasrinathmurthy/Desktop/OGTT_H.XPT")
l1_n <- subset(l1, select = c (SEQN,LBXBCD,LBXBPB,LBXBMN,LBXTHG,LBXBSE))
l2_n <- subset(l2, select = c(SEQN,LBXTC))
l3_n <- subset(l3, select = c(SEQN,LBDLYMNO,LBDMONO,LBDNENO,LBDEONO,LBDBANO,LBXRBCSI,LBXHGB,LBXPLTSI))
l5_n <- subset(l5, select = c(SEQN,LBXIN,PHAFSTHR))
l6_n <- subset(l6, select = c(SEQN,LBXGLT,GTXDRANK))
labs_new <- merge(l1_n,l2_n, by = "SEQN")
labs_new <- merge(labs_new,l3_n, by = "SEQN")
labs_new <- merge(labs_new,l4, by = "SEQN")
labs_new <- merge(labs_new,l5_n, by = "SEQN")
labs_new <- merge(labs_new,l6_n, by = "SEQN")
for(i in 1:ncol(labs_new)){
  labs_new[is.na(labs_new[,i]), i] <- mean(labs_new[,i], na.rm = TRUE)
}
labs_new["Is_Diabetic"] <- NA
labs_new$Is_Diabetic  <- ifelse(labs_new$LBXGH >= 6.0, 1 , 0)
write.csv(labs_new,'labs_new.csv')

