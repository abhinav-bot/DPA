ld <- read.table('C:/Users/mrame/OneDrive/Desktop/Labelling files/lab_med_diet_new.csv',sep=",", header=TRUE)
head(labelledData)
length(unique(labelledData$SEQN))
#labs labelled data values are stored
Lld <- read.table('C:/Users/mrame/OneDrive/Desktop/Labelling files/labs_new.csv',sep=",", header=TRUE)
head(Lld)
length(unique(Lld$SEQN))
ncol(Lld)
Lld<-Lld[,c(2,22)]
#medication labelled data
Mld <- read.table('C:/Users/mrame/OneDrive/Desktop/Labelling files/medication_new.csv',sep=",", header=TRUE)
head(Mld)
length(unique(Mld$SEQN))
Mld<-Mld[,c(2,3)]
#labs medication
Lm <- read.table('C:/Users/mrame/OneDrive/Desktop/Labelling files/lab_med_new.csv',sep=",", header=TRUE)
head(Lm)
length(unique(Lm$SEQN))
Examiniation <- read.table('C:/Users/mrame/OneDrive/Desktop/Labelling files/examination_new.csv',sep=",", header=TRUE)
head(Examiniation)
length(unique(Examiniation$SEQN))

DemDietLabsMed<-merge(cleanDemographicData, labelledData, by='SEQN', all.y=TRUE)
length(unique(DemDietLabsMed$SEQN))

DemMed<-merge(cleanDemographicData, MedicationlabelledData, by='SEQN', all.y=TRUE)
length(unique(DemMed$SEQN))

FinalLabels<-merge(MedicationlabelledData, LabslabelledData, by='SEQN', all=TRUE)
head(FinalLabels)
nrow(FinalLabels)
length(unique(FinalLabels$SEQN))

which(FinalLabels$final_IsDia==0)
length(which(FinalLabels$final_IsDia==1))
toChange<-which(FinalLabels$Is_Diabetic==1)
FinalLabels$Is_Diabetic[toChange]
FinalLabels$final_IsDia[toChange]<-FinalLabels$Is_Diabetic[toChange]
