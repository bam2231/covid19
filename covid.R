## COVID-19 Project
## Coding Author: Bryn McCarthy

rm(list=ls())
setwd("~/Desktop/Coding Projects")
library(dplyr)
library(plyr)
library(reshape2)
library(dataCompareR)
library(rstanarm)
set.seed(9904)
#Reading in all datasets

us.self<-read.csv("usselfcompleted.csv")
us.self$country<-c(rep("us",nrow(us.self)))
names(us.self) <- gsub("[.]", "", names(us.self)) 

us.family<-read.csv("usfamcompleted.csv")
us.family$country<-c(rep("us",nrow(us.family)))
names(us.family) <- gsub("[.]", "", names(us.family)) 

sa.fam<-read.csv("sa.fam.csv")
sa.fam$country<-c(rep("south america",nrow(sa.fam)))
names(sa.fam) <- gsub("[.]", "", names(sa.fam)) 

sa.self<-read.csv("sa.self.csv")
sa.self$country<-c(rep("south america",nrow(sa.self)))
names(sa.self) <- gsub("[.]", "", names(sa.self)) 

sp.fam<-read.csv("sp.fam.csv")
sp.fam$country<-c(rep("spain",nrow(sp.fam)))
names(sp.fam) <- gsub("[.]", "", names(sp.fam)) 

sp.self<-read.csv("sp.self.csv")
sp.self$country<-c(rep("spain",nrow(sp.self)))
names(sp.self) <- gsub("[.]", "", names(sp.self)) 

af.fam<-read.csv("af.fam.csv")
af.fam$country<-c(rep("afghanistan",nrow(af.fam)))
names(af.fam) <- gsub("[.]", "", names(af.fam)) 

af.self<-read.csv("af.self.csv")
af.self$country<-c(rep("afghanistan",nrow(af.self)))
names(af.self) <- gsub("[.]", "", names(af.self)) 

arab.fam<-read.csv("arab.fam.csv")
arab.fam$country<-c(rep("arab",nrow(arab.fam)))
names(arab.fam) <- gsub("[.]", "", names(arab.fam)) 

arab.self<-read.csv("arab.self.csv")
arab.self$country<-c(rep("arab",nrow(arab.self)))
names(arab.self) <- gsub("[.]", "", names(arab.self)) 

fr.fam<-read.csv("fr.fam.csv")
fr.fam$country<-c(rep("france",nrow(fr.fam)))
names(fr.fam) <- gsub("[.]", "", names(fr.fam)) 

fr.self<-read.csv("fr.self.csv")
fr.self$country<-c(rep("france",nrow(fr.self)))
names(fr.self) <- gsub("[.]", "", names(fr.self)) 

fram.fam<-read.csv("fram.fam.csv")
fram.fam$country<-c(rep("france arab",nrow(fram.fam)))
names(fram.fam) <- gsub("[.]", "", names(fram.fam)) 

fram.self<-read.csv("fram.self.csv")
fram.self$country<-c(rep("france arab",nrow(fram.self)))
names(fram.self) <- gsub("[.]", "", names(fram.self)) 

in.fam<-read.csv("in.fam.csv")
in.fam$country<-c(rep("india",nrow(in.fam)))
names(in.fam) <- gsub("[.]", "", names(in.fam)) 

in.self<-read.csv("in.self.csv")
in.self$country<-c(rep("india",nrow(in.self)))
names(in.self) <- gsub("[.]", "", names(in.self)) 

it.fam<-read.csv("it.fam.csv")
it.fam$country<-c(rep("italy",nrow(it.fam)))
names(it.fam) <- gsub("[.]", "", names(it.fam)) 

it.self<-read.csv("it.self.csv")
it.self$country<-c(rep("italy",nrow(it.self)))
names(it.self) <- gsub("[.]", "", names(it.self)) 

korea.fam<-read.csv("korea.fam.csv")
korea.fam$country<-c(rep("korea",nrow(korea.fam)))
names(korea.fam) <- gsub("[.]", "", names(korea.fam)) 

korea.self<-read.csv("korea.self.csv")
korea.self$country<-c(rep("korea",nrow(korea.self)))
names(korea.self) <- gsub("[.]", "", names(korea.self)) 

linkedin.self<-read.csv("linkedin.self.csv")
linkedin.self$country<-c(rep("linkedin",nrow(linkedin.self)))
names(linkedin.self) <- gsub("[.]", "", names(linkedin.self))

linkedin.fam<-read.csv("linkedin.fam.csv")
linkedin.fam$country<-c(rep("linkedin",nrow(linkedin.fam)))
names(linkedin.fam)<-gsub("[.]", "", names(linkedin.fam))

#Merging all datasets
all.self<-list(af.self,arab.self,fr.self,fram.self,in.self,it.self,korea.self,sa.self,sp.self,us.self,linkedin.self)
all.self<-join_all(all.self,by=NULL,type="full") 
write.csv(all.self,"~/Desktop/Coding Projects/all.self.csv")

all.fam<-list(af.fam,arab.fam,fr.fam,fram.fam,in.fam,it.fam,korea.fam,sa.fam,sp.fam,us.family,linkedin.fam)
all.fam<-join_all(all.fam,by = NULL,type = "full")
write.csv(all.fam,"~/Desktop/Coding Projects/all.fam.csv")

####  RECODING ######

#DEMOGRAPHICS

#Age
all.self$age<-all.self$WhatisyouragePleaserespondwithanumber

#Male
all.self$male<-ifelse(all.self$Whatisyourgender=="Male",1,ifelse(all.self$Whatisyourgender  == "Female",0,NA))

#Urban
all.self$urban<-ifelse(all.self$Doyouliveinanurbanorruralneighborhood == "Urban",1,ifelse(all.self$Doyouliveinanurbanorruralneighborhood == "Rural",0,NA))

#Education
all.self$education<-case_when(
  all.self$Whatisthehighestlevelofeducationyouhaveacheived  == "No formal education" ~ "No education",
  all.self$Whatisthehighestlevelofeducationyouhaveacheived == "Primary" |
    all.self$Whatisthehighestlevelofeducationyouhaveacheived == "Up to 5th class"|
    all.self$Whatisthehighestlevelofeducationyouhaveacheived == "6th-8th Class" |
  all.self$Whatisthehighestlevelofeducationyouhaveacheived ==  "Pre-Primary" ~ "Primary",
  all.self$Whatisthehighestlevelofeducationyouhaveacheived ==  "Secondary" |
    all.self$Whatisthehighestlevelofeducationyouhaveacheived == "9th-10th Class"|
    all.self$Whatisthehighestlevelofeducationyouhaveacheived == "11th-12th Class"|
    all.self$Whatisthehighestlevelofeducationyouhaveacheived == "Pre-High School"|
    all.self$Whatisthehighestlevelofeducationyouhaveacheived == "High School Graduate"|
    all.self$Whatisthehighestlevelofeducationyouhaveacheived == "High School Graduate" ~ "Secondary",
  all.self$Whatisthehighestlevelofeducationyouhaveacheived == "Some College"|
    all.self$Whatisthehighestlevelofeducationyouhaveacheived == "Some College"|
    all.self$Whatisthehighestlevelofeducationyouhaveacheived == "Bachelor's Degree"|
    all.self$Whatisthehighestlevelofeducationyouhaveacheived=="Bachelor's Degree"|
    all.self$Whatisthehighestlevelofeducationyouhaveacheived == "Associate's Degree"|
    all.self$Whatisthehighestlevelofeducationyouhaveacheived == "Associate's Degree"|
    all.self$Whatisthehighestlevelofeducationyouhaveacheived== "College or Higher"|
    all.self$Whatisthehighestlevelofeducationyouhaveacheived  == "Tertiary (University, College, Trade School)"|
    all.self$Whatisthehighestlevelofeducationyouhaveacheived == "Higher than tertiary"|
    all.self$Whatisthehighestlevelofeducationyouhaveacheived == "Masters or Higher"|
    all.self$Whatisthehighestlevelofeducationyouhaveacheived  == "Masters or Higher" ~ "Tertiary")
all.self$education<-as.factor(all.self$education)

#Preexisting conditions
all.self$cardiovascular<-ifelse(all.self$HaveyoueverbeendiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyCardiovasculardiseaseheartattackstrokecoronarystentplacement == "Yes",1,ifelse(all.self$HaveyoueverbeendiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyCardiovasculardiseaseheartattackstrokecoronarystentplacement == "No",0,NA))
  all.self$copd<-ifelse(all.self$HaveyoueverbeendiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyChronicobstructivelungdiseaseCOPD == "Yes",1,ifelse(all.self$HaveyoueverbeendiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyChronicobstructivelungdiseaseCOPD  == "No",0,NA))
  all.self$diabetes<-ifelse(all.self$HaveyoueverbeendiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyDiabetesMellitus == "Yes",1,ifelse(all.self$HaveyoueverbeendiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyDiabetesMellitus == "No",0,NA))
  all.self$prostate<-ifelse(all.self$HaveyoueverbeendiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyEnlargedprostateBenignProstateHyperplasiaBPH == "Yes",1,ifelse(all.self$HaveyoueverbeendiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyEnlargedprostateBenignProstateHyperplasiaBPH == "No",0,NA))
  all.self$highbp<-ifelse(all.self$HaveyoueverbeendiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyHighbloodpressure == "Yes",1,ifelse(all.self$HaveyoueverbeendiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyHighbloodpressure == "No",0,NA))
  all.self$preexisting.yes<-ifelse(all.self$cardiovascular == 1,1,ifelse(all.self$diabetes==1,1,ifelse(all.self$copd==1,1,ifelse(all.self$prostate==1,1,ifelse(all.self$highbp==1,1,0)))))



# ALPHA BLOCKERS
all.self$alpha.yes<-case_when(
  all.self$AreyoucurrentlytakinganyofthefollowingAlpha1BlockermedicationsSelectallthatapplyAlfuzosinUroxatralXatralAlfusin == "Yes"|
    all.self$AreyoucurrentlytakinganyofthefollowingAlpha1BlockermedicationsSelectallthatapplyDoxazosinCarduraDoxacard=="Yes"|
    all.self$AreyoucurrentlytakinganyofthefollowingAlpha1BlockermedicationsSelectallthatapplyPrazosinMinipressMinizidePrazopress == "Yes"|
    all.self$AreyoucurrentlytakinganyofthefollowingAlpha1BlockermedicationsSelectallthatapplyTamsulosinFlomaxJalynUrimax == "Yes"|
    all.self$AreyoucurrentlytakinganyofthefollowingAlpha1BlockermedicationsSelectallthatapplyTerazosinHytrinOlysterTeralfaZytrin == "Yes"|
    all.self$AreyoucurrentlytakinganyofthefollowingAlpha1BlockermedicationsSelectallthatapplyIdonttakeanyofthesemedications == "No" |
  all.self$AreyoucurrentlytakinganyofthefollowingAlpha1BlockermedicationsSelectallthatapplyCarvedilolCoregHypertenevideCardivas == "Yes" ~ 1,
  all.self$AreyoucurrentlytakinganyofthefollowingAlpha1BlockermedicationsSelectallthatapplyAlfuzosinUroxatralXatralAlfusin == "No"|
  all.self$AreyoucurrentlytakinganyofthefollowingAlpha1BlockermedicationsSelectallthatapplyCarvedilolCoregHypertenevideCardivas == "No" |
  all.self$AreyoucurrentlytakinganyofthefollowingAlpha1BlockermedicationsSelectallthatapplyDoxazosinCarduraDoxacard == "No"|
  all.self$AreyoucurrentlytakinganyofthefollowingAlpha1BlockermedicationsSelectallthatapplyPrazosinMinipressMinizidePrazopress == "No" |
  all.self$AreyoucurrentlytakinganyofthefollowingAlpha1BlockermedicationsSelectallthatapplyTamsulosinFlomaxJalynUrimax == "No" |
  all.self$AreyoucurrentlytakinganyofthefollowingAlpha1BlockermedicationsSelectallthatapplyTerazosinHytrinOlysterTeralfaZytrin == "No" ~ 0)


# OUTCOME VARIABLES

#Belief
all.self$belief<-case_when(
  all.self$DoyoubelievethatyouhavecontractedCOVID19 == "Mostly likely yes"|
    all.self$DoyoubelievethatyouhavecontractedCOVID19 == "Yes"|
    all.self$DoyoubelievethatyouhavecontractedCOVID19 == "Most likely yes"|
    all.self$DoyoubelievethatyouhavecontractedCOVID19 == "Most likely yes" ~ 1,
  all.self$DoyoubelievethatyouhavecontractedCOVID19 == "Most likely no"|
    all.self$DoyoubelievethatyouhavecontractedCOVID19 == "Most likely no"|
    all.self$DoyoubelievethatyouhavecontractedCOVID19 == "No" ~ 0,
  all.self$DoyoubelievethatyouhavecontractedCOVID19 =="Unclear"|
    all.self$DoyoubelievethatyouhavecontractedCOVID19 == "I don't know"~ 0)

#Diagnosis
all.self$diagnosis.yes<-ifelse(all.self$HaveyoubeenofficiallydiagnosedwithCOVID19byahealthcareprofessional == "Yes",1,ifelse(all.self$HaveyoubeenofficiallydiagnosedwithCOVID19byahealthcareprofessional == "No",0,NA))

#See health professional
all.self$see.health.professional<-ifelse(all.self$Didyouseeahealthprofessionalforthesesymptoms == "Yes",1,ifelse(all.self$Didyouseeahealthprofessionalforthesesymptoms == "No",0,NA))
table(all.self$see.health.professional)

#################
###  ANALYSIS ###
#################
invlogit<-plogis

#Belief
model_belief_self<-stan_glm(belief~ alpha.yes + age + male + education + urban+ preexisting.yes + male*preexisting.yes,family=binomial(link = "logit"),data = all.self)
print(model_belief_self)
plot(model_belief_self)

#Actual diagnosis
model_diagnosis_self<-stan_glm(diagnosis.yes~alpha.yes + age + male + education + urban + preexisting.yes+male*preexisting.yes,family = binomial(link = "logit"),data = all.self)
print(model_diagnosis_self)
plot(model_diagnosis_self)

jitter_binary<-function(a,jitt=0.05){
  ifelse(a==0,runif(length(a),0,jitt),runif(length(a),1-jitt,1))}

all.self$alpha.yes.jitter<-jitter_binary(all.self$alpha.yes)
all.self$diagnosis.yes.jitter<-jitter_binary(all.self$diagnosis.yes)
par(mfrow=c(1,2))
plot(all.self$alpha.yes.jitter,all.self$diagnosis.yes.jitter,
     main = "Alpha Blocker Status \n v. COVID-19 Diagnosis",
     ylab="Probability of COVID-19 Diagnosis",
     xlab="Alpha Blocker Status",
     cex.main=1,
     cex=.25,
     bty="l",
     xaxt="n",
     yaxt="n")
xtick<-seq(0,1,by=1)
axis(side=1,at=xtick,labels = FALSE)
text(x=xtick,par("usr")[3],
     labels = xtick,pos = 1,xpd=TRUE)
ytick<-seq(0,1,by=.25)
axis(side = 2,at=ytick,labels = FALSE)
text(par("usr")[1],ytick,labels = ytick,pos = 2,xpd=TRUE)
curve(invlogit(coef(model_diagnosis_self)[1]+coef(model_diagnosis_self)[2]*x),add=TRUE,col="red")

all.self$belief.jitter<-jitter_binary(all.self$belief)
plot(all.self$alpha.yes.jitter,all.self$belief.jitter,
     main = "Alpha Blocker Status \n v. COVID-19 Belief",
     ylab="Probability of Believed COVID-19 Case",
     xlab="Alpha Blocker Status",
     cex.main=1,
     cex=.25,
     bty="l",
     xaxt="n",
     yaxt="n")
xtick<-seq(0,1,by=1)
axis(side=1,at=xtick,labels = FALSE)
text(x=xtick,par("usr")[3],
     labels = xtick,pos = 1,xpd=TRUE)
ytick<-seq(0,1,by=.25)
axis(side = 2,at=ytick,labels = FALSE)
text(par("usr")[1],ytick,labels = ytick,pos = 2,xpd=TRUE)
curve(invlogit(coef(model_belief_self)[1]+coef(model_belief_self)[2]*x),add = TRUE,col="red")
#### FAMILY DATASET  ######

#DEMOGRAPHICS

#Age
all.fam$age<-all.fam$WhatiswastheiragePleaserespondwithanumber

#Male
all.fam$male<-ifelse(all.fam$Whatiswastheirgender=="Male",1,ifelse(all.fam$Whatiswastheirgender  == "Female",0,NA))

#Urban
all.fam$urban<-ifelse(all.fam$Dodidtheyliveinanurbanorruralneighborhood == "Urban",1,ifelse(all.fam$Dodidtheyliveinanurbanorruralneighborhood == "Rural",0,NA))

#Education
all.fam$education<-case_when(
  all.fam$Whatisthehighestlevelofeducationtheyhaveacheived  == "No formal education" ~ "No education",
  all.fam$Whatisthehighestlevelofeducationtheyhaveacheived == "Primary" |
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived == "Up to 5th class"|
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived == "6th-8th Class" |
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived ==  "Pre-Primary" ~ "Primary",
  all.fam$Whatisthehighestlevelofeducationtheyhaveacheived ==  "Secondary" |
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived == "9th-10th Class"|
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived == "11th-12th Class"|
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived == "Pre-High School"|
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived == "High School Graduate"|
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived == "High School Graduate" ~ "Secondary",
  all.fam$Whatisthehighestlevelofeducationtheyhaveacheived == "Some College"|
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived == "Some College"|
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived == "Bachelor's Degree"|
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived=="Bachelor's Degree"|
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived == "Associate's Degree"|
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived == "Associate's Degree"|
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived== "College or Higher"|
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived  == "Tertiary (University, College, Trade School)"|
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived == "Higher than tertiary"|
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived == "Masters or Higher"|
    all.fam$Whatisthehighestlevelofeducationtheyhaveacheived  == "Masters or Higher" ~ "Tertiary")
all.fam$education<-as.factor(all.fam$education)

#Preexisting conditions
all.fam$cardiovascular<-ifelse(all.fam$AreWeretheyeverdiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyCardiovasculardiseaseheartattackstrokecoronarystentplacement == "Yes",1,ifelse(all.fam$AreWeretheyeverdiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyCardiovasculardiseaseheartattackstrokecoronarystentplacement== "No",0,NA))

all.fam$copd<-ifelse(all.fam$AreWeretheyeverdiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyChronicobstructivelungdiseaseCOPD == "Yes",1,ifelse(all.fam$AreWeretheyeverdiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyChronicobstructivelungdiseaseCOPD == "No",0,NA))

all.fam$diabetes<-ifelse(all.fam$AreWeretheyeverdiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyDiabetesMellitus == "Yes",1,ifelse(all.fam$AreWeretheyeverdiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyDiabetesMellitus == "No",0,NA))

all.fam$highbp<-ifelse(all.fam$AreWeretheyeverdiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyHighbloodpressure == "Yes",1,ifelse(all.fam$AreWeretheyeverdiagnosedwithanyofthebelowmedicalconditionsSelectallthatapplyHighbloodpressure == "No",0,NA))

all.fam$preexisting.yes<-ifelse(all.fam$cardiovascular == 1,1,ifelse(all.fam$diabetes==1,1,ifelse(all.fam$copd==1,1,ifelse(all.fam$highbp==1,1,0))))


# ALPHA BLOCKERS
all.fam$alpha.yes<-case_when(
  all.fam$TothebestofyourknowledgeareweretheytakinganyofthefollowingAlpha1BlockermedicationsSelectallthatapplyTheydonttakeanyofthesemedications == "Yes" ~ 1,
  all.fam$TothebestofyourknowledgeareweretheytakinganyofthefollowingAlpha1BlockermedicationsSelectallthatapplyTheydonttakeanyofthesemedications == "No" ~ 0)
table(all.fam$alpha.yes)

# OUTCOME VARIABLES

#Belief
all.fam$belief<-case_when(
  all.fam$DoyoubelievethattheycontractedCOVID19 == "Most likely yes"|
    all.fam$DoyoubelievethattheycontractedCOVID19 == "Yes" ~ 1,
  all.fam$DoyoubelievethattheycontractedCOVID19 ==  "Most likely no"|
    all.fam$DoyoubelievethattheycontractedCOVID19 == "No"|
    all.fam$DoyoubelievethattheycontractedCOVID19 == "Unclear" ~ 0)
table(all.fam$belief)

#Diagnosis
all.fam$diagnosis.yes<-ifelse(all.fam$WeretheyofficiallydiagnosedwithCOVID19byahealthcareprofessional == "Yes",1,ifelse(all.self$HaveyoubeenofficiallydiagnosedwithCOVID19byahealthcareprofessional == "No",0,NA))

# Hospital
all.fam$condition<-case_when(
  all.fam$Howseriousistheircurrentcondition == "Have passed away" ~ 4,
  all.fam$Howseriousistheircurrentcondition == "They are in critical care in the hospital" ~3,
  all.fam$Howseriousistheircurrentcondition == "They are in the hospital, but not in critical care"~2,
  all.fam$Howseriousistheircurrentcondition == "They feel sick, but are not in the hospital" ~ 1)
table(all.fam$condition)

#####################
##### ANALYSIS  ######
######################

#Linear-condition
model_condition_fam<-stan_glm(condition~alpha.yes+age+male+education+urban+preexisting.yes+male*preexisting.yes,data = all.fam)
print(model_condition_fam)

# Logit- belief
model_belief_fam<-stan_glm(belief~alpha.yes+age+male+education+urban+preexisting.yes+preexisting.yes*male,data = all.fam,family = binomial(link = "logit"))
print(model_belief_fam)

#Logit-diagnosis
model_diagnosis_fam<-stan_glm(diagnosis.yes~alpha.yes+age+male+education+urban+preexisting.yes+preexisting.yes*male,data = all.fam,family = binomial(link = "logit"))
print(model_diagnosis_fam)

# Plots
invlogit<-plogis

par(mfrow=c(1,3))
plot(jitter(all.fam$alpha.yes),jitter(all.fam$condition,.5),
     main = "Linear Regression of Alpha Blockers  \n and Family Condition",ylab = "COVID-19 Condition",
     xlab = "Alpha Blocker Status",
     cex.main=.95,
     xaxt="n",yaxt="n",
     bty="l")
xtick<-seq(0,1,by=1)
axis(side=1,at=xtick,labels = FALSE)
text(x=xtick,par("usr")[3],
     labels = xtick,pos = 1,xpd=TRUE)
ytick<-seq(0,4,by=1)
axis(side = 2,at=ytick,labels = FALSE)
text(par("usr")[1],ytick,labels = ytick,pos = 2,xpd=TRUE)
abline(lm(all.fam$condition~all.fam$alpha.yes),col="red")

plot(jitter(all.fam$alpha.yes),jitter(all.fam$diagnosis.yes,.5),main = "Alpha Blockers and Family COVID-19 Diagnosis",ylab="Probability of Having COVID-19",xlab="Alpha Blocker Status",cex.main=.95,xaxt="n",yaxt="n",bty="l")
xtick<-seq(0,1,by=1)
axis(side=1,at=xtick,labels = FALSE)
text(x=xtick,par("usr")[3],
     labels = xtick,pos = 1,xpd=TRUE)
ytick<-seq(0,1,by=.25)
axis(side = 2,at=ytick,labels = FALSE)
text(par("usr")[1],ytick,labels = ytick,pos = 2,xpd=TRUE)
curve(invlogit(coef(model_diagnosis_fam)[1]+coef(model_diagnosis_fam)[2]*x),add=TRUE,col="red")

plot(jitter(all.fam$alpha.yes),jitter(all.fam$belief,.5),main = "Alpha Blockers and  \n Believed Family COVID-19 Case",
     ylab="Probability of Having COVID-19",
     xlab="Alpha Blocker Status",
     cex.main=.95,
     xaxt="n",yaxt="n",
     bty="l")
xtick<-seq(0,1,by=1)
axis(side=1,at=xtick,labels = FALSE)
text(x=xtick,par("usr")[3],
     labels = xtick,pos = 1,xpd=TRUE)
ytick<-seq(0,1,by=.25)
axis(side = 2,at=ytick,labels = FALSE)
text(par("usr")[1],ytick,labels = ytick,pos = 2,xpd=TRUE)
curve(invlogit(coef(model_belief_fam)[1]+coef(model_belief_fam)[2]*x),add = TRUE,col="red")

plot(jitter(all.fam$alpha.yes),jitter(all.fam$belief,.5),main = "Alpha Blockers and  \n Believed Family COVID-19 Case",
     ylab="Probability of Having COVID-19",
     xlab="Alpha Blocker Status",
     cex.main=.95,
     xaxt="n",yaxt="n",
     bty="l")
xtick<-seq(0,1,by=1)
axis(side=1,at=xtick,labels = FALSE)
text(x=xtick,par("usr")[3],
     labels = xtick,pos = 1,xpd=TRUE)
ytick<-seq(0,1,by=.25)
axis(side = 2,at=ytick,labels = FALSE)
text(par("usr")[1],ytick,labels = ytick,pos = 2,xpd=TRUE)


#Coefficient plots 
par(mfrow=c(1,3))
plot(model_belief_fam)
plot(model_condition_fam)
plot(model_diagnosis_fam)
