setwd("C:/Users/Nipun/Desktop/Harrisburg/R_Default_Directory")

crime=read.csv("C:/Users/Nipun/Desktop/Harrisburg/R_Default_Directory/crimedata.csv",header = FALSE, sep = ",")

##Input name

nonPredVars <- c("communityname","state","countyCode","communityCode","fold")

predVars <- c("population","householdsize","racepctblack","racePctWhite","racePctAsian","racePctHisp",
              "agePct12t21","agePct12t29","agePct16t24","agePct65up","numbUrban","pctUrban","medIncome",
              "pctWWage","pctWFarmSelf","pctWInvInc","pctWSocSec","pctWPubAsst","pctWRetire","medFamInc",
              "perCapInc","whitePerCap","blackPerCap","indianPerCap","AsianPerCap","OtherPerCap","HispPerCap",
              "NumUnderPov","PctPopUnderPov","PctLess9thGrade","PctNotHSGrad","PctBSorMore","PctUnemployed",
              "PctEmploy","PctEmplManu","PctEmplProfServ","PctOccupManu","PctOccupMgmtProf","MalePctDivorce",
              "MalePctNevMarr","FemalePctDiv","TotalPctDiv","PersPerFam","PctFam2Par","PctKids2Par","PctYoungKids2Pa                 r","PctTeen2Par","PctWorkMomYoungKids","PctWorkMom","NumKidsBornNeverMar","PctKidsBornNeverMar",
              "NumImmig","PctImmigRecent","PctImmigRec5","PctImmigRec8","PctImmigRec10","PctRecentImmig",
              "PctRecImmig5","PctRecImmig8","PctRecImmig10","PctSpeakEnglOnly","PctNotSpeakEnglWell",
              "PctLargHouseFam","PctLargHouseOccup","PersPerOccupHous","PersPerOwnOccHous","PersPerRentOccHous",
              "PctPersOwnOccup","PctPersDenseHous","PctHousLess3BR","MedNumBR","HousVacant","PctHousOccup",
              "PctHousOwnOcc","PctVacantBoarded","PctVacMore6Mos","MedYrHousBuilt","PctHousNoPhone",
              "PctWOFullPlumb","OwnOccLowQuart","OwnOccMedVal","OwnOccHiQuart","OwnOccQrange","RentLowQ",
              "RentMedian","RentHighQ","RentQrange","MedRent","MedRentPctHousInc","MedOwnCostPctInc",
              "MedOwnCostPctIncNoMtg","NumInShelters","NumStreet","PctForeignBorn","PctBornSameState",
              "PctSameHouse85","PctSameCity85","PctSameState85","LemasSwornFT","LemasSwFTPerPop","LemasSwFTFieldOps",
              "LemasSwFTFieldPerPop","LemasTotalReq","LemasTotReqPerPop","PolicReqPerOffic","PolicPerPop",
              "RacialMatchCommPol","PctPolicWhite","PctPolicBlack","PctPolicHisp","PctPolicAsian","PctPolicMinor",
              "OfficAssgnDrugUnits","NumKindsDrugsSeiz","PolicAveOTWorked","LandArea","PopDens","PctUsePubTrans",
              "PolicCars","PolicOperBudg","LemasPctPolicOnPatr","LemasGangUnitDeploy","LemasPctOfficDrugUn",
              "PolicBudgPerPop")
goalVars <- c("murders","murdPerPop","rapes","rapesPerPop",
              "robberies","robbbPerPop","assaults","assaultPerPop",
              "burglaries","burglPerPop","larcenies","larcPerPop",
              "autoTheft","autoTheftPerPop","arsons","arsonsPerPop",
              "ViolentCrimesPerPop",
              "nonViolPerPop"
)
colnames(crime) <- c(nonPredVars, predVars, goalVars)
crime_subset<-ddply(crime, .(state), summarize,  MurderPct=sum(crime$murders)/sum(crime$population), 
      RapesPct=sum(crime$rapes)/sum(crime$population),
      RobberyPct=sum(crime$robberies)/sum(crime$population),
      AssaultPct=sum(crime$assaults)/sum(crime$population),
      BurglariesPct=sum(crime$burglaries)/sum(crime$population),
      LarcenyPct=sum(crime$larcenies)/sum(crime$population),
      AutotheftPct=sum(crime$autoTheft)/sum(crime$population),
      ArsnPct=sum(crime$arsons)/sum(crime$population),
      ViolencePct=sum(crime$ViolentCrimesPerPop)/sum(crime$population),
      NonViolencePct=sum(crime$nonViolPerPop)/sum(crime$population))
crime_subset
names(crime)
head(crime)

crime$arsonsPerPop<-as.numeric(crime$arsonsPerPop)
crime$arsons<-as.numeric(crime$arsons)
crime$autoTheftPerPop<-as.numeric(crime$autoTheftPerPop)
crime$autoTheft<-as.numeric(crime$autoTheft)
crime$larcPerPop<-as.numeric(crime$larcPerPop)
crime$larcenies<-as.numeric(crime$larcenies)
crime$burglPerPop<-as.numeric(crime$burglPerPop)
crime$burglaries<-as.numeric(crime$burglaries)
crime$assaultPerPop<-as.numeric(crime$assaultPerPop)
crime$assaults<-as.numeric(crime$assaults)
crime$robbbPerPop<-as.numeric(crime$robbbPerPop)
crime$robberies<-as.numeric(crime$robberies)
crime$rapesPerPop<-as.numeric(crime$rapesPerPop)
crime$rapes<-as.numeric(crime$rapes)
crime$murdPerPop<-as.numeric(crime$murdPerPop)
crime$murders<-as.numeric(crime$murders)
crime$population<-as.numeric(crime$population)
crime$nonViolPerPop<-as.numeric(crime$nonViolPerPop)
crime$ViolentCrimesPerPop<-as.numeric(crime$ViolentCrimesPerPop)

crime$PctLess9thGrade<-as.numeric(crime$PctLess9thGrade)
crime$PctBSorMore<-as.numeric(crime$PctBSorMore)
crime$PctNotHSGrad<-as.numeric(crime$PctNotHSGrad)
crime$PctUnemployed<-as.numeric(crime$PctUnemployed)
crime$PctEmploy<-as.numeric(crime$PctEmploy)
crime$PctEmplManu<-as.numeric(crime$PctEmplManu)
crime$PctEmplProfServ<-as.numeric(crime$PctEmplProfServ)
crime$PctOccupManu<-as.numeric(crime$PctOccupManu)
crime$PctOccupMgmtProf<-as.numeric(crime$PctOccupMgmtProf)
subvariables= c("communityname","murdPerPop","rapesPerPop",
                "robbbPerPop","assaultPerPop",
                "burglPerPop","larcPerPop",
                "autoTheftPerPop","arsonsPerPop",
                "ViolentCrimesPerPop",
                "nonViolPerPop",
                "PctLess9thGrade", "PctBSorMore","PctNotHSGrad",
"PctUnemployed","PctEmploy","PctEmplManu","PctEmplProfServ","PctOccupManu","PctOccupMgmtProf")
c_sub1 = crime[subvariables]
c_sub2 = na.omit(c_sub1)
c_pca_scale = scale(c_sub2)
pca_results = prcomp(c_pca_scale)
summary(pca_results)
head(pca_results$x[,1:18])
subvariables= c("communityname",
                "ViolentCrimesPerPop",
                "PctLess9thGrade", "PctBSorMore","PctNotHSGrad",
                "PctUnemployed")
c_sub1 = crime[subvariables]
c_sub2 = na.omit(c_sub1)
set.seed(123)
km.res <- kmeans(c_sub2[,2:6], 6, nstart = 25)

km.res
plot(km.res, data = c_sub2)
plot(c_sub2[c("ViolentCrimesPerPop", "PctUnemployed")], col = km.res$cluster)
points(km.res$centers[,c("ViolentCrimesPerPop", "PctUnemployed")], col = 1:3, pch = 8, cex = 2)
plot(c_sub2[c("ViolentCrimesPerPop", "PctNotHSGrad")], col = km.res$cluster)
points(km.res$centers[,c("ViolentCrimesPerPop", "PctNotHSGrad")], col = 1:3, pch = 8, cex = 2)