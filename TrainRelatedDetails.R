TrainPredictionStatusCall <- function(fileinput){
    A<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H" | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" ) & fileinput$ACTUAL_STATUS=="Y")), ]
    B<-fileinput[which((fileinput$TRAIN_ASPU_STATUS=="H"  & fileinput$ACTUAL_STATUS=="Y")), ]
    C<-fileinput[which((fileinput$TRAIN_ASPU_STATUS=="M"  & fileinput$ACTUAL_STATUS=="Y")), ]
    D<-fileinput[which((fileinput$TRAIN_ASPU_STATUS=="L"  & fileinput$ACTUAL_STATUS=="Y")), ]
    A1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H"  | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" )  & (fileinput$TRAINPREDICTED=="Y" |fileinput$TRAINPREDICTED=="N"))),]
    B1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H")  & (fileinput$TRAINPREDICTED=="Y" |fileinput$TRAINPREDICTED=="N"))),]
    C1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="M")  & (fileinput$TRAINPREDICTED=="Y" |fileinput$TRAINPREDICTED=="N"))),]
    D1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="L")  & (fileinput$TRAINPREDICTED=="Y" |fileinput$TRAINPREDICTED=="N"))),]
    A2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H"  | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" )  & (fileinput$TRAINPREDICTED=="Y"))),] 
    B2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H")  & (fileinput$TRAINPREDICTED=="Y"))),] 
    C2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="M")  & (fileinput$TRAINPREDICTED=="Y"))),] 
    D2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="L")  & (fileinput$TRAINPREDICTED=="Y"))),] 
    A3<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H"  | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" )  & (fileinput$TRAINPREDICTED=="Y") & (fileinput$ACTUAL_STATUS=="Y"))),]  
    B3<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H")  & (fileinput$TRAINPREDICTED=="Y") & (fileinput$ACTUAL_STATUS=="Y"))),]  
    C3<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="M")  & (fileinput$TRAINPREDICTED=="Y") & (fileinput$ACTUAL_STATUS=="Y"))),]  
    D3<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="L")  & (fileinput$TRAINPREDICTED=="Y") & (fileinput$ACTUAL_STATUS=="Y"))),]  
    PredictionStatus = data.frame(PredictionStatus=c("Total Active Base Count","Base Count considered for Model Building (Y/N)","Model Predicted - Non User count","Model Post Validation - Non User count"), 
                                  TotalTrainingSet=c(nrow(A), nrow(A1), nrow(A2),nrow(A3)),
                                  High=c(nrow(B), nrow(B1), nrow(B2), nrow(B3)),
                                  Medium=c(nrow(C),nrow(C1),nrow(C2), nrow(C3)),
                                  Low=c(nrow(D),nrow(D1),nrow(D2),nrow(D3)))
    return(PredictionStatus)
  }
  
  TrainModelPredictionSummaryOverall<- function(fileinput){
    A<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H" | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" ) & (fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="Y"))), ]
    B<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H"  | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" )  & (fileinput$TRAINPREDICTED=="N")& (fileinput$ACTUAL_STATUS=="Y"))),]
    C<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H"  | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" )  & (fileinput$ACTUAL_STATUS=="Y"))),] 
    D<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H"  | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" )  & (fileinput$TRAINPREDICTED=="Y" |fileinput$TRAINPREDICTED=="N" ) & (fileinput$ACTUAL_STATUS=="Y"))),]  
    A1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H" | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" ) & (fileinput$ACTUAL_STATUS=="N") & (fileinput$TRAINPREDICTED=="Y"))), ]
    B1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H" | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" ) & (fileinput$ACTUAL_STATUS=="N") & (fileinput$TRAINPREDICTED=="N"))), ]
    C1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H" | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" ) & (fileinput$ACTUAL_STATUS=="N"))), ]
    D1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H" | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" ) & (fileinput$ACTUAL_STATUS=="N") &(fileinput$TRAINPREDICTED=="Y" | fileinput$TRAINPREDICTED=="N"))), ]
    A2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H" | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" ) & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="Y"))), ]
    B2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H" | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" ) & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="N"))), ]
    C2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H" | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" ) & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y"))), ]
    D2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H" | fileinput$TRAIN_ASPU_STATUS=="M"|fileinput$TRAIN_ASPU_STATUS=="L" ) & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="Y" | fileinput$TRAINPREDICTED=="N"))), ]
    ModelPredictionSummary = data.frame(ModelPredictionSummaryOverall=c("ActualNon User","ActualUser","GrandTotal"), 
                                        PredictedNonUser=c(nrow(A), nrow(A1), nrow(A2)),
                                        PredictedUser=c(nrow(B), nrow(B1), nrow(B2)),
                                        ActualChurners=c(nrow(C),nrow(C1),nrow(C2)),
                                        GrandTotal=c(nrow(D),nrow(D1),nrow(D2)))
    return(ModelPredictionSummary)
  }
  
  
  TrainModelPredictionSummaryHigh<- function(fileinput){
    A<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H") & (fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="Y"))), ]
    B<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H")  & (fileinput$TRAINPREDICTED=="N")& (fileinput$ACTUAL_STATUS=="Y"))),]
    C<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H")  & (fileinput$ACTUAL_STATUS=="Y"))),] 
    D<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H")  & (fileinput$TRAINPREDICTED=="Y" |fileinput$TRAINPREDICTED=="N" ) & (fileinput$ACTUAL_STATUS=="Y"))),]  
    A1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H") & (fileinput$ACTUAL_STATUS=="N") & (fileinput$TRAINPREDICTED=="Y"))), ]
    B1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H") & (fileinput$ACTUAL_STATUS=="N") & (fileinput$TRAINPREDICTED=="N"))), ]
    C1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H") & (fileinput$ACTUAL_STATUS=="N"))), ]
    D1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H") & (fileinput$ACTUAL_STATUS=="N") &(fileinput$TRAINPREDICTED=="Y" | fileinput$TRAINPREDICTED=="N"))), ]
    A2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H") & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="Y"))), ]
    B2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H") & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="N"))), ]
    C2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H") & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y"))), ]
    D2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="H") & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="Y" | fileinput$TRAINPREDICTED=="N"))), ]
    ModelPredictionSummaryH = data.frame(ModelPredictionSummaryH=c("ActualNon User","ActualUser","GrandTotal"), 
                                         PredictedNonUser=c(nrow(A), nrow(A1), nrow(A2)),
                                         PredictedUser=c(nrow(B), nrow(B1), nrow(B2)),
                                         ActualChurners=c(nrow(C),nrow(C1),nrow(C2)),
                                         GrandTotal=c(nrow(D),nrow(D1),nrow(D2)))
    return(ModelPredictionSummaryH)
  }
  
  TrainModelPredictionSummaryMedium<- function(fileinput){
    A<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="M") & (fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="Y"))), ]
    B<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="M")  & (fileinput$TRAINPREDICTED=="N")& (fileinput$ACTUAL_STATUS=="Y"))),]
    C<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="M")  & (fileinput$ACTUAL_STATUS=="Y"))),] 
    D<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="M")  & (fileinput$TRAINPREDICTED=="Y" |fileinput$TRAINPREDICTED=="N" ) & (fileinput$ACTUAL_STATUS=="Y"))),]  
    A1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="M") & (fileinput$ACTUAL_STATUS=="N") & (fileinput$TRAINPREDICTED=="Y"))), ]
    B1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="M") & (fileinput$ACTUAL_STATUS=="N") & (fileinput$TRAINPREDICTED=="N"))), ]
    C1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="M") & (fileinput$ACTUAL_STATUS=="N"))), ]
    D1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="M") & (fileinput$ACTUAL_STATUS=="N") &(fileinput$TRAINPREDICTED=="Y" | fileinput$TRAINPREDICTED=="N"))), ]
    A2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="M") & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="Y"))), ]
    B2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="M") & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="N"))), ]
    C2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="M") & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y"))), ]
    D2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="M") & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="Y" | fileinput$TRAINPREDICTED=="N"))), ]
    ModelPredictionSummaryM = data.frame(ModelPredictionSummaryM=c("ActualNon User","ActualUser","GrandTotal"), 
                                         PredictedNonUser=c(nrow(A), nrow(A1), nrow(A2)),
                                         PredictedUser=c(nrow(B), nrow(B1), nrow(B2)),
                                         ActualChurners=c(nrow(C),nrow(C1),nrow(C2)),
                                         GrandTotal=c(nrow(D),nrow(D1),nrow(D2)))
    return(ModelPredictionSummaryM)
  }
  
  TrainModelPredictionSummaryLow<- function(fileinput){
    A<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="L") & (fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="Y"))), ]
    B<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="L")  & (fileinput$TRAINPREDICTED=="N")& (fileinput$ACTUAL_STATUS=="Y"))),]
    C<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="L")  & (fileinput$ACTUAL_STATUS=="Y"))),] 
    D<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="L")  & (fileinput$TRAINPREDICTED=="Y" |fileinput$TRAINPREDICTED=="N" ) & (fileinput$ACTUAL_STATUS=="Y"))),]  
    A1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="L") & (fileinput$ACTUAL_STATUS=="N") & (fileinput$TRAINPREDICTED=="Y"))), ]
    B1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="L") & (fileinput$ACTUAL_STATUS=="N") & (fileinput$TRAINPREDICTED=="N"))), ]
    C1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="L") & (fileinput$ACTUAL_STATUS=="N"))), ]
    D1<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="L") & (fileinput$ACTUAL_STATUS=="N") &(fileinput$TRAINPREDICTED=="Y" | fileinput$TRAINPREDICTED=="N"))), ]
    A2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="L") & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="Y"))), ]
    B2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="L") & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="N"))), ]
    C2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="L") & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y"))), ]
    D2<-fileinput[which(((fileinput$TRAIN_ASPU_STATUS=="L") & (fileinput$ACTUAL_STATUS=="N" | fileinput$ACTUAL_STATUS=="Y") & (fileinput$TRAINPREDICTED=="Y" | fileinput$TRAINPREDICTED=="N"))), ]
    ModelPredictionSummaryL = data.frame(ModelPredictionSummaryL=c("ActualNon User","ActualUser","GrandTotal"), 
                                         PredictedNonUser=c(nrow(A), nrow(A1), nrow(A2)),
                                         PredictedUser=c(nrow(B), nrow(B1), nrow(B2)),
                                         ActualChurners=c(nrow(C),nrow(C1),nrow(C2)),
                                         GrandTotal=c(nrow(D),nrow(D1),nrow(D2)))
    return(ModelPredictionSummaryL)
  }
  
  
  TrainCPIScore<- function(fileinput){
    CPIScore=sqldf("SELECT CASE WHEN TRAINCOC BETWEEN 0 AND 10 THEN '01-10' WHEN TRAINCOC BETWEEN 11 AND 20 THEN '11-20' WHEN TRAINCOC BETWEEN 21 AND 30 THEN '21-30' WHEN TRAINCOC BETWEEN 31 AND 40 THEN '31-40'  WHEN TRAINCOC BETWEEN 41 AND 50 THEN '41-50' WHEN TRAINCOC BETWEEN 51 AND 60 THEN '51-60'  WHEN TRAINCOC BETWEEN 61 AND 70 THEN '61-70' WHEN TRAINCOC BETWEEN 71 AND 80 THEN '71-80' WHEN TRAINCOC BETWEEN 81 AND 90 THEN '81-90' ELSE '91-100' END CPIScoreOverall, SUM(CASE WHEN TRAINPREDICTED = 'Y' THEN 1 ELSE 0 END) ModelPredictedNonUsercount, SUM(CASE WHEN ACTUAL_STATUS='Y' THEN 1 ELSE 0 END) ActualChurner, SUM(CASE WHEN TRAINPREDICTED = 'Y' AND ACTUAL_STATUS='Y' THEN 1 ELSE 0 END) ModelPostValidationAccuracy  FROM fileinput WHERE  TRAIN_ASPU_STATUS IN ('H','M','L') GROUP BY CPIScoreOverall ORDER BY CPIScoreOverall DESC");
    return(CPIScore)
  }
  
  TrainTariffPlan<- function(fileinput){
    TariffPlan=sqldf("SELECT M3M4M5_TARIFF_PLAN TariffPlan, SUM(CASE WHEN TRAINPREDICTED = 'Y' THEN 1 ELSE 0 END) ModelPredictedNonUsercount,SUM(CASE WHEN ACTUAL_STATUS='Y' THEN 1 ELSE 0 END)ActualChurners, SUM(CASE WHEN TRAINPREDICTED = 'Y' AND ACTUAL_STATUS='Y' THEN 1 ELSE 0 END) ModelPostValidationAccuracy FROM fileinput  WHERE  TRAIN_ASPU_STATUS IN ('H','M','L') GROUP BY M3M4M5_TARIFF_PLAN")
    return(TariffPlan)
  }
  
  TrainRegion<- function(fileinput){
    Region=sqldf("SELECT M3M4M5_REGION Province, SUM(CASE WHEN TRAINPREDICTED = 'Y' THEN 1 ELSE 0 END)ModelPredictedNonUsercount, SUM(CASE WHEN ACTUAL_STATUS='Y'  THEN 1 ELSE 0 END)ActualChurner, SUM(CASE WHEN TRAINPREDICTED='Y' AND ACTUAL_STATUS='Y'  THEN 1 ELSE 0 END)ModelPostValidationAccuracy FROM fileinput WHERE  TRAIN_ASPU_STATUS IN ('H','M','L') GROUP BY M3M4M5_REGION")
    return(Region)
  }

 TrainAon<- function(fileinput){
  TrainAon=sqldf("SELECT CASE WHEN AON_MONTHS BETWEEN 0 AND 3 THEN 'AON(0-3)' WHEN AON_MONTHS BETWEEN 4 AND 6 THEN 'AON(4-6)' WHEN AON_MONTHS BETWEEN 7 AND 12 THEN 'AON(7-12)'  WHEN AON_MONTHS BETWEEN 13 AND 21 THEN 'AON(13-21)' WHEN AON_MONTHS >21 THEN 'AON(>21)' ELSE 'NA' END AON , SUM(TotalPredicted) ModelPredictedNonUsercount,SUM(ActualChurners) ActualChurners,SUM(ModelPostValidationAccuracy) ModelPostValidationAccuracy FROM (SELECT M3_AON_MONTHS AON_MONTHS, SUM(CASE WHEN TRAINPREDICTED = 'Y' THEN 1 ELSE 0 END)TotalPredicted,SUM(CASE WHEN ACTUAL_STATUS= 'Y' THEN 1 ELSE 0 END)ActualChurners,SUM(CASE WHEN TRAINPREDICTED = 'Y' AND ACTUAL_STATUS='Y' THEN 1 ELSE 0 END)ModelPostValidationAccuracy FROM fileinput WHERE  TRAIN_ASPU_STATUS IN ('H','M','L') GROUP BY AON_MONTHS ORDER BY AON_MONTHS)A GROUP BY AON")
 return(TrainAon)
 }

  TrainValueSegmentation<- function(fileinput){
    ValueSegmentation=sqldf("SELECT M3M4M5_VALUE_SEG_STATUS STATUS, SUM(CASE WHEN TRAINPREDICTED = 'Y' THEN 1 ELSE 0 END)ModelPredictedNonUsercount,  SUM(CASE WHEN ACTUAL_STATUS='Y' THEN 1 ELSE 0 END)ActualChurners, SUM(CASE WHEN TRAINPREDICTED='Y' AND ACTUAL_STATUS='Y' THEN 1 ELSE 0 END)ModelPostValidationAccuracy FROM fileinput WHERE  TRAIN_ASPU_STATUS IN ('H','M','L')  GROUP BY M3M4M5_VALUE_SEG_STATUS;")
    return(ValueSegmentation)
  }
  