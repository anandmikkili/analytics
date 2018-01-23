TestPredictionStatusCall <- function(fileinput){
  TA<-fileinput[which(((fileinput$TEST_ASPU_STATUS=="H" | fileinput$TEST_ASPU_STATUS=="M"|fileinput$TEST_ASPU_STATUS=="L" ) & fileinput$ACTUAL_STATUS=="Y")), ]
  TB<-fileinput[which((fileinput$TEST_ASPU_STATUS=="H"  & fileinput$ACTUAL_STATUS=="Y")), ]
  TC<-fileinput[which((fileinput$TEST_ASPU_STATUS=="M"  & fileinput$ACTUAL_STATUS=="Y")), ]
  TD<-fileinput[which((fileinput$TEST_ASPU_STATUS=="L"  & fileinput$ACTUAL_STATUS=="Y")), ]
  TA2<-fileinput[which(((fileinput$TEST_ASPU_STATUS=="H"  | fileinput$TEST_ASPU_STATUS=="M"|fileinput$TEST_ASPU_STATUS=="L" )  & (fileinput$TESTPREDICTED=="Y"))),] 
  TB2<-fileinput[which(((fileinput$TEST_ASPU_STATUS=="H")  & (fileinput$TESTPREDICTED=="Y"))),] 
  TC2<-fileinput[which(((fileinput$TEST_ASPU_STATUS=="M")  & (fileinput$TESTPREDICTED=="Y"))),] 
  TD2<-fileinput[which(((fileinput$TEST_ASPU_STATUS=="L")  & (fileinput$TESTPREDICTED=="Y"))),] 
  TestPredictionStatus = data.frame(TestPredictionStatus=c("Total Active Base Count","Model Predicted - Non User count"), 
                                    TotalTestSet=c(nrow(TA),nrow(TA2)),
                                    High=c(nrow(TB),nrow(TB2)),
                                    Medium=c(nrow(TC),nrow(TC2)),
                                    Low=c(nrow(TD),nrow(TD2)))
  return(TestPredictionStatus)
}

TestCPIScore<- function(fileinput){
  TestCPIScore=sqldf("SELECT CASE WHEN TESTCOC BETWEEN 0 AND 10 THEN '01-10' WHEN TESTCOC BETWEEN 11 AND 20 THEN '11-20' WHEN TESTCOC BETWEEN 21 AND 30 THEN '21-30' WHEN TESTCOC BETWEEN 31 AND 40 THEN '31-40'  WHEN TESTCOC BETWEEN 41 AND 50 THEN '41-50' WHEN TESTCOC BETWEEN 51 AND 60 THEN '51-60'  WHEN TESTCOC BETWEEN 61 AND 70 THEN '61-70' WHEN TESTCOC BETWEEN 71 AND 80 THEN '71-80' WHEN TESTCOC BETWEEN 81 AND 90 THEN '81-90' ELSE '91-100' END CPIScoreOverall, SUM(CASE WHEN TESTPREDICTED = 'Y' THEN 1 ELSE 0 END) ModelPredictedNonUsercount, SUM(CASE WHEN ACTUAL_STATUS='Y' THEN 1 ELSE 0 END) ActualChurner, SUM(CASE WHEN TESTPREDICTED = 'Y' AND ACTUAL_STATUS='Y' THEN 1 ELSE 0 END) ModelPostValidationAccuracy  FROM fileinput WHERE  TEST_ASPU_STATUS IN ('H','M','L') GROUP BY CPIScoreOverall ORDER BY CPIScoreOverall DESC");
  return(TestCPIScore)
}

#TestAon<- function(fileinput){
#  TestAon=sqldf("")
 # return(TestAon)
#}

TestTariffPlan<- function(fileinput){
  TestTariffPlan=sqldf("SELECT M3M4M5_TARIFF_PLAN TestTariffPlan, SUM(CASE WHEN TESTPREDICTED = 'Y' THEN 1 ELSE 0 END) ModelPredictedNonUsercount,SUM(CASE WHEN ACTUAL_STATUS='Y' THEN 1 ELSE 0 END)ActualChurners, SUM(CASE WHEN TESTPREDICTED = 'Y' AND ACTUAL_STATUS='Y' THEN 1 ELSE 0 END) ModelPostValidationAccuracy FROM fileinput  WHERE  TEST_ASPU_STATUS IN ('H','M','L') GROUP BY M3M4M5_TARIFF_PLAN")
  return(TestTariffPlan)
}

TestRegion<- function(fileinput){
  TestRegion=sqldf("SELECT M3M4M5_REGION Province, SUM(CASE WHEN TESTPREDICTED = 'Y' THEN 1 ELSE 0 END)ModelPredictedNonUsercount, SUM(CASE WHEN ACTUAL_STATUS='Y'  THEN 1 ELSE 0 END)ActualChurner, SUM(CASE WHEN TESTPREDICTED='Y' AND ACTUAL_STATUS='Y'  THEN 1 ELSE 0 END)ModelPostValidationAccuracy FROM fileinput WHERE  TEST_ASPU_STATUS IN ('H','M','L') GROUP BY M3M4M5_REGION")
  return(TestRegion)
}

TestValueSegmentation<- function(fileinput){
  TestValueSegmentation=sqldf("SELECT M3M4M5_VALUE_SEG_STATUS STATUS, SUM(CASE WHEN TESTPREDICTED = 'Y' THEN 1 ELSE 0 END)ModelPredictedNonUsercount,  SUM(CASE WHEN ACTUAL_STATUS='Y' THEN 1 ELSE 0 END)ActualChurners, SUM(CASE WHEN TESTPREDICTED='Y' AND ACTUAL_STATUS='Y' THEN 1 ELSE 0 END)ModelPostValidationAccuracy FROM fileinput WHERE  TEST_ASPU_STATUS IN ('H','M','L')  GROUP BY M3M4M5_VALUE_SEG_STATUS")
  return(TestValueSegmentation)
}