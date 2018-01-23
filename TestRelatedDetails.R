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

TestTariffPlan<- function(fileinput){
  TestTariffPlan=sqldf("SELECT M1M2M3_TARIFF_PLAN TestTariffPlan, SUM(CASE WHEN TESTPREDICTED = 'Y' THEN 1 ELSE 0 END) ModelPredictedNonUsercount,SUM(CASE WHEN ACTUAL_STATUS='Y' THEN 1 ELSE 0 END)ActualChurners, SUM(CASE WHEN TESTPREDICTED = 'Y' AND ACTUAL_STATUS='Y' THEN 1 ELSE 0 END) ModelPostValidationAccuracy FROM fileinput  WHERE  TEST_ASPU_STATUS IN ('H','M','L') GROUP BY M1M2M3_TARIFF_PLAN;")
  return(TestTariffPlan)
}

TestRegion<- function(fileinput){
  TestRegion=sqldf("SELECT M1M2M3_REGION Province, SUM(CASE WHEN TESTPREDICTED = 'Y' THEN 1 ELSE 0 END)ModelPredictedNonUsercount, SUM(CASE WHEN ACTUAL_STATUS='Y'  THEN 1 ELSE 0 END)ActualChurner, SUM(CASE WHEN TESTPREDICTED='Y' AND ACTUAL_STATUS='Y'  THEN 1 ELSE 0 END)ModelPostValidationAccuracy FROM fileinput WHERE  TEST_ASPU_STATUS IN ('H','M','L') GROUP BY M1M2M3_REGION;")
  return(TestRegion)
}

TestValueSegmentation<- function(fileinput){
  TestValueSegmentation=sqldf("SELECT M1M2M3_VALUE_SEG_STATUS STATUS, SUM(CASE WHEN TESTPREDICTED = 'Y' THEN 1 ELSE 0 END)ModelPredictedNonUsercount,  SUM(CASE WHEN ACTUAL_STATUS='Y' THEN 1 ELSE 0 END)ActualChurners, SUM(CASE WHEN TESTPREDICTED='Y' AND ACTUAL_STATUS='Y' THEN 1 ELSE 0 END)ModelPostValidationAccuracy FROM fileinput WHERE  TEST_ASPU_STATUS IN ('H','M','L')  GROUP BY M1M2M3_VALUE_SEG_STATUS;")
  return(TestValueSegmentation)
}

TestAON<- function(fileinput){
  TestAON=sqldf("select case when AON_MONTHS between 0 and 3 then 'AON(0-3)' when AON_MONTHS between 4 and 6 then 'AON(4-6)'  when AON_MONTHS between 7 and 12 then 'AON(7-12)'  when AON_MONTHS between 13 and 24 then 'AON(13-24)' when AON_MONTHS >24 then 'AON(>24)' else 'NA' end AON , case when AON_MONTHS between 0 and 3 then '1' when AON_MONTHS between 4 and 6 then '2'  when AON_MONTHS between 7 and 12 then '3'  when AON_MONTHS between 13 and 24 then '4'  when AON_MONTHS >24 then '5' else '6' end AON_O , SUM(TotalPredicted) TotalPredicted,SUM(ActualChurners) ActualChurners, SUM(MatchingCount) MatchingCount,(SUM(MatchingCount)/SUM(TotalPredicted))*100 MatchingPercentage FROM (SELECT M3_AON_MONTHS AON_MONTHS, sum(case when TRAINPREDICTED = 'Y' then 1 else 0 end)TotalPredicted,sum(case when ACTUAL_STATUS = 'Y' then 1 else 0 end)ActualChurners,sum(case when TRAINPREDICTED = 'Y' and ACTUAL_STATUS='Y' then 1 else 0 end)MatchingCount from fileinput where TRAIN_ASPU_STATUS IN ('H','M','L') group by AON_MONTHS ORDER BY AON_MONTHS)A GROUP BY AON,AON_O order by AON_O");
  return(TestAON)
}

