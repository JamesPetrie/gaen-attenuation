require(readxl)
require(data.table)
require(rjson)
require(plyr)
require(stringr)
require(foreach)

processExposures <- function(file){
  exposures <- fromJSON(file=file)$possibleExposures

  if(length(exposures) == 0){
    return(data.table(DurationBelow = numeric(), DurationBetween = numeric(),
                      DurationAbove = numeric(), MinThresh = numeric(), 
                      MaxThresh = numeric(), TimeDetected = numeric(),
                      DurationTotalReported= numeric()))
  }

  dt = rbindlist(llply(exposures, function(x){
    newDt = data.table(TimeDetected = x$timeDetected,
                       MinThresh = x$attenuationDurationThresholds[[1]],
                       MaxThresh = x$attenuationDurationThresholds[[2]],
                       DurationBelow = x$attenuationDurations[[1]],
                       DurationBetween = x$attenuationDurations[[2]],
                       DurationAbove = x$attenuationDurations[[3]],
                       DurationTotalReported = x$duration
    )
    return(newDt)
  }))

  minDt = dt[MinThresh == min(MinThresh), list(DurationBelow = 0, DurationBetween = DurationBelow,
                                                 DurationAbove = DurationBetween + DurationAbove,
                                                 MinThresh = 0, MaxThresh = MinThresh,
                                                 TimeDetected = TimeDetected,
                                                 DurationTotalReported = DurationTotalReported)]
  maxDt = dt[MaxThresh == max(MaxThresh), list(DurationBelow = DurationBelow + DurationBetween, DurationBetween = DurationAbove,
                                                 DurationAbove = 0,
                                                 MinThresh = MaxThresh, MaxThresh = 128,
                                                 TimeDetected = TimeDetected,
                                                 DurationTotalReported = DurationTotalReported)]

  dt = rbind(minDt, dt, maxDt)
  dt[, MeanAtten := 0.5*(MinThresh + MaxThresh)]

  return(dt)
}



processScenarios <- function(scenarioFile){
  testScenarios = data.table(read_excel(scenarioFile, skip = 1, col_types = "text"))
  colnames = unlist(unname(llply(names(testScenarios), function(x){str_replace_all(x, fixed(" "), "")})))
  setnames(testScenarios, colnames)

  setnames(testScenarios, old = c("Distance(m)", "Duration(min)", "PhoneCaseOn?", "Phone1batterybefore(%)","Phone1batteryafter(%)", "Phone2batterybefore(%)","Phone2batteryafter(%)"), 
           new = c("Distance", "DurationPlanned","PhoneCaseOn", "Phone1BatteryBefore","Phone1BatteryAfter","Phone2BatteryBefore","Phone2BatteryAfter" ), skip_absent=TRUE)

  testScenarios[, Distance := as.numeric(Distance)]

  # Get only the rows with numbers in TestID field
  testScenarios = testScenarios[grepl("\\d", TestID)]
  return(testScenarios)
}


combineTestData = function(dataFolder){
  testerDirs = list.dirs(dataFolder, recursive = FALSE)

  dt = NULL
  for(testerDir in testerDirs){
    scenarioFile = list.files(testerDir, pattern = "\\.xlsx$", full.names = TRUE)[1]
    if(is.na(scenarioFile)){
      print(paste("No scenario file - skipping test directory: ", testerDir))
      print(paste(as.character(length(list.files(testerDir))), "Other files in the directory"))
      next
    }
    testScenarios = processScenarios(scenarioFile)
    setkey(testScenarios, by = TestID)

    for(file in list.files(testerDir)){
      if(grepl("json", file, fixed = TRUE)){
        testDesc = str_split(str_remove(file, ".json"), "_")[[1]]
        testID = testDesc[length(testDesc)]
        device = testDesc[2]
        tester = testDesc[1]
        
        print(tester)
        print(testID)
        newDt = processExposures(file.path(testerDir, file))


        newDt[, TestID := testID]
        newDt[, Device := device]
        newDt[, Tester := tester]

        setkey(newDt,by = TestID)
        newDt = merge(newDt, testScenarios, all.x = TRUE)



        if(is.null(dt)){
          dt = newDt
        }else{
          dt = rbind(dt, newDt, fill =TRUE)
        }
      }
    }

  }
  dtDevices = dt[,list(Device = unique(Device)), by = list(Tester, TestID)]

  for(tester in unique(dtDevices$Tester)){
    for(testID in unique(dtDevices[Tester == tester, TestID])){
      for(device in unique(dtDevices[Tester == tester & TestID == testID, Device])){
        otherDevices = dtDevices[Tester == tester & TestID == testID & Device != device, Device]
        if(length(otherDevices) == 1){
          dt[Tester == tester & TestID == testID & Device == device, OtherDevice := otherDevices[1]]
        }else{
          print("unable to determine what the other device was")
          dt[Tester == tester & TestID == testID & Device == device, OtherDevice := NA]
        }
      }
    }
  }



  return(dt)
}


splitMeasurements = function(dt){
  #each measurement is in 5 minute intervals
  dt[, Rep := floor(DurationBetween/300)]

  dtPoint = dt[Rep == 1]

  for(rep in 2:max(dt$Rep)){
    dtPoint = rbind(dtPoint, do.call("rbind", replicate(rep, dt[Rep == rep], simplify = FALSE)))
  }
  dtPoint$DurationAbove = NULL
  dtPoint$DurationBetween = 300
  dtPoint$DurationBelow = NULL

  return(dtPoint)
}

removeCorruptedData = function(dt){
  dt[Tester == "Tester17" & 
       Barrier == "Inside/outside car" & 
       TimeDetected <= 614731724 &
       MaxThresh <= 40, 
     DurationBetween := 0]
  
  dt[Tester == "Tester17" & 
       Barrier == "Door" & 
       TimeDetected <= 614721083 &
       MaxThresh <= 40, 
     DurationBetween := 0]
  
  return(dt)
}



flagOutliers = function(dtPoint){
  dtPoint[,Outlier := FALSE]
  
  dtPoint[Tester == "Tester16" & 
        TestID %in% c("B1","B2","B3","B4","B5","B6") &
        MaxThresh < 58 &
        TimeDetected <  615406208
       ,Outlier := TRUE]
  
  dtPoint[Tester == "Tester16" & 
            TestID %in% c("C19","C20","C21","C23","C24","C25","C26","C27","C28","C30","C31","C32","C33") &
            MaxThresh < 58 &
            TimeDetected <  615406208
          ,Outlier := TRUE]
  
  dtPoint[Tester == "Tester17" & 
            TestID %in% c("E2","E3","E4","E5","E9","E10","E11","E12") &
            MaxThresh < 50 &
            TimeDetected <  615406208
          ,Outlier := TRUE]
  
  dtPoint[Tester == "Tester17" & 
            TestID %in% c("E15","E16","E17","E18","E19","E23","E24","E25","E26","E27") &
            MaxThresh < 40 &
            TimeDetected <  615406208
          ,Outlier := TRUE]
  
  dtPoint[Tester == "Tester17" & 
            TestID %in% c("F1", "F2", "F3", "F4", "F5") &
            MaxThresh < 50 &
            TimeDetected <  615406208
          ,Outlier := TRUE]
  dtPoint[Tester == "Tester1" & 
            TestID %in% c("A7", "C11", "D10", "D11", "D12") &
            MaxThresh < 40 &
            TimeDetected <  615406208
          ,Outlier := TRUE] 
  dtPoint[Tester == "Tester1" & 
            TestID %in% c( "E24","E25", "E26", "E27") &
            MaxThresh < 45 &
            TimeDetected <  615406208
          ,Outlier := TRUE]
  dtPoint[Tester == "Tester1" & 
            TestID %in% c("E1", "E2", "E3", "E4") &
            MaxThresh < 50 &
            TimeDetected <  615406208
          ,Outlier := TRUE] 
  dtPoint[Tester == "Tester15" & 
            TestID %in% c("C18", "C19", "C24", "C25","C44", "C45", "C46", "C47", "C51", "C52","C53", "C54") &
            MaxThresh < 40 &
            TimeDetected <  615406208
          ,Outlier := TRUE] 
  dtPoint[Tester == "Tester15" & 
            TestID %in% c("C26") &
            MaxThresh < 45 &
            TimeDetected <  615406208
          ,Outlier := TRUE] 
  dtPoint[Tester == "Tester15" & 
            TestID %in% c("F1","F2", "F3", "F4", "F5") &
            MaxThresh < 45 &
            TimeDetected <  615406208
          ,Outlier := TRUE] 
  dtPoint[Tester == "Tester3" & 
            TestID %in% c("C11", "C12") &
            MaxThresh < 50 &
            TimeDetected <  615406208
          ,Outlier := TRUE] 
}