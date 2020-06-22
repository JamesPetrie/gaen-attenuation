require(readxl)
require(data.table)
require(rjson)
require(plyr)
require(stringr)

processExposures <- function(file){
  exposures <- fromJSON(file=file)$possibleExposures


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

  setnames(testScenarios, old = c("Distance(m)", "Duration(min)"), new = c("Distance", "DurationPlanned"))

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
        newDt = processExposures(file.path(testerDir, file))
        testDesc = str_split(str_remove(file, ".json"), "_")[[1]]
        testID = testDesc[length(testDesc)]
        device = testDesc[2]
        tester = testDesc[1]

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

