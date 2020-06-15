require(data.table)
require(ggplot2)
require(rjson)
require(plyr)
require(stringr)




processExposures <- function(file){
  exposures <- fromJSON(file=file)
  numExposures = length(exposures)

  thresholds = sort(unique(unlist(llply(exposures, function(x){
    return(x$attenuationDurationThresholds)
  }))))
  thresholds = c(0, thresholds, 100)

  dt = rbindlist(llply(exposures, function(x){
    newDt = data.table(TimeDetected = x$timeDetected,
                       Threshold = x$attenuationDurationThresholds,
                       DurationBelow = c(x$attenuationDurations[[1]], x$attenuationDurations[[1]] + x$attenuationDurations[[2]]),
                       DurationAbove = c(x$attenuationDurations[[2]] + x$attenuationDurations[[3]], x$attenuationDurations[[3]])
    )

    return(newDt)
  }))

  dt[ , TotalDuration := DurationAbove + DurationBelow]

  numBins = length(thresholds) - 1

  durations = data.table(MinThresh = thresholds[1:(length(thresholds)-1)],
                         MaxThresh = thresholds[2:length(thresholds)],
                         Duration = 0)




  durations[MaxThresh == thresholds[2], Duration := dt[Threshold == thresholds[2], DurationBelow]]
  for(bin in 2:(numBins-1)){

    minThresh = thresholds[bin]
    maxThresh = thresholds[bin + 1]
    duration1 = dt[Threshold == minThresh, DurationAbove] - dt[Threshold == maxThresh, DurationAbove]
    duration2 = dt[Threshold == maxThresh, DurationBelow] - dt[Threshold == minThresh, DurationBelow]
    if(duration1 == duration2){
      durations[minThresh == MinThresh, Duration := duration1]
    }else{
      print("Inconsistent data")
      durations[minThresh == MinThresh, Duration := NA]
    }
  }
  durations[MinThresh == thresholds[numBins], Duration := dt[Threshold == thresholds[numBins], DurationAbove]]


  return(durations)
}


dataFolder = "~/Dropbox/JamesPetrieCWTests/"
files = list.files(dataFolder)

dt = NULL
for(file in files){
  if(grepl("json", file, fixed = TRUE)){
    newDt = processExposures(file.path(dataFolder, file))
    newDt[, TestName := str_remove(file, ".json")]
    if(is.null(dt)){
      dt = newDt
    }else{
      dt = rbind(dt, newDt)
    }
  }
}


dt[, MeanAtten := 0.5*(MinThresh + MaxThresh)]

breaks = unique(c(dt$MaxThresh, dt$MinThresh))
ggplot(dt) + geom_histogram( aes(x = MeanAtten, y = Duration, colour = TestName), stat = "identity")

