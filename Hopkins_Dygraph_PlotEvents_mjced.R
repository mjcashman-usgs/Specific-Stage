# Dygraph  to plot of events
# Krissy Hopkins
# 10/23/2018

library(dplyr)
library(zoo)
library(dygraphs)

# Load Q data
    #final <- read.csv("/Users/khopkins/Documents/Manuscripts/Trib104_Hydro/Data/Q/Input/SB_Q_All_11.2.2018_Interp.csv", stringsAsFactors = FALSE)
    #final$dateTime <- as.POSIXct(strptime(final$dateTime, "%Y-%m-%d %H:%M:%S"), tz = "America/Jamaica")
    #final <- final[,c(1,3)]
    #colnames(final) <- c("Date", "Q_cfs")

  
# Load Event Data
    Events <- finalstormevents
    Events$storm.events_start.time <- fastPOSIXct(Events$storm.events_start.time)
    Events$storm.events_end.time <- fastPOSIXct(Events$storm.events_end.time)
    Events$storm.events_time.max.flow.cfs <- as.numeric(Events$storm.events_time.max.flow.cfs)
    Events$storm.events_max.flow.cfs <- as.numeric(Events$storm.events_max.flow.cfs)
    Events$storm.events_min.flow.cfs <- as.numeric(Events$storm.events_min.flow.cfs)
    Events$storm.events_prior.dry.times.hrs <- as.numeric(Events$storm.events_prior.dry.times.hrs)
    Events$storm.events_baseQ_Qminlag12hr_cfs <- as.numeric(Events$storm.events_baseQ_Qminlag12hr_cfs)
    Events$storm.events_duration.mins <- as.numeric(Events$storm.events_duration.mins)
    Events$storm.events_duration.hrs <- as.numeric(Events$storm.events_duration.hrs)
    

# Remove events shorter than 15 minutes 
Events <- subset(Events, Events$storm.events_duration.mins > 15)

# Remove events with less than 0.1 cfs change
Events$FlowChange_cfs <- Events$storm.events_max.flow.cfs - Events$storm.events_min.flow.cfs
Events <- subset(Events, FlowChange_cfs > 0.1)

# Subset flow data for just events
Q_Storm <- data.frame(matrix(nrow=0, ncol=3))

for (i in 1:nrow(Events)){
  
  Storm <- subset(final, dateTime >= Events$storm.events_start.time[i] & dateTime <= Events$storm.events_end.time[i])
  Storm <- cbind(Storm, i)
  Q_Storm <- rbind(Q_Storm, Storm)
}

# Merge Events and Q data
colnames(Q_Storm)[15] <- "Qual_storm"
colnames(Q_Storm)[16] <- "EventID"

Q_All <- full_join(final, Q_Storm[,c(3,4,16)], by=c("dateTime"))
 colnames(Q_All)[4] <- "Q_cfs"
 colnames(Q_All)[16] <- "EventQ_cfs"
 colnames(Q_All)[17] <- "EventID"

# Make dygraph
# Remove rows with NA dates
Q_All  <- Q_All[!is.na(Q_All$dateTime), ]
Q_All <- Q_All[order(Q_All$dateTime),]

data <- Q_All %>%
  select(dateTime,Q_cfs,EventQ_cfs,EventID)
# convert to a timeseries
Z <- read.zoo(data)

# Plot
dygraph(Z, main = "Events", ylab = "Discharge (cfS)") %>% 
   dySeries("EventQ_cfs", fillGraph = FALSE, color = "black", axis = 'y', drawPoints = TRUE, pointSize = 4, strokeWidth=2) %>%  
   dySeries("Q_cfs", fillGraph = FALSE, color = "red", axis = 'y', drawPoints = FALSE, strokeWidth=1) %>%
  dyAxis("y", label = "Discharge (cfs)", valueRange = c(0, 2000), independentTicks = TRUE) %>%
  dyRangeSelector(dateWindow = c("2010-10-01", "2018-10-20"), height = 50, strokeColor = "") %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.5,
              hideOnMouseOut = FALSE) %>%
  dyOptions(useDataTimezone = FALSE)

