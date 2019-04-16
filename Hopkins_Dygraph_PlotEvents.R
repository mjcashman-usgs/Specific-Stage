# Dygraph  to plot of events
# Krissy Hopkins
# 10/23/2018

library(dplyr)
library(zoo)
library(dygraphs)

# Load Q data
final <- read.csv("/Users/khopkins/Documents/Manuscripts/Trib104_Hydro/Data/Q/Input/SB_Q_All_11.2.2018_Interp.csv", stringsAsFactors = FALSE)
final$dateTime <- as.POSIXct(strptime(final$dateTime, "%Y-%m-%d %H:%M:%S"), tz = "America/Jamaica")
final <- final[,c(1,3)]
colnames(final) <- c("Date", "Q_cfs")


# Load Event Data
Events <- read.csv("/Users/khopkins/Documents/Manuscripts/Trib104_Hydro/Data/Q/Output/SB_6hrEvents.csv", stringsAsFactors = FALSE)
Events$storm.events_start.time <- as.POSIXct(strptime(Events$storm.events_start.time, "%Y-%m-%d %H:%M"))
Events$storm.events_end.time <- as.POSIXct(strptime(Events$storm.events_end.time, "%Y-%m-%d %H:%M"))


# Remove events shorter than 15 minutes 
Events <- subset(Events, Events$storm.events_duration.mins > 15)

# Remove events with less than 0.1 cfs change
Events$FlowChange_cfs <- Events$storm.events_max.flow.cfs - Events$storm.events_min.flow.cfs
Events <- subset(Events, FlowChange_cfs > 0.1)

# Subset flow data for just events
Q_Storm <- data.frame(matrix(nrow=0, ncol=3))

for (i in 1:nrow(Events)){
  
  Storm <- subset(final, Date >= Events$storm.events_start.time[i] & Date <= Events$storm.events_end.time[i])
  Storm <- cbind(Storm, i)
  Q_Storm <- rbind(Q_Storm, Storm)
}

# Merge Events and Q data
Q_All <- full_join(final, Q_Storm[,c(1:3)], by=c("Date"))
colnames(Q_All)[2] <- "Q_cfs"
colnames(Q_All)[3] <- "EventQ_cfs"
colnames(Q_All)[4] <- "EventID"

# Make dygraph
# Remove rows with NA dates
Q_All  <- Q_All[!is.na(Q_All$Date), ]
Q_All <- Q_All[order(Q_All$Date),]

# convert to a timeseries
Z <- read.zoo(Q_All)

# Plot
dygraph(Z, main = "Events", ylab = "Discharge (cfS)") %>% 
   dySeries("EventQ_cfs", fillGraph = FALSE, color = "black", axis = 'y', drawPoints = TRUE, pointSize = 4, strokeWidth=0) %>%  
   dySeries("Q_cfs", fillGraph = FALSE, color = "red", axis = 'y', drawPoints = FALSE, strokeWidth=2) %>%
  dyAxis("y", label = "Discharge (cfs)", valueRange = c(0, 10), independentTicks = TRUE) %>%
  dyRangeSelector(dateWindow = c("2017-10-01", "2018-10-20"), height = 50, strokeColor = "") %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.5,
              hideOnMouseOut = FALSE) %>%
  dyOptions(useDataTimezone = TRUE)

