
# Identify storm events in sub-daily discharge records
# Adapted from code from Kelly Hondula 
# Krissy Hopkins
# 12/20/2018

library(dplyr)
library(tidyr)
library(reshape)
library(plyr)

########################
# Load streamflow data #
########################

final <- read.csv("", stringsAsFactors = FALSE)
final$dateTime <- as.POSIXct(strptime(final$dateTime, "%Y-%m-%d %H:%M:%S"), tz = "America/Jamaica")
colnames(final) <- c("Date", "Q_cfs") # Name columns DateTime and Q_cfs

# Make sure the datetimes are in order
final <- final[order(final$dateTime),] 

##############################
# Calculate Rolling minimums #
##############################
# If you are using 15-min data you values in rollapply are the number of time increments 
# 15-min data would have the following
# 24 hours is 96 increments
# 12 hours is 48 increments
final$Q_leadrollmin_24hr <- rollapply(final$Q_cfs, 96, fill = NA, align = "left", min) # lead, falling limb
final$Q_lagrollmin_12hr <- rollapply(final$Q_cfs, 46, fill = NA, align = "right", min) # lag rising limb

# Calculate discharge minus rolling mins
final = final %>% 
  mutate(Q_minLead24hr = Q_cfs - Q_leadrollmin_24hr) %>%  # 24 hour window
  mutate(Q_minlag12hr = Q_cfs - Q_lagrollmin_12hr) # 12 hours window


#########################
# Create indicator code #
#########################
final$EventInd <- 0
final$EventInd[final$Q_cfs > 2] <- 1 # cfs threshold, this is the flow you are sure would be included as an event
final$EventInd[final$Q_minLead24hr > 0.17] <- 1 # Difference from baseflow indicator to trigger an event, this is in cfs
final$EventInd[final$Q_minlag12hr > 0.17] <- 1  # Difference from baseflow indicator to trigger an event, this is in cfs

###########################################################################
# Make eventlengthd_df to find out lengths of time periods between events #
###########################################################################
# Use rle() run length to determine the lengths of time between events

stormeventlengths_df <- as.data.frame(cbind(rle(final$EventInd)$lengths, rle(final$EventInd)$values))
colnames(stormeventlengths_df) <- c("length", "indicator")
stormeventlengths_df$qLength <- cumsum(stormeventlengths_df$length)

stormeventlengths_df$hours <- NA  # add hours column

# use difftime() to calculate time difference between first row of flow recording to the end of that first event, first qLength
start1 <- final[1,"Date"]
end1 <- final[(stormeventlengths_df[1,"qLength"]),"Date"]
time1 <- as.numeric(difftime(end1,start1, units="hours"))


#######################################################################################
# function to calculate all but the first time difference of continuously recorded 0/1s
#######################################################################################
calcEventHours <- function(x){
  start <- final[(stormeventlengths_df[x-1,"qLength"]),"Date"]
  end <- final[(stormeventlengths_df[x,"qLength"]),"Date"]
  return(as.numeric(difftime(end,start, units="hours")))
}
#######################################################################################
# END function 

# calculate the length of all other periods for hours column 
# make a vector of the rest of the events to calculate the time of

events_vector <- c(2:nrow(stormeventlengths_df))

# apply function to calculate the rest of the events to fill in the rest of the hours column

stormeventlengths_df$hours <- c(time1, sapply(events_vector, calcEventHours))

#######################################################################################

# set threshold amount of time (hours) for no stormflow
noFlow_T = 6 # inter-event window in hours. I used 6 hours

# for continuously recorded no flow periods (indicator == 0), add TRUE/FALSE depending on whether duration exceeds noFlow_T
stormeventlengths_df$dry_duration <- NA
stormeventlengths_df$dry_duration[(stormeventlengths_df$hours>noFlow_T & stormeventlengths_df$indicator==0)] <- TRUE
stormeventlengths_df$dry_duration[(stormeventlengths_df$hours<noFlow_T & stormeventlengths_df$indicator==0) ] <- FALSE

# identify the start of flow events that meet minimum inter-event time
# event.starts is a vector of the rows in the events.df which have long enough dry periods
eventstarts <- as.numeric(as.vector(row.names(stormeventlengths_df[which(stormeventlengths_df$dry_duration==TRUE),])))

#################################################################################################
# function that makes a dataframe of a storm events that meet inter-event time threshold
#################################################################################################

makeEventdf <- function(x){
  start.row <- stormeventlengths_df[eventstarts[x],"qLength"]     # starting row finds a row in eventlengths_df that meets criteria and finds the end of the dry period
  end.row <- stormeventlengths_df[eventstarts[x+1]-1,"qLength"]   # ending row looks for the row before the next row that meets criteria, ie. finds the end of the wet period right before the next long enough dry period
  event.df <- final[(start.row+1):end.row,]              # subset the rain data using these rows, add one to start row because that is the end of the dry period
  return(event.df)  
}

#################################################################################################
# END function that makes data frame of a precipitation event that meets inter-event time threshold
#################################################################################################

# number of storm events is the length of eventstarts -1
number_storm_events <- length(eventstarts)-1

# Calculate stats
storm_events_list <- lapply(c(1:number_storm_events), makeEventdf) 

storm.events_start.time <- sapply(storm_events_list, function(x) strftime(x[1,"Date"]))

storm.events_end.time <- sapply(storm_events_list, function(x) strftime(x[nrow(x),"Date"]))

storm.events_prior.dry.times.hrs <- sapply(c(1:number_storm_events), function(x) round(stormeventlengths_df[eventstarts[x],]$hours,2))

storm.events_baseQ_Qminlag12hr_cfs <- sapply(storm_events_list, function(x) min(x$Q_minlag12hr))

storm.events_time.max.flow <- sapply(storm_events_list, function(x) strftime(x[which.max(x$Q_cfs),"Date"]))

storm.events_max.flow.cfs <- sapply(storm_events_list, function(x) max(x$Q_cfs))

storm.events_min.flow.cfs <- sapply(storm_events_list, function(x) min(x$Q_cfs))

storm.events_duration.mins <- sapply(storm_events_list, function(x) round(as.numeric(difftime(x[nrow(x),"Date"],x[1,"Date"], units="mins")),digits=0))

storm.events_duration.hrs <- sapply(storm_events_list, function(x) round(as.numeric(difftime(x[nrow(x),"Date"],x[1,"Date"], units="hours")),digits=2))


# summary of event statistics
all.storm.events.info <-
  cbind(as.data.frame(cbind(storm.events_start.time, storm.events_end.time)),
        as.data.frame(cbind(storm.events_time.max.flow.cfs, 
                            storm.events_max.flow.cfs, 
                            storm.events_min.flow,
                            storm.events_prior.dry.times.hrs,
                            storm.events_baseQ_Qminlag12hr_cfs,
                            storm.events_duration.mins,
                            storm.events_duration.hrs)))

finalstormevents <- all.storm.events.info[which(storm.events_duration.mins>0),]

write.csv(finalstormevents, "")
