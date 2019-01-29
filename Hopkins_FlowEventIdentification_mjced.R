
# Identify storm events in sub-daily discharge records
# Adapted from code from Kelly Hondula 
# Krissy Hopkins
# 12/20/2018

library(zoo)
library(dplyr)
library(tidyr)
library(reshape)
library(plyr)

########################
# Load streamflow data #
########################
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table, fasttime, fs, gridExtra, plotly, wesanderson, magrittr,shiny)

writeDir <- paste0("./NWIS_pulls/Patapsco")
#Read DV data  
dv_data <- fread(paste0(writeDir,"/dv_data.csv"), colClasses=c(site_no="character")) %>%
  filter(Flow != "-999999")
dv_data$Date <- as.Date(dv_data$Date)
#Read UV data
uv_data <- fread(paste0(writeDir,"/uv_data.csv"), colClasses=c(site_no="character")) %>%
  filter(Flow_Inst != "-999999") %>%
  filter(GH_Inst != "-999999")
uv_data$dateTime <- fastPOSIXct(uv_data$dateTime)
#Read site data
site_info <- fread(paste0(writeDir,"/site_info.csv"), colClasses=c(site_no="character")) %>%
  unite(full_name, agency_cd, site_no, station_nm, sep = " ", remove=FALSE)
#Read measurement data
meas_data <- fread(paste0(writeDir,"/site_meas.csv"), colClasses=c(site_no="character"))
meas_data$measurement_dateTime <- fastPOSIXct(meas_data$measurement_dateTime)
#Link site and name info into other datasets
dv_data <- left_join(dv_data,site_info, by = c("site_no"))
uv_data <- left_join(uv_data,site_info, by = c("site_no"))
meas_data <- left_join(meas_data,site_info, by = c("site_no"))
#Calculate stage to datum elevation
uv_data %<>% 
  mutate(elev_GH = GH_Inst + alt_va)
meas_data %<>% 
  mutate(elev_GH = gage_height_va + alt_va)

##########################
# Start of Krissy's Code #
##########################

final <- uv_data
#final$dateTime <- as.POSIXct(strptime(final$dateTime, "%Y-%m-%d %H:%M:%S"), tz = "America/Jamaica")
final %<>%  
 # rename(Date = dateTime) %>%
  dplyr::rename(Q_cfs = Flow_Inst) %>%
  filter(site_no == "01589000")
  
#colnames(final) <- c("Date", "Q_cfs") # Name columns DateTime and Q_cfs

# Make sure the datetimes are in order
  #final <- final[order(final$dateTime),] 

##############################
# Calculate Rolling minimums #
##############################
# If you are using 15-min data you values in rollapply are the number of time increments 
# 15-min data would have the following
# 24 hours is 96 increments
# 12 hours is 48 increments
library(zoo)
final$Q_leadrollmin_24hr <- rollapply(final$Q_cfs, 192, fill = NA, align = "left", min) # lead, falling limb
final$Q_lagrollmin_12hr <- rollapply(final$Q_cfs, 46, fill = NA, align = "right", min) # lag rising limb

# Calculate discharge minus rolling mins
final %<>% 
  select(agency_cd.x, site_no, dateTime, Q_cfs, GH_Inst, Turb_Inst, tz_cd.x, full_name,station_nm,elev_GH,Q_leadrollmin_24hr,Q_lagrollmin_12hr) %>%
  mutate(Q_minLead24hr = Q_cfs - Q_leadrollmin_24hr) %>%  # 24 hour window
  mutate(Q_minlag12hr = Q_cfs - Q_lagrollmin_12hr) # 12 hours window


#########################
# Create indicator code #
#########################
final$EventInd <- 0
final$EventInd[final$Q_cfs > 1000] <- 1 # cfs threshold, this is the flow you are sure would be included as an event
final$EventInd[final$Q_minLead24hr > 30] <- 1 # Difference from baseflow indicator to trigger an event, this is in cfs
final$EventInd[final$Q_minlag12hr > 30] <- 1  # Difference from baseflow indicator to trigger an event, this is in cfs

###########################################################################
# Make eventlengthd_df to find out lengths of time periods between events #
###########################################################################
# Use rle() run length to determine the lengths of time between events

stormeventlengths_df <- as.data.frame(cbind(rle(final$EventInd)$lengths, rle(final$EventInd)$values))
colnames(stormeventlengths_df) <- c("length", "indicator")
stormeventlengths_df$qLength <- cumsum(stormeventlengths_df$length)

stormeventlengths_df$hours <- NA  # add hours column

# use difftime() to calculate time difference between first row of flow recording to the end of that first event, first qLength
start1 <- final[1,"dateTime"]
end1 <- final[(stormeventlengths_df[1,"qLength"]),"dateTime"]
time1 <- as.numeric(difftime(end1,start1, units="hours"))


#######################################################################################
# function to calculate all but the first time difference of continuously recorded 0/1s
#######################################################################################
calcEventHours <- function(x){
  start <- final[(stormeventlengths_df[x-1,"qLength"]),"dateTime"]
  end <- final[(stormeventlengths_df[x,"qLength"]),"dateTime"]
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
noFlow_T = 24 # inter-event window in hours. I used 6 hours

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

storm.events_start.time <- sapply(storm_events_list, function(x) strftime(x[1,"dateTime"]))

storm.events_end.time <- sapply(storm_events_list, function(x) strftime(x[nrow(x),"dateTime"]))

storm.events_prior.dry.times.hrs <- sapply(c(1:number_storm_events), function(x) round(stormeventlengths_df[eventstarts[x],]$hours,2))

storm.events_baseQ_Qminlag12hr_cfs <- sapply(storm_events_list, function(x) min(x$Q_minlag12hr))

storm.events_time.max.flow.cfs <- sapply(storm_events_list, function(x) strftime(x[which.max(x$Q_cfs),"dateTime"]))

storm.events_max.flow.cfs <- sapply(storm_events_list, function(x) max(x$Q_cfs))

storm.events_min.flow.cfs <- sapply(storm_events_list, function(x) min(x$Q_cfs))

storm.events_duration.mins <- sapply(storm_events_list, function(x) round(as.numeric(difftime(x[nrow(x),"dateTime"],x[1,"dateTime"], units="mins")),digits=0))

storm.events_duration.hrs <- sapply(storm_events_list, function(x) round(as.numeric(difftime(x[nrow(x),"dateTime"],x[1,"dateTime"], units="hours")),digits=2))


# summary of event statistics
all.storm.events.info <-
  cbind(as.data.frame(cbind(storm.events_start.time, storm.events_end.time)),
        as.data.frame(cbind(storm.events_time.max.flow.cfs, 
                            storm.events_max.flow.cfs, 
                            storm.events_min.flow.cfs,
                            storm.events_prior.dry.times.hrs,
                            storm.events_baseQ_Qminlag12hr_cfs,
                            storm.events_duration.mins,
                            storm.events_duration.hrs)))

finalstormevents <- all.storm.events.info[which(storm.events_duration.mins>0),]

#Plot Data for storm events
p1<-ggplot(data=final, aes(x=dateTime, y=Q_cfs,group=EventInd))+
  geom_line(data=subset(final,EventInd==0), aes(x=dateTime, y=Q_cfs),color="Black", alpha = 0.8)+
  geom_line(data=subset(final,EventInd==1), aes(x=dateTime, y=Q_cfs),color="Red", alpha = 0.8)+
  ylab("Instantaneous Discharge (cfs)")+
  scale_y_log10()+
  facet_wrap(~ full_name, ncol = 1)+
  scale_x_datetime(date_labels = "%Y",date_breaks = "1 year",limits= as.POSIXct(strptime(c("2010-10-01","2018-11-01"), format = "%Y-%m-%d")))
#p1
library(plotly)
#ggplotly(p1)
#write.csv(finalstormevents, "")
