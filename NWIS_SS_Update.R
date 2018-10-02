#Begin Run----
rm(list=ls())

#Load Packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(tidyverse, ggthemes, fs, stringr, dataRetrieval, data.table,tictoc, microbenchmark,fasttime)

#Define Gage Sites and Quantiles to Work On----
Site <- c("01589035")
startDate <- ""
endDate <- ""
Quantiles <- c(0.3,0.5,0.7,0.8,0.85,0.9,0.95,0.98,0.99,0.9975)
Sensitivity <- 0.01
target <- 8
  
#Define Functions ---- 
import_sites <- function(Site, startDate, endDate) {
  message("Pulling unit value data from NWIS might take some time, please be patient")
  message("Reading Site Info")
    sitedata <<- readNWISsite(siteNumbers = Site)
  message("Reading Daily Data")
    daily <<- readNWISdv(siteNumbers = Site,
                      parameterCd = c("00060"),
                      startDate = startDate,
                      endDate = endDate) %>%
                      renameNWISColumns() %>%
                      filter(Flow!=-999999)
  message("Reading Continuous Data")
    uvdata <<- readNWISuv(siteNumbers = Site,
                       parameterCd = c("00060","00065"),
                       startDate = startDate,
                       endDate = endDate) %>% 
                       renameNWISColumns() %>%
                       filter(Flow_Inst!=-999999) %>%
                       filter(GH_Inst!=-999999)
}
write_sites <- function(Site) {
  writeDir <- paste0("./NWIS_pulls/",Site)
  dir_create(writeDir) #Create Export directory
  message("Saving Daily Data locally")
  fwrite(daily,paste0(writeDir,"/daily.csv"))
  message("Saving Continuous Data locally")
  fwrite(uvdata,paste0(writeDir,"/uvdata.csv"))
  message("Saving Site Info locally")
  fwrite(sitedata,paste0(writeDir,"/sitedata.csv"))
}
read_local <- function() {
  readDir <- paste0("./NWIS_pulls/",Site)
  message("Reading Daily Data locally")
    daily <<- fread(paste0(readDir,"/daily.csv"), colClasses=c(site_no="character"))
    daily$Date <<- as.Date(daily$Date)
  message("Reading Continuous Data locally")
    uvdata <<- fread(paste0(readDir,"/uvdata.csv"), colClasses=c(site_no="character"))
    uvdata$dateTime <<- fastPOSIXct(uvdata$dateTime)
  message("Reading Site Info locally")
    sitedata <<- fread(paste0(readDir,"/sitedata.csv"), colClasses=c(site_no="character"))
}
theme_ss <- function(base_size, base_family) {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(size = rel(1), hjust = 0.5),
            plot.subtitle = element_text(size = rel(01), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2, size = rel(0.9)),
            axis.title.x=element_blank(),
            axis.text.y = element_text(size=rel(0.8)), 
            axis.text.x = element_text(size=rel(0.8)), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "right",
            # legend.direction = "verticale",
            # legend.key.size= unit(0.5, "cm"),
           # legend.margin = unit(0, "cm"),
            legend.title = element_text(size = rel(0.8)),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text()
    ))
}
SS <- function(Site, Quantiles) {
  quantiles <<- quantile(daily$Flow, probs=Quantiles, na.rm=TRUE)
  Runlist = list()
  for(i in 1:(length(quantiles))) {
    Discharge <- quantiles[[i]]
    # Find points where Flow_Inst is above target discharge
    # Points always intersect when above=TRUE, then FALSE or reverse 
    above<-uvdata$Flow_Inst>Discharge
    intersect.index<-which(diff(above)!=0) #Vector of index values immediately before flow passes quantile
    
    # Find the slopes for each line segment.
    Intersect.slopes <- uvdata$Flow_Inst[intersect.index+1]-uvdata$Flow_Inst[intersect.index]
    # Estimate time where target quantile crosses interpolated discharge
    x.points <- intersect.index + ((Discharge - uvdata$Flow_Inst[intersect.index]) / (Intersect.slopes))
    intersect.points <- as.data.frame(x.points)
    
    suppressWarnings(index.fraction <- intersect.points %>% #Split decimals
      separate(x.points, into = c("index","fraction")))
    
    index.fraction$fraction <- index.fraction$fraction %>% #Replace NA with 0's
      replace_na(0)
    
    index.fraction$fraction <- paste("0.", index.fraction$fraction, sep="") #Add 0. to front of decimal values
    index.fraction$secondspast <- round(as.numeric(index.fraction$fraction)*15*60)
    
    QestTime<-uvdata$dateTime[as.numeric(index.fraction$index)]+index.fraction$secondspast #Estimated time of target Q
    
    Approx_Flow <- as.tibble(approx(uvdata$dateTime, y = uvdata$Flow_Inst, xout = QestTime))  %>%
      left_join(as.tibble(approx(uvdata$dateTime, y = uvdata$GH_Inst, xout = QestTime)), by = "x") %>%
      rename(Flow_Inst = y.x) %>%
      rename(GH_Inst = y.y) %>%
      rename(dateTime = x)
    Approx_Flow$i <- paste(i)
    Approx_Flow$Quantflow <- quantiles[[i]]
    Runlist[[i]] <- Approx_Flow
  }
  
  #Bind quantile lists into one dataframe
  SS_results <<- bind_rows(Runlist)
  
  #Cleanup <- not needed anymore due to global binding in function?
  #rm(Approx_Flow,index.fraction,intersect.points,Runlist)
}


#Import data from NWIS,  save to disk, and/or read in locally ----
 #import_sites(Site, startDate, endDate)
 #write_sites(Site)
  #OR#
  read_local()

#Plot original data for check ----  
#Plot daily data
ggplot(data=subset(daily), aes(x=Date, y=Flow))+
    ggtitle(paste("USGS Gage", Site, sitedata$station_nm), subtitle = "Mean Daily Hydrograph")+
    theme_ss(14,"sans")+
    geom_line(alpha=0.8)+
    ylab("Mean Daily Discharge (cfs)")

#Plot unit value discharge & stage
ggplot(data=subset(uvdata), aes(x=dateTime, y=Flow_Inst))+
    ggtitle(paste("USGS Gage", Site, sitedata$station_nm), subtitle = "Instantaneous Unit Value Hydrograph")+
    theme_ss(14,"sans")+
    geom_line(alpha=0.5)+
    ylab("Instantaneous Discharge (cfs)")

ggplot(data=subset(uvdata), aes(x=dateTime, y=GH_Inst))+
  ggtitle(paste("USGS Gage", Site, sitedata$station_nm), subtitle = "Instantaneous Unit Value Stage")+
  theme_ss(14,"sans")+
    geom_line(alpha=0.5)+
    ylab("Gage Height (ft)")
  
#Run Specific Stage Analysis ----
SS_results <- SS(Site, Quantiles)

#Check results for stage and discharge to make sure there are no outliers
ggplot(data = SS_results, aes(x=dateTime, y=Flow_Inst, color=as.factor(i)))+
  geom_point(alpha=0.5)+
  theme_ss(14, "sans")+
  ggtitle(paste("USGS Gage", Site, sitedata$station_nm), subtitle = "Discharge Quantiles Check")+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  scale_color_discrete(name="Discharge\nQuantile", breaks = unique(SS_results$i), labels=(paste(colnames(quantiles),":",round(quantiles,0)," cfs")))+
  scale_x_datetime(date_labels = "%b\n%Y")+
  ylab("UV Discharge (cfs)")

#Clean up Outliers ----
SS_results_clean <- SS_results %>%
                    mutate(pc_diff = (Flow_Inst-Quantflow)/Quantflow) %>%
                    filter(pc_diff > -Sensitivity & pc_diff < Sensitivity)

# Plot Cleaned results
ggplot(data = SS_results_clean, aes(x=dateTime, y=Flow_Inst,color=as.factor(i)))+
  geom_point(alpha=0.5)+
  theme_ss(14, "sans")+
  ggtitle(paste("USGS Gage", Site, sitedata$station_nm), subtitle = "Discharge Quantiles Check - Cleaned")+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  scale_color_discrete(name="Discharge\nQuantile", breaks = unique(SS_results$i), labels=(paste(colnames(quantiles),":",round(quantiles,0)," cfs")))+
  scale_x_datetime(date_labels = "%b\n%Y")+
  ylab("UV Discharge (cfs)")

ggplot(data=subset(SS_results_clean,i==target), aes(x=dateTime,y=Flow_Inst))+
  geom_point(alpha=0.3)+
  ggtitle(paste("USGS Gage", Site, sitedata$station_nm), subtitle = "Target Discharge Quantile Check - Cleaned")+
  theme_ss(14, "sans")+
  scale_color_discrete(name="Discharge\nQuantile", breaks = unique(SS_results_clean$i), labels=(paste(colnames(quantiles),":",round(quantiles,0)," cfs")))+
  guides(colour = guide_legend(reverse=TRUE))+
  scale_x_datetime(date_labels = "%b\n%Y",date_minor_breaks = "1 year")+
  ylab("UV Discharge (cfs)")+
  #geom_smooth(method="gam",alpha=0.5,se=FALSE)
  geom_smooth(alpha=0.05,span=0.2,se=FALSE)+
  theme(axis.title.x=element_blank())

#Plot Specific Stage Results for All Quantiles
ggplot(data=subset(SS_results_clean), aes(x=dateTime,y=GH_Inst,color=as.factor(i)))+
  geom_point(alpha=0.3)+
  ggtitle(paste("USGS Gage", Site, sitedata$station_nm), subtitle = "Specific Stage for all Flow Quantiles")+
  theme_ss(14, "sans")+
  scale_color_discrete(name="Discharge\nQuantile", breaks = unique(SS_results_clean$i), labels=(paste(colnames(quantiles),":",round(quantiles,0)," cfs")))+
  guides(colour = guide_legend(reverse=TRUE))+
  scale_x_datetime(date_labels = "%b\n%Y",date_breaks = "1 year")+
  ylab("Stage (ft)")+
  #geom_smooth(method="gam",alpha=0.5,se=FALSE)
  geom_smooth(alpha=0.05,span=0.3,se=FALSE)+
  theme(axis.title.x=element_blank())
  
#Plot Specific Stage Results for Target Quantiles
ggplot(data=subset(SS_results_clean,i==target), aes(x=dateTime,y=GH_Inst))+
  geom_point(alpha=0.3)+
  ggtitle(paste("USGS Gage", Site, sitedata$station_nm),
          subtitle = paste("Specific Stage at", round(quantiles[[target]],2), " cfs, ", Quantiles[target]*100,"% of mean daily flow"))+
  theme_ss(14, "sans")+
  scale_color_discrete(name="Discharge\nQuantile",labels=(paste(colnames(quantiles),":",round(quantiles,0)," cfs")))+
  guides(colour = guide_legend(reverse=TRUE))+
  scale_x_datetime(date_labels = "%b\n%Y",date_breaks = "1 year")+
  ylab(paste("Stage (ft)"))+
  #geom_smooth(method="gam",alpha=0.5,se=FALSE)
  geom_smooth(alpha=0.05,span=0.2,se=FALSE)+
  theme(axis.title.x=element_blank())
  
