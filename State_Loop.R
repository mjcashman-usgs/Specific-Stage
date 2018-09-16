#Summary Results of Error Analysis
getwd()
require(ggplot2) || install.packages("ggplot2")

#Import data from working directory
getwd()
setwd("D:/R_Projects/Specific Stage") #Set WD

rm(list=ls())
G_03223425_DV_MEAN <- read.delim("D:/R_Projects/Specific Stage/03223425_MDV_DISCHARGE.RDB", comment.char="#")
G_03223425_DV_MEAN<-G_03223425_DV_MEAN[-1,]
G_03223425_UV_DISCHARGE <- read.delim("D:/R_Projects/Specific Stage/03223425_UV_DISCHARGE.RDB", comment.char="#")
G_03223425_UV_DISCHARGE<-G_03223425_UV_DISCHARGE[-1,]
G_03223425_UV_STAGE <- read.delim("D:/R_Projects/Specific Stage/03223425_UV_STAGE.RDB", comment.char="#")
G_03223425_UV_STAGE<-G_03223425_UV_STAGE[-1,]

#Combine
G_03223425 <- merge(G_03223425_UV_DISCHARGE,G_03223425_UV_STAGE) 
rm(G_03223425_UV_DISCHARGE,G_03223425_UV_STAGE) 

#Convert UV  to Numeric
G_03223425$U03223425.00060<-as.numeric(G_03223425$U03223425.00060)/10
G_03223425$U03223425.00065<-as.numeric(G_03223425$U03223425.00065)/100
str(G_03223425)

#Convert DV  to Numeric
G_03223425_DV_MEAN$D03223425.00060<-as.numeric(G_03223425_DV_MEAN$D03223425.00060)/10

#Discharge Determination
quantiles<-quantile(G_03223425_DV_MEAN$D03223425.00060,probs=seq(0,1,0.1))
quantiles 
Target_Discharge<-quantiles[[5+1]]
Target_Discharge

# Hydrographs -------------------------------------------------------------
ggplot(data=G_03223425_DV_MEAN, aes(x=as.numeric(row.names(G_03223425_DV_MEAN)), y=D03223425.00060))+
  ggtitle(paste("Hollofield Hydrograph"))+
  geom_path(alpha=0.5)+
  ylab("Discharge (cfs)")+
  geom_hline(yintercept=Target_Discharge, color="red",size=1.5)+
  theme(axis.title.x=element_blank())

#Repeated Plot for each Quantile
    quantiles<-quantile(G_03223425_DV_MEAN$D03223425.00060,probs=seq(0,1,0.1))
    quantiles<-t(as.matrix(quantiles) )
    quantiles
    G_03223425$date <- as.Date(paste(G_03223425$YEAR, G_03223425$MONTH,G_03223425$DAY, sep = "." )  , format = "%Y.%m.%d" )
    for(i in 1:(ncol(quantiles))) {
      Discharge<-quantiles[i]
      #Run.no<-colnames(quantiles)[i]
      d.frame<-subset(G_03223425,U03223425.00060>(Discharge*0.995)&U03223425.00060<(Discharge*1.005))
      df.names <- assign(paste("Run", i-1,sep=""),d.frame)
    }
    #Prepare Data
    require(reshape2) || install.packages("reshape2")
    newData <- melt(list(Run0, Run1, Run2, Run3, Run4, Run5, Run6, Run7, Run8, Run9, Run10), id.vars = c("date","YEAR","MONTH", "DAY","MINUTE"))
    #Plot  Stage
        ggplot(data=subset(newData,variable=="U03223425.00065"), aes(x=date,y=value,color=as.factor(L1)))+
          geom_point(alpha=0.5)+
          geom_smooth(method="lm",alpha=0.7,se=FALSE)+
      ylab("Stage Height")+
     scale_color_discrete(name="Discharge\nQuantile",labels=colnames(quantiles))+
          coord_cartesian(ylim=c(0,50))+
    scale_x_date(date_breaks = "1 year", date_labels = "%b\n%Y",date_minor_breaks = "6 months", expand=c(0,0))
    
    
# Target Subsetting and Plot -----------------------------------------------------
G_03223425_Target<-subset(G_03223425,U03223425.00060>(Target_Discharge*0.995)&U03223425.00060<(Target_Discharge*1.005))
      #Checking Discharge Selection is correct
          ggplot(data=G_03223425_Target,aes(x=date, y=U03223425.00060))+
            geom_hline(yintercept=Target_Discharge, color="red",size=1.5)+
            geom_point(alpha=0.5)+ 
          geom_smooth()
      
      #Plot  Stage
      ggplot(data=G_03223425_Target,aes(x=date, y=U03223425.00065))+
        geom_point(alpha=0.5)+ 
        stat_smooth()+
        ylab("Stage Height")+
        scale_x_date(date_breaks = "1 year", date_labels = "%b\n%Y",date_minor_breaks = "6 months", expand=c(0,0))
          
       
        
          #Convert Dates
          library(lubridate)
          G_03223425_Target$date <- as.Date(paste(G_03223425_Target$YEAR, G_03223425_Target$MONTH,G_03223425_Target$DAY, sep = "." )  , format = "%Y.%m.%d" )
          G_03223425_Target$Time <- as.POSIXct(G_03223425_Target$MINUTE,format="%M")
          G_03223425_Target$datetime <- as.POSIXct(paste(G_03223425_Target$YEAR, G_03223425_Target$MONTH,G_03223425_Target$DAY,G_03223425_Target$DAY, sep = "." )  , format = "%Y.%b.%d.%M" )
          