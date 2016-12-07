rm(list=ls())
#Load Packages
require(reshape2) || install.packages("reshape2")
require(ggplot2) || install.packages("ggplot2")

#Choose Sensitivity Threshold
  sensitivity<-0.05

#Choose Quantiles
  quantile.selection<-seq(0,1,0.1)
  
####Data Preparation####
  #Import data from working directory
      getwd()
      setwd("D:/R_Projects/Specific Stage") #Set WD
      
      G_03223425_DV_MEAN <- read.delim("D:/R_Projects/Specific Stage/03223425_DV_MEAN.RDB", comment.char="#")
      G_03223425_DV_MEAN<-G_03223425_DV_MEAN[-1,]
      write.csv(G_03223425_DV_MEAN, file = "G_03223425_DV_MEAN.csv")
      G_03223425_DV_MEAN<-read.csv("D:/R_Projects/Specific Stage/G_03223425_DV_MEAN.csv")      
      
      G_03223425_UV_DISCHARGE <- read.delim("D:/R_Projects/Specific Stage/03223425_UV_DISCHARGE.RDB", comment.char="#")
      G_03223425_UV_DISCHARGE<-G_03223425_UV_DISCHARGE[-1,]
      write.csv(G_03223425_UV_DISCHARGE, file = "G_03223425_UV_DISCHARGE.csv")
      G_03223425_UV_DISCHARGE<-read.csv("D:/R_Projects/Specific Stage/G_03223425_UV_DISCHARGE.csv")      
      
      G_03223425_UV_STAGE <- read.delim("D:/R_Projects/Specific Stage/03223425_UV_STAGE.RDB", comment.char="#")
      G_03223425_UV_STAGE<-G_03223425_UV_STAGE[-1,]
      write.csv(G_03223425_UV_STAGE, file = "G_03223425_UV_STAGE.csv")
      G_03223425_UV_STAGE<-read.csv("D:/R_Projects/Specific Stage/G_03223425_UV_STAGE.csv")      
      
  
  #Combine
      G_03223425 <- merge(G_03223425_UV_DISCHARGE,G_03223425_UV_STAGE, all=TRUE) 
      rm(G_03223425_UV_DISCHARGE,G_03223425_UV_STAGE) 
  

  #Convert Dates
    G_03223425$date <- as.Date(paste(G_03223425$YEAR, G_03223425$MONTH,G_03223425$DAY, sep = "." )  , format = "%Y.%m.%d" )
    
  # ####Discharge Quantile Determination####
  #   quantiles<-quantile(G_03223425_DV_MEAN$D03223425.00060,probs=seq(0,1,0.1))
  #   quantiles 
  #   Target_Discharge<-quantiles[[5+1]]
  #   Target_Discharge

##### Checking the Proper Discharge values #####-----------------------------------------------------------
  # ggplot(data=G_03223425_DV_MEAN, aes(x=as.numeric(row.names(G_03223425_DV_MEAN)), y=D03223425.00060))+
  #   ggtitle(paste("Hollofield Hydrograph"))+
  #   geom_path(alpha=0.5)+
  #   ylab("Discharge (cfs)")+
  #   geom_hline(yintercept=Target_Discharge, color="red",size=1.5)+
  #   theme(axis.title.x=element_blank())

  

####Specific Stage Loop for each Quantile####
    quantiles<-quantile(G_03223425_DV_MEAN$D03223425.00060,probs=quantile.selection)
    quantiles<-t(as.matrix(quantiles) )
    rownames(quantiles)<-"cfs"
    print(quantiles)[1]
    
    for(i in 1:(ncol(quantiles))) {
      Discharge<-quantiles[i]
      #Run.no<-colnames(quantiles)[i]
      x1<-G_03223425$U03223425.00060
      G_03223425$x2<-Discharge
      x2<-G_03223425$x2
      # Find points where x1 is above x2.
      above<-x1>x2
      # Points always intersect when above=TRUE, then FALSE or reverse
      intersect.points<-which(diff(above)!=0)
      # Find the slopes for each line segment.
      x1.slopes<-x1[intersect.points+1]-x1[intersect.points]
      x2.slopes<-x2[intersect.points+1]-x2[intersect.points]
      # Find the intersection for each segment.
      x.points<-intersect.points + ((x2[intersect.points] - x1[intersect.points]) / (x1.slopes-x2.slopes))
      y.points<-x1[intersect.points] + (x1.slopes*(x.points-intersect.points))
      # Plot.
      #plot(x1,type='l')
      #lines(x2,type='l',col='red')
      #points(x.points,y.points,col='blue')
      
      #X1 is the estimated index number whenever discharge is x2
      
      Interceptx2<-G_03223425[x.points,]
      #plot(Interceptx2$date, Interceptx2$U03223425.00060)
      #fit<-lm(H_S~datetime, data=Interceptx2)
      #summary(fit)
      d.frame<-subset(Interceptx2,U03223425.00060>(Discharge*(1-sensitivity))&U03223425.00060<(Discharge*(1+sensitivity)))
      df.names <- assign(paste("Run", i-1,sep=""),d.frame)
    }
  #Prepare Data
    
    Runs.nos<-list(Run0, Run1, Run2, Run3, Run4, Run5, Run6, Run7, Run8, Run9, Run10)
    All.Runs <- melt(Runs.nos, id.vars = c("date","YEAR","MONTH", "DAY","MINUTE"))
    #Clean up Data from Loop
    rm(d.frame,df.names,Interceptx2,above,i,intersect.points,x.points,x1,x1.slopes,x2.slopes,y.points)
    rm(Runs.nos,Run0, Run1, Run2, Run3, Run4, Run5, Run6, Run7, Run8, Run9, Run10)

####GGPlot Discharge For All Quantiles for Quality Assurance####
    ggplot(data=subset(All.Runs,variable=="U03223425.00060"), aes(x=date,y=value,color=as.factor(L1)))+
      geom_point(alpha=0.5)+
      ggtitle("Discharge Extracted at each Quantile")+
      scale_color_discrete(name="Discharge\nQuantile",labels=colnames(quantiles))+
      scale_x_date(date_breaks = "1 year", date_labels = "%b\n%Y",date_minor_breaks = "6 months", expand=c(0,0))+
      ylab("cfs")#+
    # ylim(5,120)
    #coord_cartesian(ylim=c(0,100))+
    #geom_smooth(method="lm",alpha=0.7,se=FALSE)+
    #stat_smooth(alpha=0.5,se=FALSE)
    
####GGPlot Stage For Hydrograph####
    ggplot(data=G_03223425_DV_MEAN, aes(x=date, y=D03223425.00060))+
      ggtitle(paste("Hydrograph"))+
      geom_path(alpha=0.5)+
      ylab("Discharge (cfs)")+
      coord_trans(y="log")+
     # geom_hline(yintercept=Target_D, color="red",size=1.5)+
      theme(axis.title.x=element_blank())
    
####GGPlot Stage For All Quantiles####
    ggplot(data=subset(All.Runs,variable=="U03223425.00065"), aes(x=date,y=value,color=as.factor(L1)))+
      geom_point(alpha=0.1)+
      ggtitle("Stage at Discharge Quantiles")+
      scale_color_discrete(name="Discharge\nQuantile",labels=colnames(quantiles))+
      scale_x_date(date_breaks = "1 year", date_labels = "%b\n%Y")+
      ylab("Stage Height")+
     # ylim(5,120)
      coord_cartesian(ylim=c(0,5))+
      #geom_smooth(method="lm",alpha=0.7,se=FALSE)
      stat_smooth(alpha=0.5,se=FALSE)
      

USGSGno <- "03223425"
Target_Quantile <- 8
Target_D <-round(quantiles[Target_Quantile],2)
qplot(mpg, wt, data = mtcars) + labs(title = myTitle)
####GGPlot Stage For 8th Quantiles####
ggplot(data=subset(All.Runs,variable=="U03223425.00065"&L1==Target_Quantile), aes(x=date,y=value))+
  ggtitle(paste0("Stage at USGS Gage #", USGSGno,"\n", Target_Quantile,"th Discharge Quantile: ",Target_D, " cfs"))+
 # scale_color_discrete(name="Discharge\nQuantile",labels=colnames(quantiles))+
  scale_x_date(date_breaks = "2 year", date_labels = "%b\n%Y")+
  ylab("Stage Height (ft)")+
  #theme_grey()+
  theme(panel.grid.minor = element_blank())+
  coord_cartesian(ylim=c(0,5))+
  #geom_smooth(method="lm",alpha=0.7,se=FALSE)
  geom_point(alpha=0.1)+
  stat_smooth(method="loess",weight=1.56,size=1.2,se=TRUE,alpha=0.2,span=0.03)

