#Load Libraries 
library(ggplot2)
library(reshape2)

#Data Preparation----
rm(list=ls())

#Choose Sensitivity Threshold
sensitivity<-0.02
quantile.selection<-c(0.3,0.5,0.7,0.8,0.85,0.9,0.92,0.94,0.96,0.97,0.98,0.99) #Manual Range

#Import data from working directory
getwd()
setwd("D:/R_Projects/Specific Stage/") #Set WD
mainDir <- "D:/R_Projects/Specific Stage/"

#Problem Site Selection
Current_Site <-"03272100"
Target_Quantile<-7

#Difficult Run Cutoff
Current_Site <- "01645704"
print(paste0("Importing Gage Data for Difficult Run #", Current_Site, " Above Fox Lake Near Fairfax, VA"))
Site_Data <- read.delim(paste0("D:/R_Projects/Specific Stage/Difficult Run/","UDR_GH.RDB"), comment.char="#")
Site_Data<-Site_Data[-1,]
write.csv(Site_Data, file = "UDR.csv")
Site_Data<-read.csv("D:/R_Projects/Specific Stage/Difficult Run/UDR.csv")      


##### ANALYSIS RUNS BELOW #####
print(paste0("Starting Gage #", Current_Site))
subDir <- paste0("Output_Gage_",Current_Site)

if (file.exists(subDir)){  } else {
  dir.create(file.path(mainDir, subDir))  }

#Import Files and Format as needed
start.time.loop <- Sys.time()

# print(paste0("Importing Daily Mean Values"))
# Path<-paste0(Current_Site,"_MDV.RDB")
# DV_MEAN <- read.delim(paste0("D:/R_Projects/Specific Stage/",Path), comment.char="#")
# DV_MEAN<-DV_MEAN[-1,]
# write.csv(DV_MEAN, file = "DV_MEAN.csv")
# DV_MEAN<-read.csv("D:/R_Projects/Specific Stage/DV_MEAN.csv")      


#Convert Dates
Site_Data$date <- as.Date(paste(Site_Data$YEAR, Site_Data$MONTH,Site_Data$DAY, sep = "." )  , format = "%Y.%m.%d" )
colnames(Site_Data)[which(colnames(Site_Data) == paste0("U",Current_Site,".00065"))] <- 'S'
    #Clean up Negative Error Code
        Site_Data$S[Site_Data$S==-123456E20] <- NA
Site_Data$date <- as.Date(paste(Site_Data$YEAR, Site_Data$MONTH,Site_Data$DAY, sep = "." )  , format = "%Y.%m.%d" )
##Plot Data for All data sources

ggplot(data=subset(Site_Data), aes(x=date, y=S))+
  ggtitle(paste0("Unit Value Stage Record at Upper Difficult Run"))+
  theme(plot.title=element_text(hjust=0.5))+
  geom_line(alpha=0.5)+
  ylab("Unit Stage (feet)")+
  #coord_cartesian(ylim=c(0,3000))+
  # geom_hline(yintercept=Target_D, color="red",size=1.5)+
  theme(axis.title.x=element_blank())

colnames(Site_Data)[which(colnames(Site_Data) == paste0("U",Current_Site,".00060"))] <- 'Q'
Site_Data$Q[Site_Data$Q==-123456E20] <- NA
Site_Data$date <- as.Date(paste(Site_Data$YEAR, Site_Data$MONTH,Site_Data$DAY, sep = "." )  , format = "%Y.%m.%d" )

ggplot(data=subset(Site_Data,Q>0), aes(x=date, y=Q))+
  ggtitle(paste0("Unit Value Discharge Record at Upper Difficult Run"))+
  theme(plot.title=element_text(hjust=0.5))+
  geom_line(alpha=0.5)+
  ylab("Unit Discharge (Cfs)")+
  #coord_cartesian(ylim=c(0,3000))+
  # geom_hline(yintercept=Target_D, color="red",size=1.5)+
  theme(axis.title.x=element_blank())

      #Stage/Quantile Loop----
      print(paste0("Starting Quantile Selection"))
      
      quantiles<-quantile(Site_Data$Q,probs=quantile.selection,na.rm=TRUE)
      quantiles<-t(as.matrix(quantiles) )
      rownames(quantiles)<-"cfs"
      
      for(i in 1:(ncol(quantiles))) {
        Discharge<-quantiles[i]
        vectorQ<-paste0("Site_Data$",Current_Site,".00060")
        colnames(Site_Data)[which(colnames(Site_Data) == paste0("U",Current_Site,".00060"))] <- 'Q'
        colnames(Site_Data)[which(colnames(Site_Data) == paste0("U",Current_Site,".00065"))] <- 'S'
        x1<-Site_Data$Q
        Site_Data$x2<-Discharge
        x2<-Site_Data$x2
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
        Interceptx2<-Site_Data[x.points,]
        #plot(Interceptx2$date, Interceptx2$U03223425.00060)
        #fit<-lm(H_S~datetime, data=Interceptx2)
        #summary(fit)
        d.frame<-subset(Interceptx2,Q>(Discharge*(1-sensitivity))&Q<(Discharge*(1+sensitivity)))
        df.names <- assign(paste("Run", i,sep=""),d.frame)
      }
      
      #Prepare Data
      Run.names<-paste0("Run",(1:ncol(quantiles)))
      Runs.nos<-list(mget((Run.names)))
      All.Runs <- melt((Runs.nos), id.vars = c("date","YEAR","MONTH", "DAY","MINUTE"))
      
      #Stage Plot----
      Plot0<-(ggplot(data=subset(Site_Data,Q>0), aes(x=date, y=S))+
                ggtitle(paste0("Hydrograph at USGS Gage #", Current_Site))+
                theme(plot.title=element_text(hjust=0.5))+
                geom_path(alpha=0.5)+
                ylab("Unit Value Stage (feet)")+
                #coord_cartesian(ylim=c(0,3000))+
                # geom_hline(yintercept=Target_D, color="red",size=1.5)+
                theme(axis.title.x=element_blank()))
      print(Plot0)
      name<-paste0("UVStage_", Current_Site,".png")
      ggsave(name,path=(subDir),width=60,height=40,units="cm",scale=0.3)
      print(paste0("Export of ", name, " Complete"))
      
      #Hydrograph Plot----
      Plot1<-(ggplot(data=subset(Site_Data,Q>0), aes(x=date, y=Q))+
                ggtitle(paste0("Hydrograph at USGS Gage #", Current_Site))+
                theme(plot.title=element_text(hjust=0.5))+
                geom_path(alpha=0.5)+
                ylab("Unit Value Discharge (cfs)")+
                #coord_cartesian(ylim=c(0,3000))+
                # geom_hline(yintercept=Target_D, color="red",size=1.5)+
                theme(axis.title.x=element_blank()))
      print(Plot1)
      name<-paste0("UVHydrograph_", Current_Site,".png")
      ggsave(name,path=(subDir),width=60,height=40,units="cm",scale=0.3)
      print(paste0("Export of ", name, " Complete"))
      
      #Calculate Min_Max Range for Scaling
      Hydrograph_Range<-layer_scales(Plot1)$x$range$range
      Hydrograph_Range<-as.Date(Hydrograph_Range, origin="1970-01-01")
      
      #Discharge QA Plot----
      print(ggplot(data=subset(All.Runs,variable=="Q"), aes(x=date,y=value,color=as.factor(L2)))+
              geom_point(alpha=0.5)+
              ggtitle(paste0("Discharge Quantiles at USGS Gage #", Current_Site))+
              theme(plot.title=element_text(hjust=0.5))+
              guides(colour = guide_legend(override.aes = list(alpha = 1)))+
              scale_color_discrete(name="Discharge\nQuantile",breaks=rev(Run.names),labels=rev(paste(colnames(quantiles),":",round(quantiles,0)," cfs")))+
              scale_x_date(date_breaks = "1 year", date_labels = "%b\n%Y",date_minor_breaks = "6 months",limits = Hydrograph_Range)+
              ylab("UV Discharge cfs"))
      name<-paste0("UVDischarge_Check_", Current_Site,".png")
      ggsave(name,path=(subDir),width=60,height=40,units="cm",scale=0.3)
      
      #All UV Quantile Plot----
      All.Runs$L2<-as.factor(All.Runs$L2)
      Plot2<-(ggplot(data=subset(All.Runs,variable=="S"), aes(x=date,y=value,color=L2))+
                geom_point(alpha=0.5)+
                ggtitle(paste0("Specific Stage for USGS Gage #",Current_Site))+
                theme(plot.title=element_text(hjust=0.5))+
                guides(colour = guide_legend(override.aes = list(alpha = 1)))+
                scale_color_discrete(name="Discharge\nQuantile",breaks=rev(Run.names),labels=rev(paste(colnames(quantiles),":",round(quantiles,0)," cfs")))+
                scale_x_date(date_breaks = "1 year", date_labels = "%b\n%Y",date_minor_breaks = "6 months",limits = Hydrograph_Range)+
                ylab("Stage Height"))+
                stat_smooth(method="loess",weight=1.56,size=1.2,se=TRUE,alpha=0.2,span=0.1)

      print(Plot2)
      name<-paste0("All_UV_Quant_", Current_Site,".png")
      ggsave(name,path=(subDir),width=60,height=40,units="cm",scale=0.3)
      print(paste0("Export of ", name, " Complete"))
     
      
      #Stage @ Target Quantile Plot----
      
  for (i in seq(quantiles)){
      Target_Quantile <- i
      Target_D <-round(quantiles[i],2)
      
      savename<-colnames(quantiles)[Target_Quantile]
      savename<-gsub("%", "pc", savename)
      
      Plot3<-(ggplot(data=subset(All.Runs,variable=="S"&L2==Run.names[Target_Quantile]), aes(x=date,y=value))+
                ggtitle(paste0("Stage at USGS Gage #", Current_Site,"\n",  colnames(quantiles)[Target_Quantile], " Discharge Quantile: ",Target_D, " cfs"))+
                theme(plot.title=element_text(hjust=0.5))+
                scale_x_date(date_breaks = "1 year", date_labels = "%b\n%Y",date_minor_breaks = "6 months",limits = Hydrograph_Range)+
                ylab("Stage Height (ft)")+
                theme(panel.grid.minor = element_blank())+
                #coord_cartesian(ylim=c(-1,0.5))+
                #geom_smooth(method="lm",alpha=0.7,se=FALSE)
                geom_point(alpha=0.3)+
                stat_smooth(method="loess",weight=1.56,size=1.2,se=TRUE,alpha=0.2,span=0.1))
      
      Plot1<-Plot1+scale_y_reverse()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = NA))
      
      library(ggplot2)
      library(gtable)
      library(grid)
      grid.newpage()
      # extract gtable
      g1 <- ggplot_gtable(ggplot_build(Plot3))
      g2 <- ggplot_gtable(ggplot_build(Plot1))
      
      # overlap the panel of 2nd plot on that of 1st plot
      pp <- c(subset(g1$layout, name == "panel", se = t:r))
      g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                           pp$l, pp$b, pp$l)
      
      # axis tweaks
      ia <- which(g2$layout$name == "axis-l")
      ga <- g2$grobs[[ia]]
      ax <- ga$children[[2]]
      ax$widths <- rev(ax$widths)
      ax$grobs <- rev(ax$grobs)
      ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
      g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
      g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
      
      # draw it
      grid.draw(g)
      name<-paste0("Joint_Hydrograph_Q_Quant_",savename,"_",Current_Site,".png")
      ggsave(grid.draw(g),file=name,path=subDir,width=60,height=40,units="cm",scale=0.3)
      print(paste0("Export of ", name, " Complete"))
      
      
      #Combined delta Quant. S & Q Plot----
      
      All.Runs.Target<-subset(All.Runs,variable=="S"&L2==Run.names[Target_Quantile])
      All.Runs.Target<-All.Runs.Target[order(as.Date(All.Runs.Target$date, format="%Y-%m-%d")),]
      Start_Stage<-All.Runs.Target$value[1]
      All.Runs.Target$SS<-All.Runs.Target$value-Start_Stage
      
      Plot4<-(ggplot(data=subset(All.Runs.Target), aes(x=date,y=SS))+
                ggtitle(paste0("Stage at USGS Gage #", Current_Site,"\n", colnames(quantiles)[Target_Quantile]," Discharge Quantile: ",Target_D, " cfs"))+
                theme(plot.title=element_text(hjust=0.5))+
                scale_x_date(date_breaks = "1 year", date_labels = "%b\n%Y",date_minor_breaks = "6 months",limits = Hydrograph_Range)+
                ylab("Change in Stage Height (ft)")+
                theme(panel.grid.minor = element_blank())+
                coord_cartesian(ylim=c(-1,0.5))+
                #geom_smooth(method="lm",alpha=0.7,se=FALSE)
                geom_point(alpha=0.1)+
                stat_smooth(method="loess",weight=1.56,size=1.2,se=TRUE,alpha=0.2,span=0.1))
      Plot1<-Plot1+scale_y_reverse()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = NA))
      library(ggplot2)
      library(gtable)
      library(grid)
      grid.newpage()
      
      # extract gtable
      g1 <- ggplot_gtable(ggplot_build(Plot4))
      g2 <- ggplot_gtable(ggplot_build(Plot1))
      
      # overlap the panel of 2nd plot on that of 1st plot
      pp <- c(subset(g1$layout, name == "panel", se = t:r))
      g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                           pp$l, pp$b, pp$l)
      
      # axis tweaks
      ia <- which(g2$layout$name == "axis-l")
      ga <- g2$grobs[[ia]]
      ax <- ga$children[[2]]
      ax$widths <- rev(ax$widths)
      ax$grobs <- rev(ax$grobs)
      ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
      g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
      g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
      
      # draw it
      grid.draw(g)
      name<-paste0("Joint_Hydrograph_dQ_Quant_",savename,"_",Current_Site,".png")
      print(paste0("Export of ", name, " Complete"))
      ggsave(grid.draw(g),file=name,path=subDir,width=60,height=40,units="cm",scale=0.3)
      
  }
      
#Finished and Time Elapsed Calculation----
  end.time.loop <- Sys.time()
      time.taken <- end.time.loop - start.time.loop
      print(paste0("Finished Gage #",Current_Site))
      print(paste0("Analysis Run in..."))
      print(time.taken)
      