rm(list=ls())
start.time.total <- Sys.time()
#Load Packages

require(reshape2) || install.packages("reshape2")
require(ggplot2) || install.packages("ggplot2")

#Choose Sensitivity Threshold
  sensitivity<-0.02

#Choose Quantiles
  #quantile.selection<-seq(0.1,0.9,0.1) #Automatic Range
  quantile.selection<-c(0.3,0.5,0.7,0.8,0.85,0.9,0.92,0.94,0.96,0.97,0.98,0.99) #Manual Range
  Target_Quantile <- 8 #target Number in Sequence

#Data Preparation----
#Import data from working directory
  getwd()
  setwd("D:/R_Projects/Specific Stage") #Set WD
  
  mainDir <- "D:/R_Projects/Specific Stage"

#Extract Site Numbers to Work On
   
    #Site_List <-"03223425"
    SiteInfo <- read.csv("D:/R_Projects/Specific Stage/Current_Site_List.csv",colClasses="character")
    #Site_Subset <- subset(SiteInfo,RSQA=="SESQA")
    Site_List <- SiteInfo$STAID

#Site Loop
for(j in 1:length(Site_List)){
  
  Current_Site<-Site_List[j]
  print(paste0("Starting Gage #",Current_Site))
  subDir <- paste0("Output_Gage_",Current_Site)
  
  if (file.exists(subDir)){  } else {
    dir.create(file.path(mainDir, subDir))  }
  
  #Import Files and Format as needed
  start.time.loop <- Sys.time()
  
  print(paste0("Importing Daily Mean Values"))
  Path<-paste0(Current_Site,"_MDV.RDB")
  DV_MEAN <- read.delim(paste0("D:/R_Projects/Specific Stage/",Path), comment.char="#")
  DV_MEAN<-DV_MEAN[-1,]
  write.csv(DV_MEAN, file = "DV_MEAN.csv")
  DV_MEAN<-read.csv("D:/R_Projects/Specific Stage/DV_MEAN.csv")      
  
  print(paste0("Importing Instantaneous Stage Values"))
  Path<-paste0(Current_Site,"_UV_STAGE.RDB")
  UV_STAGE <- read.delim(paste0("D:/R_Projects/Specific Stage/",Path), comment.char="#")
  UV_STAGE<-UV_STAGE[-1,]
  write.csv(UV_STAGE, file = "UV_STAGE.csv")
  UV_STAGE<-read.csv("D:/R_Projects/Specific Stage/UV_STAGE.csv")      
  
  print(paste0("Importing Instantaneous Discharge Values"))
  Path<-paste0(Current_Site,"_UV_DISCHARGE.RDB")
  UV_DISCHARGE <- read.delim(paste0("D:/R_Projects/Specific Stage/",Path), comment.char="#")
  UV_DISCHARGE<-UV_DISCHARGE[-1,]
  write.csv(UV_DISCHARGE, file = "UV_DISCHARGE.csv")
  UV_DISCHARGE<-read.csv("D:/R_Projects/Specific Stage/UV_DISCHARGE.csv")      
  
  #Combine
  print(paste0("Merging Instantaneous Values"))
  Combined_Q_S <- merge(UV_DISCHARGE,UV_STAGE, all=TRUE) 
  rm(UV_DISCHARGE,UV_STAGE,Path) 
  
  #Convert Dates
  Combined_Q_S$date <- as.Date(paste(Combined_Q_S$YEAR, Combined_Q_S$MONTH,Combined_Q_S$DAY, sep = "." ), format = "%Y.%m.%d" )
  DV_MEAN$date <- as.Date(paste(DV_MEAN$YEAR, DV_MEAN$MONTH,DV_MEAN$DAY, sep = "." )  , format = "%Y.%m.%d" )
  

#Stage/Quantile Loop----
  print(paste0("Starting Quantile Selection"))
  
  vectorQ<-paste0("DV_MEAN$",Current_Site,".00060")
  colnames(DV_MEAN)[which(colnames(DV_MEAN) == paste0("D",Current_Site,".00060"))] <- 'Q'
  quantiles<-quantile(DV_MEAN$Q,probs=quantile.selection)
  quantiles<-t(as.matrix(quantiles) )
  rownames(quantiles)<-"cfs"
  
  for(i in 1:(ncol(quantiles))) {
    Discharge<-quantiles[i]
    vectorQ<-paste0("Combined_Q_S$",Current_Site,".00060")
    colnames(Combined_Q_S)[which(colnames(Combined_Q_S) == paste0("U",Current_Site,".00060"))] <- 'Q'
    colnames(Combined_Q_S)[which(colnames(Combined_Q_S) == paste0("U",Current_Site,".00065"))] <- 'S'
     x1<-Combined_Q_S$Q
    Combined_Q_S$x2<-Discharge
    x2<-Combined_Q_S$x2
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
    Interceptx2<-Combined_Q_S[x.points,]
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

#Discharge QA Plot----
  print(ggplot(data=subset(All.Runs,variable=="Q"), aes(x=date,y=value,color=as.factor(L2)))+
          geom_point(alpha=0.5)+
          ggtitle(paste0("Discharge Quantiles at USGS Gage #", Current_Site))+
          guides(colour = guide_legend(override.aes = list(alpha = 1)))+
          scale_color_discrete(name="Discharge\nQuantile",breaks=rev(Run.names),labels=rev(paste(colnames(quantiles),":",round(quantiles,0)," cfs")))+
          scale_x_date(date_breaks = "1 year", date_labels = "%b\n%Y",date_minor_breaks = "6 months", expand=c(0,0))+
          ylab("cfs"))
  name<-paste0("Discharge_Check_", Current_Site,".png")
  ggsave(name,path=(subDir),width=60,height=40,units="cm",scale=0.3)
  
#Hydrograph Plot----
      Plot1<-(ggplot(data=subset(DV_MEAN,Q>0), aes(x=date, y=Q))+
              ggtitle(paste0("Hydrograph at USGS Gage #", Current_Site))+
              geom_path(alpha=0.5)+
              ylab("Discharge (cfs)")+
              #coord_cartesian(ylim=c(0,3000))+
              # geom_hline(yintercept=Target_D, color="red",size=1.5)+
              theme(axis.title.x=element_blank()))
        print(Plot1)
        name<-paste0("Hydrograph_", Current_Site,".png")
        ggsave(name,path=(subDir),width=60,height=40,units="cm",scale=0.3)
        print(paste0("Export of ", name, " Complete"))
      
#All Quantile Plot----
     All.Runs$L2<-as.factor(All.Runs$L2)
     Plot2<-(ggplot(data=subset(All.Runs,variable=="S"), aes(x=date,y=value,color=L2))+
            geom_point(alpha=0.5)+
            ggtitle(paste0("Specific Stage for USGS Gage #", Current_Site))+
            guides(colour = guide_legend(override.aes = list(alpha = 1)))+
            scale_color_discrete(name="Discharge\nQuantile",breaks=rev(Run.names),labels=rev(paste(colnames(quantiles),":",round(quantiles,0)," cfs")))+
            scale_x_date(date_breaks = "1 year", date_labels = "%b\n%Y")+
            ylab("Stage Height"))
            # ylim(5,120)
           # coord_cartesian(ylim=c(0,5)))
  #geom_smooth(method="lm",alpha=0.7,se=FALSE)
  # geom_smooth(method="loess",alpha=0.5,se=FALSE))
  print(Plot2)
  name<-paste0("All_Quant_", Current_Site,".png")
  ggsave(name,path=(subDir),width=60,height=40,units="cm",scale=0.3)
  print(paste0("Export of ", name, " Complete"))
  
  
#Stage @ Target Quantile Plot----

  Target_D <-round(quantiles[Target_Quantile],2)
  Plot3<-(ggplot(data=subset(All.Runs,variable=="S"&L2==Run.names[Target_Quantile]), aes(x=date,y=value))+
            ggtitle(paste0("Stage at USGS Gage #", Current_Site,"\n",  colnames(quantiles)[Target_Quantile], " Discharge Quantile: ",Target_D, " cfs"))+
            scale_x_date(date_breaks = "2 year", date_labels = "%b\n%Y")+
            ylab("Stage Height (ft)")+
            theme(panel.grid.minor = element_blank())+
            #coord_cartesian(ylim=c(-1,0.5))+
            #geom_smooth(method="lm",alpha=0.7,se=FALSE)
            geom_point(alpha=0.1)+
            stat_smooth(method="loess",weight=1.56,size=1.2,se=TRUE,alpha=0.2,span=0.05))
  print(Plot3)
  name<-paste0("Stage_Quant_",Target_Quantile, "_",Current_Site,".png")
  ggsave(name,path=(subDir),width=60,height=40,units="cm",scale=0.3)
  print(paste0("Export of ", name, " Complete"))
  
#Combined Quant. S & Q Plot----
  
  All.Runs.Target<-subset(All.Runs,variable=="S"&L2==Run.names[Target_Quantile])
  All.Runs.Target<-All.Runs.Target[order(as.Date(All.Runs.Target$date, format="%Y-%m-%d")),]
  Start_Stage<-All.Runs.Target$value[1]
  All.Runs.Target$SS<-All.Runs.Target$value-Start_Stage
  
  Plot4<-(ggplot(data=subset(All.Runs.Target), aes(x=date,y=SS))+
            ggtitle(paste0("Stage at USGS Gage #", Current_Site,"\n", colnames(quantiles)[Target_Quantile]," Discharge Quantile: ",Target_D, " cfs"))+
            scale_x_date(date_breaks = "2 year", date_labels = "%b\n%Y")+
            ylab("Change in Stage Height (ft)")+
            theme(panel.grid.minor = element_blank())+
            coord_cartesian(ylim=c(-1,0.5))+
            #geom_smooth(method="lm",alpha=0.7,se=FALSE)
            geom_point(alpha=0.1)+
            stat_smooth(method="loess",weight=1.56,size=1.2,se=TRUE,alpha=0.2,span=0.05))
  
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
  name<-paste0("Joint_SS_Q_Quant_",Target_Quantile,Current_Site,".png")
  print(paste0("Export of ", name, " Complete"))
  ggsave(grid.draw(g),file=name,path=subDir,width=60,height=40,units="cm",scale=0.3)

  
#Cleanup
  
  rm(d.frame,df.names,Interceptx2,above,i,intersect.points,x.points,x1,x1.slopes,x2.slopes,y.points,x2,Discharge,Runs.nos)
  rm(list=(Run.names))
  rm(g,g1,g2,ia,ga,ax,Plot1,Plot2,Plot3,Plot4,pp,Start_Stage,Target_D)
  

    
#Finished and Time Elapsed Calculation----
  end.time.loop <- Sys.time()
  time.taken <- end.time.loop - start.time.loop
  print(paste0("Finished Gage #",Current_Site))
  print(paste0("Site Time Elapsed: ", round(time.taken,3)," sec."))
  
}
    
end.time.total <- Sys.time()
  time.taken.total <- end.time.total - start.time.total
  print(paste0("Finished Entire Analysis"))
  print(time.taken.total)
