#Begin Run----
rm(list=ls())
start.time.total <- Sys.time()

#Load Packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(tidyverse, ggthemes, fs, stringr, dataRetrieval, data.table)

#Set and create data and export directories ----
exportDir <-"~/Export/"
dir_create(exportDir) #Create Export directory

#Extract Site Numbers to Work On----
Site_List <- c("01589025","01589035")

#Import data from NWIS
daily <- readNWISdv(siteNumbers = Site_List,
           parameterCd = c("00060"),
           startDate = "",
           endDate = "") %>% renameNWISColumns()

uvdata <- readNWISuv(siteNumbers = Site_List,
           parameterCd = c("00060","00065"),
           startDate = "",
           endDate = "") %>% renameNWISColumns()

#Write data locally
NWISDir <- paste0(mainDir,"/NWIS_pulls/")
dir_create(NWISDir) #Create Export directory
fwrite(daily,paste0(NWISDir,"daily.csv"))
fwrite(uvdata,paste0(NWISDir,"uvdata.csv"))

#Set Analysis Paramaters ---- #Can this be later added interactively into a Shiny app?
sensitivity <- 0.02 #Intercept sensistivity threshold 

#Manual
  Quantiles <- c(0.3,0.5,0.7,0.8,0.85,0.9,0.92,0.94,0.96,0.97,0.98,0.99,0.9975)
#Automatic
#quantile_selection <- seq(0.1,0.9,0.1)

target_quantile <- 14 #target Number in Sequence


#Create standard ggtheme
theme_ss <- function(base_size=12, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(size = rel(1), hjust = 0.5),
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
            legend.margin = unit(0, "cm"),
            legend.title = element_text(size = rel(0.8)),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text()
    ))
  
}


SS <- function(Site, sensitivity, Quantiles, Target_Quant) {
  site_daily <- daily %>% filter(site_no == Site)
  site_uv <- uvdata %>% filter(site_no == Site)
  
  quantiles <- quantile(site_daily$Flow, probs=Quantiles, na.rm=TRUE)
  
  for(i in 1:(length(quantiles))) {
    Discharge <- quantiles[[i]]
    # Find points where Flow_Inst is above target discharge <- can this be interpolated
    # Points always intersect when above=TRUE, then FALSE or reverse 
    above<-site_uv$Flow_Inst>Discharge
    
    
    intersect.index<-which(diff(above)!=0)
    
    #Test
    x1<-site_uv$Flow_Inst
    site_uv$x2<-Discharge
    x2<-site_uv$x2
    
    # Find the slopes for each line segment.
    Intersect.slopes<-site_uv$Flow_Inst[intersect.index+1]-site_uv$Flow_Inst[intersect.index]
    # Find the intersection point fraction for each segment.
    x.points <- intersect.index + ((Discharge - site_uv$Flow_Inst[intersect.index]) / (Intersect.slopes))
    intersect.points <- as.data.frame(x.points)
    
    index.fraction<-intersect.points %>%
      separate(x.points, into = c("index","fraction"))
    index.fraction$fraction<-index.fraction$fraction %>%
      replace_na(0)#Split decimals
    
    index.fraction$fraction <- paste("0.", index.fraction$fraction, sep="") #Add 0. to front of decimal values
    index.fraction$secondspast <- round(as.numeric(index.fraction$fraction)*15*60)

    QestTime<-site_uv$dateTime[as.numeric(index.fraction$index)]+index.fraction$secondspast #Estimated time of target Q
    
    approx(site_uv$dateTime, y = site_uv$Flow_Inst, xout = QestTime) 
    
    d.frame<-subset(Interceptx2,Q>(Discharge*(1-sensitivity))&Q<(Discharge*(1+sensitivity)))
    df.names <- assign(paste("Run", i,sep=""),d.frame)
  }

  
  #Prepare Data
  Run.names<-paste0("Run",(1:ncol(quantiles)))
  Runs.nos<-list(mget((Run.names)))
  All.Runs <- melt((Runs.nos), id.vars = c("date.time"),measure.vars = c("Q","S"))
  
}

#Scrap beyond here ----
#Site Loop ----
for(j in 1:length(Site_List)){
  
  #Create Site Folder Definitions and Folders
  
  Current_Site<-Site_List[j]
  print(paste0("Starting Gage #",Current_Site, "; Site ",j," of ", length(Site_List)))
  subDir <- paste0("Output_Gage_",Current_Site)
  finalDir <- paste0(exportDir,subDir) %>%
    dir_create(finalDir) #Create data directory
  
  #Identify fiels to import
  file.list<-list.files(readDir)
  Current_Site_Files<-subset(file.list, regexpr(paste0("@",Current_Site),file.list)>0&regexpr("EntireRecord.csv",file.list)>0)
  
  #Importing dvQ, uvQ, and uvS
  Path<-subset(Current_Site_Files, regexpr("Discharge",Current_Site_Files)>0&regexpr("Mean",Current_Site_Files)>0)
  DV_MEAN <- fread(paste0(readDir,Path),skip=14)
  
  Path<-subset(Current_Site_Files, regexpr("Discharge",Current_Site_Files)>0&regexpr("Mean",Current_Site_Files)<0)
  UV_DISCHARGE <- fread(paste0(readDir,Path),skip=14)
  
  Path<-subset(Current_Site_Files, regexpr("Gage_height",Current_Site_Files)>0)
  UV_STAGE <- fread(paste0(readDir,Path),skip=14)
  
  
  #Merging uv data 
  Combined_Q_S <- merge(UV_DISCHARGE,UV_STAGE, by="ISO 8601 UTC", all=FALSE) 
  
  #Data Cleanup
  rm(UV_DISCHARGE,UV_STAGE,Path) 
  
  colnames(DV_MEAN)[which(colnames(DV_MEAN) == paste0("Value"))] <- 'Q'
  #colnames(DV_MEAN)[which(colnames(DV_MEAN) == paste0(timestamp))] <- 'date.time'
  #DV_MEAN$date.time = substr(DV_MEAN[,2],1,nchar(DV_MEAN[,2])-4)
  
  DV_MEAN$date<-DV_MEAN[,2]
  DV_MEAN$date<-as.Date(DV_MEAN$date,  format="%Y-%m-%d")
  
  
  colnames(Combined_Q_S)[which(colnames(Combined_Q_S) == paste0("Value.x"))] <- 'Q'
  colnames(Combined_Q_S)[which(colnames(Combined_Q_S) == paste0("Value.y"))] <- 'S'
  
  Combined_Q_S$date.time<-Combined_Q_S[,2]
  Combined_Q_S$date.time<-as.POSIXct(Combined_Q_S$date.time, tz=tz, format="%Y-%m-%d %H:%M:%S")
  
  
  DV_MEAN$Q[DV_MEAN$Q==-123456E20] <- NA
  DV_MEAN$Q[DV_MEAN$Q==-1.23456e+25] <- NA
  Combined_Q_S$Q[Combined_Q_S$Q==-1.23456E25] <- NA
  Combined_Q_S$S[Combined_Q_S$S==-1.23456E25] <- NA
  Combined_Q_S$S[Combined_Q_S$S>100] <- NA
  
  #Stage/Quantile Loop----
  print(paste0("Starting Quantile Selection"))
  
  quantiles<-quantile(DV_MEAN$Q,probs=quantile_selection,na.rm=TRUE)
  quantiles<-t(as.matrix(quantiles))
  
  rownames(quantiles)<-"cfs"
  
  for(i in 1:(ncol(quantiles))) {
    Discharge<-quantiles[i]
    x1<-Combined_Q_S$Q
    Combined_Q_S$x2<-Discharge
    x2<-Combined_Q_S$x2
    # Find points where x1 is above x2.
    above<-x1>x2
    print(paste0("Points Above Target Discharge: Mean Daily Quantile ", colnames(quantiles)[i]))
    table(above)
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
  All.Runs <- melt((Runs.nos), id.vars = c("date.time"),measure.vars = c("Q","S"))
  
  #DV Hydrograph Plot----
  Plot1<-(ggplot(data=subset(DV_MEAN,Q>0), aes(x=date, y=Q))+
            ggtitle(paste0("Mean Daily Hydrograph at USGS Gage #", Current_Site))+
            theme_ss()+
            geom_line(alpha=0.5)+
            ylab("Mean Daily Discharge (cfs)"))
  #coord_cartesian(ylim=c(0,3000))+
  # geom_hline(yintercept=Target_D, color="red",size=1.5))
  
  print("Plotting DV Hydrograph")
  #print(Plot1)
  name<-paste0("DV_Hydrograph_", Current_Site,".png")
  ggsave(name,path=finalDir,width=60,height=40, units="cm",scale=0.3)
  print(paste0("Export of ", name, " Complete"))
  
  #Calculate Min_Max Range for Scaling
  DVHydrograph_Range<-layer_scales(Plot1)$x$range$range
  DVHydrograph_Range.date<-as.Date(DVHydrograph_Range, origin="1970-01-01")
  DVHydrograph_Range.posixct<-as.POSIXct(DVHydrograph_Range.date)
  
  #UV Hydrograph Plot----
  print("Plotting UV Hydrograph")
  Plot1a<-(ggplot(data=subset(Combined_Q_S), aes(x=date.time, y=Q))+
             ggtitle(paste0("Unit Value Hydrograph at USGS Gage #", Current_Site))+
             theme_ss()+
             geom_line(alpha=0.5)+
             ylab("Unit Value Discharge (cfs)")+
             #coord_cartesian(ylim=c(0,3000))+
             # geom_hline(yintercept=Target_D, color="red",size=1.5)+
             scale_x_datetime(date_labels = "%b\n%Y",limits = DVHydrograph_Range.posixct))
  
  #print(Plot1a)
  name<-paste0("UV_Hydrograph_", Current_Site,".png")
  ggsave(name,path=finalDir,width=60,height=40,units="cm",scale=0.3)
  print(paste0("Export of ", name, " Complete"))
  
  print("Plotting UV Hydrograph Zoomed")
  Plot1aa<-(ggplot(data=subset(Combined_Q_S), aes(x=date.time, y=Q))+
              ggtitle(paste0("Unit Value Hydrograph at USGS Gage #", Current_Site))+
              theme_ss()+
              geom_line(alpha=0.5)+
              ylab("Unit Value Discharge (cfs)"))
  #coord_cartesian(ylim=c(0,3000))+
  # geom_hline(yintercept=Target_D, color="red",size=1.5)+
  #print(Plot1aa)
  name<-paste0("UV_Hydrograph_Zoom", Current_Site,".png")
  ggsave(name,path=finalDir,width=60,height=40,units="cm",scale=0.3)
  print(paste0("Export of ", name, " Complete"))
  
  UVHydrograph_Range<-layer_scales(Plot1aa)$x$range$range
  UVHydrograph_Range.date<-as.Date(UVHydrograph_Range, origin="1970-01-01")
  UVHydrograph_Range.posixct<-as.POSIXct(UVHydrograph_Range.date)
  
  #UV Stage Plot----
  print("Plotting UV Stage")
  Plot1b<-(ggplot(data=subset(Combined_Q_S), aes(x=date.time, y=S))+
             ggtitle(paste0("UV Stage at USGS Gage #", Current_Site))+
             theme_ss()+
             geom_line(alpha=0.5)+
             ylab("Unit Value Stage (ft.)"))
  #print(Plot1b)
  name<-paste0("UV_Stage_", Current_Site,".png")
  ggsave(name,path=finalDir,width=60,height=40,units="cm",scale=0.3)
  print(paste0("Export of ", name, " Complete"))
  
  UVStage_Range<-layer_scales(Plot1b)$x$range$range
  UVStage_Range.date<-as.Date(UVStage_Range, origin="1970-01-01")
  UVStage_Range.posixct<-as.POSIXct(UVStage_Range.date)
  
  #Discharge QA Plot----
  print("Plotting Discharge QA Plot")
  Plot0<-ggplot(data=subset(All.Runs,variable=="Q"), aes(x=date.time,y=value,color=as.factor(L2)))+
    geom_point(alpha=0.5)+
    theme_ss()+
    ggtitle(paste0("Discharge Quantiles at USGS Gage #", Current_Site))+
    guides(colour = guide_legend(override.aes = list(alpha = 1)))+
    scale_color_discrete(name="Discharge\nQuantile",breaks=rev(Run.names),labels=rev(paste(colnames(quantiles),":",round(quantiles,0)," cfs")))+
    scale_x_datetime(date_labels = "%b\n%Y")+
    ylab("UV Discharge (cfs)")
  #print(Plot0)
  name<-paste0("UVDischarge_Check_", Current_Site,".png")
  ggsave(name,path=finalDir,width=60,height=40,units="cm",scale=0.3)
  print(paste0("Export of ", name, " Complete"))
  
  
  #All Quantile Plot----
  print("Plotting All Quantile Plot")
  All.Runs$L2<-as.factor(All.Runs$L2)
  Plot2<-(ggplot(data=subset(All.Runs,variable=="S"), aes(x=date.time,y=value,color=L2))+
            geom_point(alpha=0.3)+
            ggtitle(paste0("Specific Stage for USGS Gage #", Current_Site))+
            theme_ss()+
            guides(colour = guide_legend(override.aes = list(alpha = 1)))+
            scale_color_discrete(name="Discharge\nQuantile",breaks=rev(Run.names),labels=rev(paste(colnames(quantiles),":",round(quantiles,0)," cfs")))+
            scale_x_datetime(date_labels = "%b\n%Y",date_minor_breaks = "1 year")+
            ylab("Stage (ft)"))+
    #geom_smooth(method="lm",alpha=0.7,se=FALSE)
    geom_smooth(alpha=0.5,span=0.2,se=FALSE)+
    theme(axis.title.x=element_blank())
  #print(Plot2)
  name<-paste0("All_Quant_", Current_Site,".png")
  ggsave(name,path=finalDir,width=60,height=40,units="cm",scale=0.3)
  print(paste0("Export of ", name, " Complete"))
  
  
  #Stage @ Target Quantile Plot with hydrograph----
  print("Plotting Target Quantile with hydrograph")
  Target_D <-round(quantiles[target_quantile],2)
  savename<-colnames(quantiles)[target_quantile]
  savename<-gsub("%", "pc", savename)
  
  Plot3<-(ggplot(data=subset(All.Runs,variable=="S"&L2==Run.names[target_quantile]), aes(x=date.time,y=value))+
            ggtitle(paste0("Stage at USGS Gage #", Current_Site,"\n",  colnames(quantiles)[target_quantile], " Discharge Quantile: ",Target_D, " cfs"))+
            theme_ss()+
            # scale_x_date(date_labels = "%b\n%Y",limits = Hydrograph_Range)+
            ylab("Stage (ft)")+
            #coord_cartesian(ylim=c(-1,0.5))+
            #geom_smooth(method="lm",alpha=0.7,se=FALSE)
            geom_point(alpha=0.3)+
            stat_smooth(method="loess",weight=1.56,size=1.2,se=TRUE,alpha=0.2,span=0.1))
  
  Plot1aa<-Plot1aa+scale_y_reverse()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = NA))
  library(ggplot2)
  library(gtable)
  library(grid)
  grid.newpage()
  # extract gtable
  g1 <- ggplot_gtable(ggplot_build(Plot3))
  g2 <- ggplot_gtable(ggplot_build(Plot1aa))
  
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
  #grid.draw(g)
  name<-paste0("Joint_Hydrograph_Q_Quant_",savename,"_",Current_Site,".png")
  ggsave(grid.draw(g),file=name,path=finalDir,width=60,height=40,units="cm",scale=0.3)
  print(paste0("Export of ", name, " Complete"))
  
  
  
  
  #Run for all Quantiles Plot----
  
  for (i in seq(quantiles)){
    
    target_quantile <- i
    Target_D <-round(quantiles[i],2)
    
    savename<-colnames(quantiles)[target_quantile]
    savename<-gsub("%", "pc", savename)
    
    Plot3<-(ggplot(data=subset(All.Runs,variable=="S"&L2==Run.names[target_quantile]), aes(x=date.time,y=value))+
              ggtitle(paste0("Stage at USGS Gage #", Current_Site,"\n",  colnames(quantiles)[target_quantile], " Discharge Quantile: ",Target_D, " cfs"))+
              theme(plot.title=element_text(hjust=0.5))+
              # scale_x_date(date_labels = "%b\n%Y",limits = Hydrograph_Range)+
              ylab("Stage (ft)")+
              theme(panel.grid.minor = element_blank())+
              #coord_cartesian(ylim=c(-1,0.5))+
              #geom_smooth(method="lm",alpha=0.7,se=FALSE)
              geom_point(alpha=0.3)+
              stat_smooth(method="loess",weight=1.56,size=1.2,se=TRUE,alpha=0.2,span=0.1))
    
    Plot1aa<-Plot1aa+scale_y_reverse()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = NA))
    
    library(ggplot2)
    library(gtable)
    library(grid)
    grid.newpage()
    # extract gtable
    g1 <- ggplot_gtable(ggplot_build(Plot3))
    g2 <- ggplot_gtable(ggplot_build(Plot1aa))
    
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
    ggsave(grid.draw(g),file=name,path=finalDir,width=60,height=40,units="cm",scale=0.3)
    print(paste0("Export of ", name, " Complete"))
    
    
    #     #Combined delta Quant. S & Q Plot----
    #     
    #     All.Runs.Target<-subset(All.Runs,variable=="S"&L2==Run.names[target_quantile])
    #     All.Runs.Target<-All.Runs.Target[order(as.Date(All.Runs.Target$date, format="%Y-%m-%d")),]
    #     Start_Stage<-All.Runs.Target$value[1]
    #     All.Runs.Target$SS<-All.Runs.Target$value-Start_Stage
    #     
    #     Plot4<-(ggplot(data=subset(All.Runs.Target), aes(x=date.time,y=SS))+
    #               ggtitle(paste0("Stage at USGS Gage #", Current_Site,"\n", colnames(quantiles)[target_quantile]," Discharge Quantile: ",Target_D, " cfs"))+
    #               theme(plot.title=element_text(hjust=0.5))+
    #             #  scale_x_date(date_labels = "%b\n%Y",date_minor_breaks = "6 months",limits = Hydrograph_Range)+
    #               ylab("Change in Stage (ft)")+
    #               theme(panel.grid.minor = element_blank())+
    #               #coord_cartesian(ylim=c(-1,0.5))+
    #               #geom_smooth(method="lm",alpha=0.7,se=FALSE)
    #               geom_point(alpha=0.3)+
    #               stat_smooth(method="loess",weight=1.56,size=1.2,se=TRUE,alpha=0.2,span=0.1))
    #     Plot1aa<-Plot1aa+scale_y_reverse()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = NA))
    #     library(ggplot2)
    #     library(gtable)
    #     library(grid)
    #     grid.newpage()
    #     
    #     # extract gtable
    #     g1 <- ggplot_gtable(ggplot_build(Plot4))
    #     g2 <- ggplot_gtable(ggplot_build(Plot1aa))
    #     
    #     # overlap the panel of 2nd plot on that of 1st plot
    #     pp <- c(subset(g1$layout, name == "panel", se = t:r))
    #     g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
    #                          pp$l, pp$b, pp$l)
    #     
    #     # axis tweaks
    #     ia <- which(g2$layout$name == "axis-l")
    #     ga <- g2$grobs[[ia]]
    #     ax <- ga$children[[2]]
    #     ax$widths <- rev(ax$widths)
    #     ax$grobs <- rev(ax$grobs)
    #     ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
    #     g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    #     g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
    #     
    #     # draw it
    #     grid.draw(g)
    #     name<-paste0("Joint_Hydrograph_dQ_Quant_",savename,"_",Current_Site,".png")
    #     print(paste0("Export of ", name, " Complete"))
    #     ggsave(grid.draw(g),file=name,path=finalDir,width=60,height=40,units="cm",scale=0.3)
    #     
    #   }
    # 
    
    #Finished and Time Elapsed Calculation----
    end.time.loop <- Sys.time()
    time.taken <- end.time.loop - start.time.loop
    print(paste0("Finished Gage #",Current_Site))
    print(paste0("Site Time Elapsed: ", round(time.taken,3)," sec."))
    
  }
  
}
end.time.total <- Sys.time()
time.taken.total <- end.time.total - start.time.total
print(paste0("Finished Entire Analysis"))
print(time.taken.total)
