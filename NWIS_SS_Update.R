#Begin Run----
rm(list=ls())

#Load Packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(tidyverse, ggthemes, fs, stringr, dataRetrieval, data.table)

#Define Functions ----
read_sites <- function(Site_List) {
  sitedata <- readNWISsite(siteNumbers = Site_List)
  daily <- readNWISdv(siteNumbers = Site_List,
                      parameterCd = c("00060"),
                      startDate = "",
                      endDate = "") %>% renameNWISColumns()
  
  uvdata <- readNWISuv(siteNumbers = Site_List,
                       parameterCd = c("00060","00065"),
                       startDate = "",
                       endDate = "") %>% renameNWISColumns()
  
    NWISDir <- paste0("./NWIS_pulls/")
  dir_create(NWISDir) #Create Export directory
  fwrite(daily,paste0(NWISDir,"daily.csv"))
  fwrite(uvdata,paste0(NWISDir,"uvdata.csv"))
  fwrite(sitedata,paste0(NWISDir,"sitedata.csv"))
}
theme_ss <- function(base_size=12, base_family="sans") {
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
Plot_DV <- function(Site) {#Plot Stage and Discharge data
  ggplot(data=subset(daily, site_no == Site), aes(x=Date, y=Flow))+
    ggtitle(paste0("Mean Daily Hydrograph at USGS Gage #", Site), subtitle = subset(sitedata, site_no == Site)$station_nm)+
    theme_ss()+
    geom_line(alpha=0.5)+
    ylab("Mean Daily Discharge (cfs)")
}
Plot_UV <- function(Site) {#Plot Stage and Discharge data
  ggplot(data=subset(site_daily,site_no == Site&Flow > 0), aes(x=Date, y=Flow))+
    ggtitle(paste0("Mean Daily Hydrograph at USGS Gage #", Site), subtitle = subset(sitedata, site_no == Site)$station_nm)+
    theme_ss()+
    geom_line(alpha=0.5)+
    ylab("Mean Daily Discharge (cfs)")
}
SS <- function(Site, Quantiles) {
  site_daily <- daily %>% filter(site_no == Site)
  site_uv <- uvdata %>% filter(site_no == Site)
  
  quantiles <- quantile(site_daily$Flow, probs=Quantiles, na.rm=TRUE)
  
  Runlist = list()
  
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
    Intersect.slopes <- site_uv$Flow_Inst[intersect.index+1]-site_uv$Flow_Inst[intersect.index]
    # Find the intersection point fraction for each segment.
    x.points <- intersect.index + ((Discharge - site_uv$Flow_Inst[intersect.index]) / (Intersect.slopes))
    intersect.points <- as.data.frame(x.points)
    
    index.fraction <- intersect.points %>% #Split decimals
      separate(x.points, into = c("index","fraction"))
    
    index.fraction$fraction <- index.fraction$fraction %>% #Replace NA with 0's
      replace_na(0)
    
    index.fraction$fraction <- paste("0.", index.fraction$fraction, sep="") #Add 0. to front of decimal values
    index.fraction$secondspast <- round(as.numeric(index.fraction$fraction)*15*60)
    
    QestTime<-site_uv$dateTime[as.numeric(index.fraction$index)]+index.fraction$secondspast #Estimated time of target Q
    
    Approx_Flow <- as.tibble(approx(site_uv$dateTime, y = site_uv$Flow_Inst, xout = QestTime))  %>%
      left_join(as.tibble(approx(site_uv$dateTime, y = site_uv$GH_Inst, xout = QestTime)), by = "x") %>%
      rename(GH_Inst = y.x) %>%
      rename(Flow_Inst = y.y) %>%
      rename(dateTime = x)
    
    df.names <- assign(paste("Run", i,sep=""), Approx_Flow)
    Approx_Flow$i <- paste("Run", i,sep="")
    Runlist[[i]] <- Approx_Flow
  }
  
  #Bind quantile lists into one dataframe
  SS_bind <- bind_rows(Runlist)
  
}


#Define Gage Sites and Quantiles to Work On----
Site_List <- c("01589025","01589035")
Quantiles <- c(0.3,0.5,0.7,0.8,0.85,0.9,0.92,0.94,0.96,0.97,0.98,0.99,0.9975)

#Import data from NWIS and save to disk
system.time(read_sites(Site_List))

#Read local data
NWISDir <- paste0("./NWIS_pulls/")
daily <- fread(paste0(NWISDir,"daily.csv"), stringsAsFactors = F , colClasses=c("character","character","Date","numeric","character"))
daily$Date <- as.Date(daily$Date)
uvdata <- fread(paste0(NWISDir,"uvdata.csv"))
sitedata <- fread(paste0(NWISDir,"sitedata.csv"))

#Plot daily data
Plot_DV(Site_List[[1]])
  
SS_results <- SS(Site_List[[1]], Quantiles)



  #DV Hydrograph Plot----
  Plot1<-(ggplot(data=subset(daily,Flow>0), aes(x=Date, y=Flow))+
            ggtitle(paste0("Mean Daily Hydrograph at USGS Gage #", Site), subtitle = subset(sitedata, site_no == Site)$station_nm)+
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
  Plot0<-ggplot(data = SS_results, aes(x=dateTime, y=GH_Inst,color=as.factor(i)))+
    geom_point(alpha=0.5)+
    theme_ss()+
    ggtitle(paste0("Discharge QUantiles at USGS Gage #", Site), subtitle = subset(sitedata, site_no == Site)$station_nm)+
    guides(colour = guide_legend(override.aes = list(alpha = 1)))+
   # scale_color_discrete(name="Discharge\nQuantile",breaks=rev(i),labels=rev(paste((quantiles),":",round(quantiles,0)," cfs")))+
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
