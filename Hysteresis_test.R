if (!require("pacman")) install.packages("pacman"); library(pacman)

p_load(smwrData, DVstats, tidyverse)
data(ChoptankFlow)
# Process by calendar year as that is the retrieval range
ChopPart <- with(ChoptankFlow, hysep(Flow, datetime, da=113,STAID="01491000"))
ChopPart
lines(df[1:366,"Qw"],df[1:366,"Qs"],
        lwd=2        )
s <- seq(from=1, to=length(df[,"Qw"])-1, by=22)
arrows(df[,"Qw"][s], df[,"Qs"][s], df[,"Qw"][s+1], df[,"Qs"][s+1], 
        length=0.1,
        code=2,
        lwd=2
        )

himid <- function(x){

mid <- function(x) 0.5*(max(x, na.rm = T) – min(x, na.rm = T)) + min(x, na.rm = T)

target <- mid(x$q)
idx 0)

f <- function(i, target) approx(c(x$q[i], x$q[i+1]), c(x$ssc[i], x$ssc[i+1]), xout=target)$y
yp <- sapply(idx, f, target = target)

if (!length(yp) == 2) {
h <- NA
return(h)
} else {

if (yp[1] < yp[2]) {
h <- -1/(yp[1]/yp[2]) + 1
return(h)
} else {

h <- (yp[1]/yp[2]) – 1

return(h)
}
}
}
install.packages("devtools")
devtools::install_github("USGS-R/Rainmaker")
library(Rainmaker)


## Group and s

#Calculate and scale turbidity and flow
hyst_data <- Q_Storm  %>%
  na.omit() %>%
  select(site_no,dateTime,Q_cfs,GH_Inst,Turb_Inst,elev_GH,Qual_storm,EventID) %>%
  group_by(EventID) %>%
  mutate(scale_turb = (Turb_Inst-min(Turb_Inst))/(max(Turb_Inst)-min(Turb_Inst))) %>%
  mutate(scale_Q = (Q_cfs-min(Q_cfs))/(max(Q_cfs)-min(Q_cfs))) %>%
  na.omit() %>%
  mutate(max_turb = max(Turb_Inst)) %>%
  filter(max_turb > 100)

#Plot 
for (i in unique(hyst_data$EventID)) {
 plot_data <- hyst_data %>%
    filter(EventID == i )
  
 lab_dates <- pretty(plot_data$dateTime)
 label_Q <- max(plot_data$Q_cfs)
 label_Turb <- max(plot_data$Turb_Inst)
 
   p + annotate("text", x = 4, y = 25, label = "Some text")
 
  plot<- ggplot(plot_data, aes(x=scale_Q,y=scale_turb, color=as.numeric(dateTime)))+
  geom_path(size=2, alpha = 0.7)+
  geom_segment(aes(xend=c(tail(x, n=-1), NA), yend=c(tail(y, n=-1), NA)),
                 arrow=arrow(length=unit(0.3,"cm"))) + 
  geom_point(alpha = 0.2)+
  scale_color_viridis_c(breaks = as.numeric(lab_dates), 
                        labels = lab_dates)+
    annotate("text", label = paste0("Storm Max ", label_Q," cfs", "\nStorm Turb ", label_Turb, " FNU",
                                    "\n Storm Event ", i, " of ", max(hyst_data$EventID)), x=0.2,y=0.8)
 print(plot)
}


#Calculate flow quantiles within each storm
Qi = k(Qmax-Qmin)+Qmax

#Calculate Hysteresis value based on quantile

HIQ = CRL_Qi - CFL_Qi


#Calculate Loop area
library(geometry)
polyarea(x=Data$Q, y=Data$DOC)


#Calculate shape clockwise/counterclockwise number etc
