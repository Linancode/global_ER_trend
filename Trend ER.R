library(sp)
library(raster)
library(rgdal)
library(trend)

setwd("Z:/TIF_yearly/")
fl<-list.files(pattern = "*.tif")
firs<-raster(fl[1])
for (i in 1:30) {
  r<-raster(fl[i])
  firs<-stack(firs,r)
}
fun<-function(y){
  if(length(na.omit(y))<30) return(c(NA,NA,NA))
  av<-mean(y,na.r=T)
  MK_estimate<-sens.slope(ts(na.omit(y),start = 1989,end = 2018,frequency = 1),conf.level=0.95)
  slope<-MK_estimate$estimate
  mktest<-MK_estimate$p.value
  return(c(av,slope,mktest))
}
e<-calc(firs,fun)
MEAN<-subset(e,1)
SLOPE<-subset(e,2)
MKTEST<-subset(e,3)

writeRaster(MEAN,"Z:/mean_ER.tif",format="GTiff",overwrite=TRUE)
writeRaster(SLOPE,"Z:/Senslope_ER.tif",format="GTiff",overwrite=TRUE)
writeRaster(MKTEST,"Z:/MK_Pvalue_ER.tif",format="GTiff",overwrite=TRUE)


#############plot mean ER
library(raster)
library(ggplot2)
library(cetcolor)
library(cowplot)

tif<- raster("Z:/mean_ER.tif")
data_ER<-as.data.frame(tif,xy=T)

p1<-ggplot(data_ER)+geom_raster(aes(x = x, y = y, fill = mean_ER_1989_2018),
                                interpolate=TRUE)+
  theme_bw() +
  scale_fill_gradientn(colours =cet_pal(5, name = "rainbow"),
                       na.value= "transparent",
                       guide = "colourbar",
                       aesthetics = "fill",
                       limits=c(0,2800),breaks = round(seq(0,2800,length.out = 5),700))+
  # theme(plot.background = element_blank(),legend.position = "none",
  #       strip.text = element_text(size = 15, colour = "black"),
  #       axis.text = element_text(size = 12, colour = "black"))+
  theme(plot.background = element_blank(),legend.position =c(0.4,-0.2),legend.direction = "horizontal",
        legend.spacing.x = unit(0.5, 'cm'),legend.key.size = unit(0.7,'cm'), legend.key.width = unit(1.5,'cm'),
        legend.text = element_text(size = 13),
        legend.title = element_text( size=14),
        strip.text = element_text(size = 16, colour = "black"),
        axis.text = element_text(size = 15, colour = "black"))+
  scale_y_continuous(breaks = seq(-60, 60, by = 30), 
                     labels = paste0(seq(-60, 60, 30), "°")) +
  scale_x_continuous(breaks = seq(-180,180,60), 
                     labels = paste0(seq(-180,180,60), "°")) +
  labs(x = "", y = "")+
  coord_quickmap(expand = F)+
  borders(colour = "black", size = 0.5)+
  labs(fill=expression(atop("Annual mean ER",(g~C~m^{-2}~y^{-1}))))+
  labs(title =  "DT-RH")

p<-ggdraw() +draw_plot(p1, 0, 0.15, 0.9, 0.9)
ggsave(file="Z:/Fig_1.1.jpg",p,width =12,height = 6,dpi = 600)


################### relative importance
library(readxl)
library(rfPermute)
library(rwa)

data<-read_excel("Z:/Explanatory variable.xlsx",sheet = 1)

x<-data[,c(2,4,8,10,13,14)]

rfP <- rfPermute(ER_DT_RH~.,data=x, ntree = 500,mtry=3, nrep = 1000, num.cores = 1)
plot(rp.importance(rfP, scale = TRUE))
rp.importance(rfP)

model<-rwa(x,outcome = "ER_DT_RH",predictors = c("Tair", "Pre", "SWC", "SPEI","Rg"))
model$rsquare
model$result



