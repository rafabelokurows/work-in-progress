library(ggplot2)
library(tidyr)
library(dplyr)
library(tibble)
#https://stackoverflow.com/questions/62556246/how-to-plot-the-variant-circular-bar-chart-in-r-with-ggplot
weatherporto <- get_GSOD(years = c(2016,2017,2018,2019,2020), station = "085450-99999")
weatherporto$rained = ifelse(weatherporto$PRCP == 0 | is.na(weatherporto$PRCP),"0","1")
nrow(weatherporto)
teste2 = data.frame(year = as.factor(weatherporto$YEAR),
                    day=as.factor(weatherporto$YDAY),
                    temp=weatherporto$TEMP)
row_num3 = length(levels(teste2$year))

g3 = ggplot(teste2,aes(x=day,y=as.numeric(year),fill=temp)) + 
  xlim(c("",unique(teste2$day))) +
  ylim(c(-row_num3/1.5,row_num3+1))+
  scale_fill_gradientn(colours = rev(colorsporto[1:7]))+
  geom_tile()+ ylab("") +
  annotate(x="",y=1:row_num3,label=levels(teste2$year),size=2.5,geom="text") 

g3 + coord_polar(start=0) + theme_bw() + 
  theme(legend.position = c(0.5, 0.5),legend.key.size = unit(0.2, "cm"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + labs(fill = "Temperature")

dev.off()


df = data.frame(mat) %>% 
  rownames_to_column("row") %>% 
  pivot_longer(-row) %>%
  mutate(name=factor(name,levels=colnames(mat)),
         row=factor(row,levels=rownames(mat)))


teste = data.frame(MONTH = as.factor(tbar_porto$MONTH),DAY=as.factor(tbar_porto$DAY),TEMP=tbar_porto$TEMP)
str(teste)
row_num = length(levels(df$row))
row_num2 = length(levels(teste$MONTH))

g2 = ggplot(teste,aes(x=DAY,y=as.numeric(MONTH),fill=TEMP)) + 
  xlim(c("",unique(teste$DAY))) + ylim(c(-row_num2/1.5,row_num2+1))+
  scale_fill_gradientn(colours = rev(colorsporto[1:7]))+
  geom_tile()+ ylab("")+
  annotate(x="",y=1:row_num2,label=levels(teste$MONTH),size=2.5,geom="text") 


g = ggplot(df,aes(x=name,y=as.numeric(row),fill=value)) + 
  xlim(c("",colnames(mat))) + ylim(c(-row_num/1.5,row_num+1))+
  geom_tile()+ ylab("")+
  annotate(x="",y=1:row_num,label=levels(df$row),size=2.5,geom="text") 
g
g2 + coord_polar(start=-0.10) + theme_bw() + 
  theme(legend.position = c(0.5, 0.5),legend.key.size = unit(0.2, "cm")) 
