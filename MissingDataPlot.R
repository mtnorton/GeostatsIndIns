# Missing data plot
# Needs HeatIndex.R and HeatIndexCenicafe.R

library(ggplot2)

# Make table of available data in yearly format
# Color scale according to number of missing days in that year

#total_missing
#total_missing_cc

# 1st column year
# 2nd column station name
# 3rd column data

missing.df <- matrix(NA, 1829, 5)
colnames(missing.df) <- c("Year","Station","Perc_Missing", "Data_Source", "Region")

total_missing <- total_missing[,-32]

for (i in 1:nrow(total_missing))
{
  for (j in 1:ncol(total_missing))
  {
    missing.df[(j+(i-1)*31),1] <- 1982 + j
    missing.df[j+(i-1)*31,2] <- sta_names_govt[i]
    if (is.na(total_missing[i,j])) {missing.df[j+(i-1)*31,3] <- 0}
    else {missing.df[j+(i-1)*31,3] <- 1-total_missing[i,j]/365}
    missing.df[j+(i-1)*31,4] <- "Government"
    missing.df[j+(i-1)*31,5] <- sta_output[i,1]
  }
}

# Now for Cenicafe Stations

CC_regions <- read.csv("/Users/mtnorton/Coffee_Ins_Heat_Index/CC_regions.csv",header=F)

missing.df_cc <- matrix(NA, 1250, 5)
colnames(missing.df_cc) <- c("Year","Station","Perc_Missing", "Data_Source", "Region")

for (i in 1:nrow(total_missing_cc))
{
    for (j in 1:ncol(total_missing_cc))
    {
      missing.df_cc[(j+(i-1)*25),1] <- 1988 + j
      missing.df_cc[j+(i-1)*25,2] <- sta_names[i,1]
      if (is.na(total_missing_cc[i,j])) {missing.df_cc[j+(i-1)*25,3] <- 0}
      else {missing.df_cc[j+(i-1)*25,3] <- 1-total_missing_cc[i,j]/365}
      missing.df_cc[j+(i-1)*25,4] <- "Cenicafe"
      missing.df_cc[j+(i-1)*25,5] <- as.character(CC_regions[i,1])
    }
}

stations <- as.matrix(read.csv("/Users/mtnorton/Dropbox/temp/Distance measures/coffeestations.csv"))

missing.df <- rbind(missing.df,missing.df_cc)

#missing.df <- missing.df[,order(missing.df[,2])]

missing.df <- as.data.frame(missing.df)

missing.df$Station <- as.character(missing.df$Station) 
missing.df$Station <- factor(missing.df$Station, levels=rev(unique(missing.df$Station)))

ggplot(missing.df,aes(x=Year, y=Station))+
  geom_tile(aes(fill=Data_Source,color=Data_Source,alpha=Perc_Missing),na.rm=T)+
  guides(alpha="none")+
  scale_fill_manual(values = c("#3366cc","#4daf4a"))+
  scale_color_manual(values = c("#3366cc","#4daf4a"))+
  theme(axis.text.x = element_text(angle=45,size = 10,vjust=.5),
        axis.text.y=element_blank(),
        legend.position="bottom")+
  ylab("Weather Station")+
  ggtitle("Data Availability By Year, Source, and Region")+
  facet_grid(Region~.,scales="free_y",space="free_y")
  #theme(axis.text.y = element_text(size = 7))
