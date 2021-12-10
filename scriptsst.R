library(stars)
library(ggplot2)
library(sf)
library(tidyr)
path_data<-('./Environmental/SST/')
sst1<-read_ncdf(paste0(path_data,'METOFFICE-GLO-SST-L4-REP-OBS-SST_1633110411621.nc'),
                ignore_bounds = TRUE,make_time = TRUE)
sst1 <- st_transform(sst1, 4326)# We need to stablsh the projection

ggplot() + geom_stars(data = sst1[1,,,1]) +# for example sst and day 1
  coord_sf(xlim=c(-83,-70),ylim=c(-21,-3))+#xlim=c(-77.4,-76),ylim=c(-14.7,-12.7) area of influence
  scale_y_discrete(name = "Latitude", expand=c(0,0)) +
  scale_x_discrete(name = "Longitude",expand=c(0,0))+
  scale_fill_gradientn(colors = topo.colors(40))+#
  theme_light()
st_crs(sst1)#check the projection

#-----------Averages----#
by_t = "1 month"
sstmonth<-aggregate(sst1, by = by_t , FUN = mean,na.rm=TRUE) 
image(sstmonth[1,1:4,,],axes=TRUE)
times =lubridate::as_date(st_get_dimension_values(sstmonth,'time'))
sstmonth<-st_set_dimensions(sstmonth, "time", values = times, names = "time")
b <- sstmonth%>% as.data.frame %>%
  dplyr::group_by(time) %>%dplyr::summarize(averagsst = mean(analysed_sst-273.15,na.rm = TRUE)) 

write.csv(b,paste0(path_data,'Monthly_sst.csv'))