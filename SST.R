#library(stars)
library(ggplot2)
library(sf)
library(tidyr)
library(ncdf4)
path_data<-('./Environmental/SST/')

trtfile <- nc_open("home/Seabirds/METOFFICE-GLO-SST-L4-REP-OBS-SST_1633110411621.nc")
sst1<-read.ncdf(trtfile)


# daily from 2000-2007

ncpath <- "~/Seabirds/"
ncname <- "METOFFICE-GLO-SST-L4-REP-OBS-SST_1633110411621"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "analysed_sst"  # note: tmp means temperature (not temporary)

ncin <- nc_open(ncfname)

# get longitude and latitude
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)


# get time
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt

#convert time from secondsd since xxx into dates
time<-as.POSIXct(time, origin="1981-01-01")
tail(time)


# get temperature


# starting from year 4 (2004)
tmp_array<- ncvar_get(ncin,dname)[1097:1200] # cannot allociate the vector because it is too big

dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)


# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)


# reshape the array into vector
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)


# reshape the vector into a matrix
tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)  # have to adjust nt when data is subsetted
dim(tmp_mat)

# head(na.omit(tmp_mat))


# create a dataframe
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df02 <- data.frame(cbind(lonlat,tmp_mat)) # time goes across columns, lon and lat go along rows

## PROBLEM: 
# want to get the averages
## here I have no solution yet: 
### should I average along all geographic locations for each time point so that there is only one temperature per time and then
#    take the average per month from those averages? 

# then I would (after averaging across region per day) have a df containing colum 1 = date, colum 2= average SST, and then create column 3 = mean SST per month
# orientating myself on the following lines of code

#  Get months
df1$Month <- months(df1$X1)

#  Get years
df1$Year <- format(df1$X1,format="%y")

#  Aggregate 'X2' on months and year and get mean
aggregate( X2 ~ Month + Year , df1 , mean )
#    Month Year        X2
#1 December   09 0.0000000
#2 February   10 0.1714286
#3  January   10 1.2074074

