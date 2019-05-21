#Processing one NetCDF file----
library(ncdf4)
library(reshape2)
library(dplyr)
library(ncdf4.helpers)
# Clear workspace
rm(list=ls())
# set path and filename
setwd("/Users/yasmine/Desktop/ERSST v5/")
ncpath <- "/Users/yasmine/Desktop/ERSST v5/"
ncname <- "ersst.v5.199801"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "ssta"  # note: Extended sst means sst temperature 
# open a netCDF file
ncin <- nc_open(ncfname)
print(ncin)

# get longitude and latitude
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))
# get time
time <- ncvar_get(ncin,"time")
time
tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt
tunits$value
ts <- nc.get.time.series(ncin)

# Get a list of the nc variable names.
attributes(ncin$var)$names

# Take a look at the chlorophyll variable's nc attributes (units etc).
ncatt_get(ncin, attributes(ncin$var)$names[2])

#Retrieve a matrix of the chlorophyll data using the ncvar_get function:
sm <- ncvar_get(ncin, attributes(ncin$var)$names[2])

# Print the data's dimensions
dim(sm)

attributes(ncin$dim)$names
attributes(ncin$dim)$names[4]

# get ersst
ersst_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(ersst_array)
nc_close(ncin)



#Processing multiple NetCDF files----
library(ncdf4)
library(reshape2)
library(dplyr)
library(ncdf4.helpers)

# Clear workspace
rm(list=ls())
# retrieve a list of nc files in my data folder:
flist <- list.files(path="/Users/yasmine/Desktop/BNB Satelite data/ERSST v5/", pattern = "^.*\\.(nc4|nc|NC|Nc|Nc)$")

# Define our function
process_nc <- function(files){
  # iterate through the nc
  for (i in 1:length(files)){
    # open a conneciton to the ith nc file
    nc_tmp <- nc_open(paste0("/Users/yasmine/Desktop/BNB Satelite data/ERSST v5/",files[i]))
    # store values from variables and atributes
    nc_chla <- ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[2])
    nc_lat <- ncvar_get(nc_tmp, attributes(nc_tmp$dim)$names[1])
    nc_lon <- ncvar_get(nc_tmp, attributes(nc_tmp$dim)$names[3])
    time<-ncvar_get(nc_tmp,"time")
    tunits<-ncatt_get(nc_tmp,"time",attname="units")
    tustr<-strsplit(tunits$value, " ")
    nc_start_date<-as.Date(time,origin=unlist(tustr)[3])
    # close the connection sice were finished
    nc_close(nc_tmp)
    # set the dimension names and values of your matrix to the appropriate latitude and longitude values
    dimnames(nc_chla) <- list(lon=nc_lon, lat=nc_lat)
    
    # I'm choosing to store all the data in long format.
    # depending on your workflow you can make different choices here...
    # Your variable may get unmanageably large here
    # if you have high spatial and temporal resolution nc data.
    tmp_chl_df <- melt(nc_chla, value.name = "ssta")
    tmp_chl_df$date<- nc_start_date
    
    # set the name of my new variable and bind the new data to it
    if (exists("chla_data_monthly")){
      chla_data_monthly <- bind_rows(chla_data_monthly, tmp_chl_df)
    }else{
      chla_data_monthly <- tmp_chl_df
    }
    # tidy up, not sure if necesarry really, but neater
    rm(nc_chla, nc_lat, nc_lon, nc_tmp, nc_start_date, tmp_chl_df)
  }
  
  return(chla_data_monthly)
}


data <- process_nc(flist)
## Storing data
save(data, file="ersst.RData")
load(ersst.RData)
write.csv(data, file = "ersst.csv")
## Omitting missing observation
data_nomissing <- na.omit(data)
save(data_nomissing, file="ersst_nomissing.RData")
load(ersst_nomissing.RData)
write.csv(data_nomissing, file = "ersst_nomissing.csv")
summary(data_nomissing)

### Defining Spatiotemporal------------------------------------------------------
tropical<- subset(data_nomissing, data_nomissing$lon>=123.5 & data_nomissing$lon<=290.5 & data_nomissing$lat>=-30.5 & data_nomissing$lat<=60.5) 
library(lubridate)
tropical$month <- month(tropical$date)
tropical$year <- format(tropical$date, "%Y")
summary(tropical)
sd(tropical$ssta)

library(spacetime)
m = stConstruct(tropical, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
str(STFDF)
summary(STFDF[,,"ssta"])
STFDF.years = 1998:2016

#STplot yearly------------------------------------------------------
sel = (1:19) * 1
stplot(STFDF[,sel, "ssta"], 
       STFDF.years[sel], 
       col.regions=rainbow (n = 100, start = 0, end = 1),
       key.space = "right", main="", scales = list(draw = TRUE, 
                                                   alternating=c(1,2), cex=.55, rot=45), 
       cuts=(breaks=c(-6, -5, -4, -3,-2, -1,0, 1,2, 3, 4, 5, 6)))

# Aggregate by month------------------------------------------------------

library(lubridate)
bymonth <- aggregate(ssta~month+lon+lat, data=STFDF,FUN=mean)
bymonth$date=seq(as.Date("2000/1/1"), by = "month", length.out = 12)
summary(bymonth)
# summary statistics by month and year raindall----------------------------------
library(purrr)
bymonth %>% split(.$Month) %>% map(summary)
rain<- subset(TRMMENSO, select = c(Year, ssta, onefourth, ssta))
rain %>% split(.$Year) %>% map(summary)


#STplot monthly------------------------------------------------------

m = stConstruct(bymonth, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
STFDF.months = 1:12
#STplot yearly------------------------------------------------------
sel = (1:12) * 1
stplot(STFDF[,sel, "ssta"], 
       STFDF.months[sel], 
       col.regions=rainbow (n = 100, start = 0, end = .9),
       key.space = "right", main="", scales = list(draw = TRUE, 
                                                   alternating=c(1, 2), cex=.6, rot=45), 
       cuts=(breaks=c(0, 50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800)))

# Aggregate by kirmet------------------------------------------------------

kirmet <- subset(tropical, month > 05 & month < 10)
library(spacetime)
m = stConstruct(kirmet, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
str(STFDF)
summary(STFDF[,,"ssta"])
STFDF.years = 1998:2016
#STplot kirmet yearly------------------------------------------------------
sel = (1:19) * 1
stplot(STFDF[,sel, "ssta"], 
       STFDF.years[sel], 
       col.regions=rainbow (n = 100, start = 0, end = 1),
       key.space = "right", main="", scales = list(draw = TRUE, 
                                                   alternating=c(-3,-2, -1,0, 1,2, 3)))


# Aggregate by Belg------------------------------------------------------


belg=subset(TRMMENSO, Month > 2 & Month < 6)

library(spacetime)
m = stConstruct(belg, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
str(STFDF)
summary(STFDF[,,"ssta"])
STFDF.years = 1998:2016

#STplot Belg yearly------------------------------------------------------

sel = (1:19) * 1
stplot(STFDF[,sel, "ssta"], 
       STFDF.years[sel], 
       col.regions=rainbow (n = 100, start = 0, end = .9),
       key.space = "right", main="", scales = list(draw = TRUE, 
                                                   alternating=c(1, 2), cex=.6, rot=45), cuts=(breaks=c(0, 50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800)))



# Aggregate by Bega------------------------------------------------------

bega=subset(TRMMENSO, Month =c(1, 2, 10, 11, 12))

summary(bega)

library(spacetime)
m = stConstruct(bega, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
str(STFDF)
summary(STFDF[,,"ssta"])
STFDF.years = 1998:2016

#STplot bega yearly------------------------------------------------------

sel = (1:19) * 1
stplot(STFDF[,sel, "ssta"], 
       STFDF.years[sel], 
       col.regions=rainbow (n = 100, start = 0, end = .9),
       key.space = "right", main="", scales = list(draw = TRUE, alternating=c(1, 2), cex=.6, rot=45), cuts=(breaks=c(0, 50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800)))



# Aggregate by spatial  ------------------------------------------------------
library(lubridate)
bysptial <- aggregate(ssta~month+year, data=STFDF,FUN=mean, na.rm=TRUE)
bysptial$date<- seq(from = as.Date("1998-01-01", tz = 'UTC'), to = as.Date("2016-12-01", tz = 'UTC'), by = "month") 

# EOF/PCA of Precipitation ssta -------
library(gstat)
library(sp)
library(spacetime)
m = stConstruct(tropical, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
str(STFDF)
raineof= eof(STFDF[,, "ssta"], "spatial", scale.=TRUE, center = TRUE)
raineof.PCs = eof(STFDF[,, "ssta"], "spatial", scale.=TRUE, center = TRUE, returnEOFs = FALSE)
summary(raineof.PCs)
plot(raineof.PCs)

prc <- prcomp(TRMMwide, center=TRUE, scale=TRUE)
summary(prc)
plot(prc)
varimax7 <- varimax(prc$rotation[,1:2], normalize = FALSE)
newData <- scale(TRMMwide) %*% varimax7$loadings

raineof.PCs = eof(STFDF[,, "ssta"], "spatial", scale.=TRUE, center = TRUE, returnEOFs = FALSE)
varimaxpc2=varimax(raineof.PCs$rotation[, 1:2])
newData= scale(TRMMwide)%*%varimaxpc2$loadings

## Map the leading EOF mode
# Figure 5.17: EOF's
spplot(raineof[1], col.regions = bpy.colors(), scales = list(draw=TRUE),
       main = "")
spplot(raineof[2], col.regions = bpy.colors(), scales = list(draw=TRUE),
       main = "")
spplot(raineof[3], col.regions = bpy.colors(), scales = list(draw=TRUE),
       main = "")

time_index <- seq(from = as.Date("1998-01-01", tz = 'UTC'), to = as.Date("2016-12-01", tz = 'UTC'), by = "month") 

library(xts)
eof.t = xts(predict(raineof.PCs), time_index)
plot(eof.t[,1], main = " ")
plot(eof.t[,2], main = "")


# ... and so on.
names(raineof.PCs)
v = raineof.PCs$sdev^2
w=100*cumsum(v[1:228])/sum(v)

plot(100*cumsum(v[1:100])/sum(v),
     ylim=c(30, 100), xlim=c(0, 100),ylab = "Percent", 
     xlab = "EOF", main = "")
library(Hmisc)
minor.tick(nx=10, ny=10, tick.ratio = 10)

plot(raineof.PCs$sdev, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', type = "lines", main = " ")
lines(raineof.PCs$sdev)
screeplot(raineof.PCs, npcs = min(10, length(raineof.PCs$sdev)), type = "lines")


# REOF/RPCA of Precipitation ssta -------
library(spacetime)
m = stConstruct(tropical, c("lon", "lat"), "date", crs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
STFDF = as(m, "STFDF")
str(STFDF)
raineof= eof(STFDF[,, "ssta"], "spatial", scale.=TRUE, center = TRUE, retx = TRUE, rank. = 3)
raineof.PCs = eof(STFDF[,, "ssta"], "spatial", scale.=TRUE, center = TRUE, retx = TRUE, rank. = 3, returnEOFs = FALSE)
summary(raineof.PCs)
plot(raineof.PCs)

class(raineof.PCs)

## Map the leading EOF mode
# Figure 5.17: EOF's
spplot(raineof[1], col.regions = bpy.colors(), scales = list(draw=TRUE),
       main = "")
spplot(raineof[2], col.regions = bpy.colors(), scales = list(draw=TRUE),
       main = "")
spplot(raineof[3], col.regions = bpy.colors(), scales = list(draw=TRUE),
       main = "")

library(devtools)
install_version("colorspace", "1.2-4")
library(colorspace)
#Mapping EOF using ggplot2 ----------------------------
tropicaleofdf=as.data.frame(raineof)
fix(tropicaleofdf)
library(ggplot2)
EOF1<- ggplot(tropicaleofdf, aes(lon, lat, z = REOF1), colour = ..level..)
EOF1=EOF1 + geom_raster(aes(fill = REOF1)) +
  scale_fill_gradient(low = "yellow", high="blue") +
  geom_contour(colour = "white")+ggtitle("") +
  labs(x = "Longitude",y = "Latitude")
EOF1+guides(fill = guide_legend(" "))
EOF1

EOF2<- ggplot(tropicaleofdf, aes(lon, lat, z = REOF2), colour = ..level..)
EOF2=EOF2 + geom_raster(aes(fill = REOF2)) +
  scale_fill_gradient(low = "yellow", high="blue") +
  geom_contour(colour = "white")+ggtitle("") +
  labs(x = "Longitude",y = "Latitude")
EOF2+guides(fill = guide_legend(" "))
EOF2

EOF3<- ggplot(tropicaleofdf, aes(lon, lat, z = REOF3), colour = ..level..)
EOF3=EOF3 + geom_raster(aes(fill = REOF3)) +
  scale_fill_gradient(low = "yellow", high="blue") +
  geom_contour(colour = "white")+ggtitle("") +
  labs(x = "Longitude",y = "Latitude")
EOF3+guides(fill = guide_legend(" "))
EOF3

library(xts)
eof.t = xts(predict(tropicaleofdf), time_index)
RPC1=plot(eof.t[,1], main = "RPC 1")
RPC2=plot(eof.t[,2], main = "RPC 2")
RPC3=plot(eof.t[,3], main = "RPC 3")

library(ggplot2)
tropicalpcdf=as.data.frame(eof.t)
tropicalpcdf$date <- seq(from = as.Date("1998-01-01", tz = 'UTC'), to = as.Date("2016-12-01", tz = 'UTC'), by = "month") 
save(tropicalpcdf, file = "tropicalpcdf.RData")
fix(tropicalpcdf)

RPC1=ggplot() + geom_linerange(data = tropicalpcdf, 
                               aes(x = date,y = RPC1, ymin = 0, ymax = RPC1, colour = ifelse(RPC1 <0, "RED", "BLUE")))
RPC1=RPC1 + labs(y = "RPC (mm)")+ labs(x = "Date")+ theme(legend.position='none')+ggtitle("(a)")
RPC1+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 

RPC2=ggplot() + geom_linerange(data = tropicalpcdf, 
                               aes(x = date,y = RPC2, ymin = 0, ymax = RPC2, colour = ifelse(RPC2 <0,"RED", "BLUE")))
RPC2=RPC2 + labs(y = "RPC (mm)")+ labs(x = "Date")+ theme(legend.position='none')+ggtitle("(b)")
RPC2+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 

RPC3=ggplot() + geom_linerange(data = tropicalpcdf, 
                               aes(x = date,y = RPC3, ymin = 0, ymax = RPC3, colour = ifelse(RPC3 <0,"RED", "BLUE")))
RPC3=RPC3 + labs(y = "RPC (mm)")+ labs(x = "Date")+ theme(legend.position='none')+ggtitle("(c)")
RPC3+ scale_x_date(date_breaks = "1 year", date_labels =  "%Y") 


# ... and so on.
names(raineof.PCs)
v = raineof.PCs$sdev^2
plot(100*cumsum(v[1:228])/sum(v),
     ylim=c(30, 100), ylab = "Percent", xlab = "EOF", main = "")


plot(raineof.PCs$sdev, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', type = "lines", main = " ")
lines(raineof.PCs$sdev)
screeplot(raineof.PCs, npcs = min(10, length(raineof.PCs$sdev)), type = "lines")


# https://www.rdocumentation.org/packages/spacetime/versions/1.1-5/topics/EOF
## https://stats.stackexchange.com/questions/72421/showing-spatial-and-temporal-correlation-on-maps
# https://www.rdocumentation.org/packages/stats/versions/3.4.3/topics/prcomp

# Put REOF/RPC ggplot in one page -------
library(gridExtra)
grid.arrange(EOF1, EOF2,EOF3, RPC1, RPC2,RPC3, ncol=3)

# Merge 2 TS datasets -----------------------------------------------------------
rainglobal$date<- seq(from = as.POSIXct("1998-01-01", tz = 'UTC'), to = as.POSIXct("2016-12-01", tz = 'UTC'), by = "month") 
global.temp=merge(global_sst_2016, temp, by = c("Month", "Year"))
pc.global.temp=merge(global.temp, rainpcdf, by = "date")
save(pc.global.temp,file = "pc.global.temp.RData")


# Select variables from PC + Global SST Rainfall-------
pc.global.temp.subset <- subset(pc.global.temp, select = c(nino1.2ssta, nino3.4ssta, amm, pdo, mei, tsa, soi, temp, PC1, PC2))

#Correlation matrix with significance levels (p-value) -----------
res=cor(pc.global.temp.subset,method="pearson")
round(res, 2)

library("Hmisc")
res2 <- rcorr(as.matrix(pc.global.temp.subset))
res2

#Visualize correlation matrix-----------------
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "pch")
# Positive correlations are displayed in blue and negative correlations in red color. Color intensity and the size of the circle are proportional to the correlation coefficients. In the right side of the correlogram, the legend color shows the correlation coefficients and the corresponding colors.

#Use chart.Correlation()--------------
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(pc.global.temp.subset, histogram=TRUE, pch=19)
#The distribution of each variable is shown on the diagonal.
#On the bottom of the diagonal : the bivariate scatter plots with a fitted line are displayed
#On the top of the diagonal : the value of the correlation plus the significance level as stars
#Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols("***", "**", "*", ".", " ")

#beautiful LATEX, HTML and ASCII tables from R statistical output -------------
install.packages("stargazer")
library(stargazer)

# Select variables from Global SST Rainfall-------
global.temp.subset <- subset(global.temp, select = 
                               -c(date, Year, Month, NINO1.2, NINO3, nino3ssta, NINO4, nino4ssta, NINO3.4))


#regression ---------------
# Load libraries
library(stats)
library(forecast)
library(Matrix)
library(astsa)
library(xlsx)
library(lmtest)
library(imputeTS)

# Model TP at Station 1


## Time Series plot of PC1
PC1ts <- ts(pc.global.temp$PC1, frequency=12, start=c(1998,1))

d <- density(PC1ts) # returns the density data
plot(d, main="") 

hist(PC1ts)

plot(PC1ts, xaxt='n', las = 2,col = "blue", cex.axis = 0.78, cex.main=0.9,  xlab="Years", ylab="P(mm)")
axis(side = 1, at = pc.global.temp$date)

qqnorm(PC1ts, main="", col='red', cex.axis = 0.78)               
qqline(PC1ts) 

## Time Series plot of PC2

PC2ts <- ts(pc.global.temp$PC2, frequency=12, start=c(1998,1))
summary(PC2ts)

d <- density(PC2ts) # returns the density data
plot(d, main="") 

hist(PC2ts)

plot(PC2ts, xaxt='n', las = 2,col = "blue", cex.axis = 0.78, cex.main=0.9,  xlab="Years", ylab="P(mm)")

qqnorm(PC2ts, main="", col='red', cex.axis = 0.78)               
qqline(PC2ts) 


# Create data frame

# put the dependent variable y in a matrix form
y1=data.matrix(pc.global.temp$PC1) 


# put the regressors in a data frame
x1=data.frame(global.temp.subset,check.names = TRUE)

# run the model ARIMA (1,1,0) with independent variable: log(y) and 
# external regressors s1, s2, s3, s4 (indicator variables for the seasons) and f1 (flow)
ts1=arima(y1, order = c(1,1,0)) 

ts1

# test significance of coefficients
coeftest(ts1)
#checking residuals
r1=rstandard(ts1)

plot(r1, main="ts1 std residuals")

par(mfrow=c(2,2)) #put the next two graphs side by side
hist(r1))
qqnorm(r1)
qqline(r1)
par(mfrow=c(1,1))


