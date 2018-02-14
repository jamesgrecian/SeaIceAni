####################################################################
### Some code to manipulate the NSIDC Sea Ice concentration data ###
####################################################################

#Load libraries
require(animation)
require(ggplot2)
require(httr)
require(RCurl)
require(raster)
require(rworldmap) #Load in rworldmap to access countriesLow shapefile
require(sf)

#Define projection - R reads in NSIDC data with wrong projection...
prj = "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"

#Data needed for animation...
#"2017-04-12 15:10:30" - "2017-09-07 06:55:47" 

#Use RCurl library to query FTP server and list files
url = "ftp://anonymous:wjg5@sidads.colorado.edu/DATASETS/NOAA/G02135/north/monthly/geotiff/01_Jan/"
fn <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE, verbose = F)
fn = paste(url, strsplit(fn, "\r*\n")[[1]], sep = "") 

#Only access files for sea ice concentration
fn = fn[grep("concentration", fn)]
res <- GET(fn[38], write_disk(basename(fn[38]))) #most recent file

ic = stack(res$request$output$path) #more flexible alternative to raster
ic2 = readGDAL(res$request$output$path) 

projection(ic) = "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
#0 is ocean; 2510 pole hole; 2530 coast line; 2540 land; 2550 missing
#0-1000 so divide by 10 to get percentage
ic[ic>1000] = NA
ic = ic/10
plot(ic)
plot(ic)

###
### Alternative download first...
###
fn = list.files("~/NSIDC concentraion")
x = stack()
for (i in 1:length(fn)){
  ice = brick(paste0("~/NSIDC concentraion/", fn[i]))
  x = stack(x, ice)
}
projection(x) = "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"

#0 is ocean; 2510 pole hole; 2530 coast line; 2540 land; 2550 missing
#0-1000 so divide by 10 to get percentage
x[x>1000] = NA
x = x/10
plot(x)

#Load in world shape from rworldmap and force to same CRS
CP <- as(extent(-180, 180, 30, 90), "SpatialPolygons")
proj4string(CP) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
proj4string(countriesLow) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
world_shp <- rgeos::gIntersection(countriesLow, CP, byid=T, drop_lower_td = T)

#pal <- wes_palette("Zissou", 100, type = "continuous")
#install.packages("viridis")
#library(viridis)
#viridis_pal(option = "D")(26) 

saveVideo({
  for (i in 1:nlayers(x)){
    
    ice_df = as.data.frame(rasterToPoints(subset(x, i)))
    names(ice_df) = c("x", "y", "z")
    
    p1 = ggplot() +
      geom_raster(data = ice_df, aes(x = x, y = y, fill = z)) +
      scale_fill_distiller("% Sea Ice", palette = "Blues", limits = c(0,100)) +
      geom_sf(data = st_transform(st_as_sf(world_shp), prj), colour = "dark grey", fill = "dark grey") +
      coord_sf(xlim = c(-3850000, 3750000), ylim = c(-5250000, 5700000), crs = prj, expand = F) +
      xlab("") +ylab("") +
      ggtitle(substr(fn[i], 3, 8)) + theme(plot.title = element_text(hjust = 0, face = "italic")) +
      theme(plot.margin=unit(c(1, 1, 0.5, 0.5), "cm")) #pad window space
    
    print(p1)
  }
}, movie.name = "seaice.mp4", interval = 0.5, nmax = 50, ani.width = 750, 
ani.height = 750, other.opts = "-pix_fmt yuv420p -b:v 1080k")




quartz(title = "Panel Plot", width = 7, height = 7)
print(p1)
quartz.save(file = "~/Desktop/Arctic land and ice.jpeg", type = "jpeg",
            dev  = dev.cur(), dpi = 500)
dev.off()

#  scale_colour_viridis(discrete=TRUE, option = "viridis") +
theme(legend.position="none") +
  
  ggplot() +
  scale_fill_distiller("fsle", palette = "RdYlBu") +
  geom_polygon(data = world_df, mapping = aes(x = long, y = lat, group = id), fill = "black") +
  coord_fixed(ratio = 1, xlim = c(-1500000, 2000000), ylim = c(2900000, 6000000)) +
  ylab("Northings") +xlab("Eastings") +
  scale_x_continuous(labels = comma, expand = c(0, 0)) + 
  scale_y_continuous(labels = comma, expand = c(0, 0)) +
  theme(axis.title.y = element_text(angle=-90)) +
  theme(axis.text.y = element_text(angle=-90, hjust = 0.5)) + 
  theme(text = element_text(family = "serif")) +
  theme(plot.margin=unit(c(1, 0.5, 0.5, 0.5), "cm")) #pad window space

