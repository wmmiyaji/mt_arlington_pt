library(tidyverse)
library(sf)

# URLs for the shapefiles 
#http://morrisgisapps.co.morris.nj.us/GISData/PublicData/Shapefiles/Countywide/Parcels.zip
#http://morrisgisapps.co.morris.nj.us/GISData/PublicData/Shapefiles/Countywide/ParcelsMODIV.zip
#http://morrisgisapps.co.morris.nj.us/GISData/PublicData/Shapefiles/Countywide/Municipalities.zip
#http://morrisgisapps.co.morris.nj.us/GISData/PublicData/Shapefiles/Countywide/Counties_Morris.zip

gis.file.list <- c("ParcelsMODIV", "Municipalities", "Counties_Morris", "Parcels")

make_gis_download_url <- function(GIS.TYPE) {
  URL <- paste0("http://morrisgisapps.co.morris.nj.us/GISData/PublicData/Shapefiles/Countywide/", 
                GIS.TYPE, ".zip")
  return(URL)
}

make_gis_zip_destination <- function(GIS.TYPE) {paste0("./Data/Morris",  GIS.TYPE, ".zip")}

download.gis.zip.file <- function(GIS.TYPE) {download.file(url = make_gis_download_url(GIS.TYPE), 
                                                           destfile = make_gis_zip_destination(GIS.TYPE))
  }

untar.zip.file <- function(GIS.TYPE){
  
  ifelse(as.character(Sys.info()[1]) == "Darwin",               # Darwin means Mac 
         untar(make_gis_zip_destination(GIS.TYPE), exdir = paste0("./Data/Morris/", GIS.TYPE)),
         unzip(make_gis_zip_destination(GIS.TYPE), exdir = paste0("./Data/Morris/", GIS.TYPE)))
  
  #untar(make_gis_zip_destination(GIS.TYPE), exdir = paste0("./Data/Morris/", GIS.TYPE))
  
  
}

make.county.gis.sf <- function(GIS.TYPE) {
  file.location <- paste0("./Data/Morris/", GIS.TYPE, "/", GIS.TYPE, "/", GIS.TYPE,".shp")
  sf.file <- st_read(file.location)
  sf.file <- st_transform(sf.file, crs ='+proj=longlat +datum=WGS84')
}


gis.file.list %>% walk(.f = function(GIS.TYPE){
  download.gis.zip.file(GIS.TYPE)
  untar.zip.file(GIS.TYPE)
})

Counties_Morris.sf <- make.county.gis.sf("Counties_Morris")
Municipalities.sf <- make.county.gis.sf("Municipalities")
ParcelsMODIV.sf <- make.county.gis.sf("ParcelsMODIV")
Parcels.sf <- make.county.gis.sf("Parcels")

Municipality.sf <- Municipalities.sf %>% filter(MuniID == muni.code)

Parcels.sf <- Parcels.sf %>% 
  filter(MuniID == muni.code) 


save(Parcels.sf, file = paste0(str_replace_all(muni.name, " ", "_"), "_sf.RData"))
