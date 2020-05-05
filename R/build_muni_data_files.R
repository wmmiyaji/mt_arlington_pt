library(tidyverse)
library(readr)
library(lubridate)

muni.code <- 1426   # Mt Arlington 

# Morris Data https://www.state.nj.us/treasury/taxation/lpt/TaxListSearchPublicWebpage.shtml
# https://www.state.nj.us/treasury/taxation/lpt/MODIV-Counties/2019/Morris19.zip  most years 
# https://www.state.nj.us/treasury/taxation/lpt/MODIV-Counties/2009/Morris.zip  for years 2009, 2011, 2012
# 2019 Layout https://www.state.nj.us/treasury/taxation/lpt/MODIV-Counties/2018/MODIVLayout2019.pdf
# manual process to coerce to layout1.csv
# District Codes https://www.state.nj.us/treasury/taxation/pdf/current/county-muni-codes.pdf

# THIS WAS THE ORIGINAL PROCESS TO CREATE layout1.csv WHICH IS THE FIXED FILE FORMAT FOR TAX Morris18.zip
# library(pdftools)
# library(pdftables)
# text <- pdf_text("MODIVLayout2018.pdf" )
# text2 <- strsplit(text, "\n") %>% str_squish() 
# 
# layout <- data.frame(column_nam = c(text2[[1]][3:49], text2[[2]][3:55],  text2[[3]][3:14])) %>% 
#   mutate(column_nam = str_squish(column_nam), 
#     field_level = word(column_nam, 1), 
#     name = word(column_nam, 2), 
#     picture = word(column_nam,3),
#     field =  word(column_nam, 4), 
#     start = word(column_nam, 5), 
#     end = word(column_nam, 6),
#     length = word(column_nam, 7)) %>% 
#   filter( picture != "GROUP") 
# 
# write.csv(layout, file = "layout.csv")
#  move to field start stop length 66	460	468	9 from line 62 to line 61
# delete 62 

layout1 <- read_csv(file= "./Data_In/layout1.csv") %>% 
  mutate(name = str_replace_all(name, "-", "_"))

code.county.muni.list <- read_csv("./Data_In/Code_County_Muni_List.csv") %>% 
  data.frame() %>% select(1:4) %>% 
  mutate(county.muni = paste(MUNICIPALITY, COUNTY, COUNTY2)) %>% 
  select(-MUNICIPALITY, -COUNTY, -COUNTY2) %>% 
  filter(CODE != "CODE") %>% 
  filter(substr(CODE, 1,2) == "14") %>% 
  mutate(county.muni = str_replace(county.muni, "M orris", "Morris"),
         county.muni = str_replace(county.muni, "Mo rris", "Morris"),
         county.muni = str_replace(county.muni, "Mor ris", "Morris"),
         county.muni = str_replace(county.muni, "Morr is", "Morris"),
         county.muni = str_replace(county.muni, "Morri s", "Morris"), 
         county.muni = str_replace(county.muni, "C ounty", "County"),
         county.muni = str_replace(county.muni, "Co unty", "County"),
         county.muni = str_replace(county.muni, "Cou nty", "County"),
         county.muni = str_replace(county.muni, "Coun ty", "County"),
         county.muni = str_replace(county.muni, "Count y", "County"), 
         county.muni = str_replace(county.muni, " Morris County", ""),
         county.muni = str_replace(county.muni, "TO WN", "TOWN"),
         county.muni = str_replace(county.muni, "LAKE S", "LAKES"),
         county.muni = str_replace(county.muni, "MOUNT ARLINGT ON BORO", "MOUNT ARLINGTON BORO"),
         county.muni = str_replace(county.muni, "TW P", "TWP"),
         county.muni = str_replace(county.muni, "T WP", "TWP"),
         county.muni = str_replace(county.muni, "BOR O", "BORO"),
         county.muni = str_replace(county.muni, "GARDE NS", "GARDENS"),
         county.muni = str_replace(county.muni, "PARSIPPANY TR HLS TWP", "Parsippany-Troy Hills"),
         county.muni = str_to_title(county.muni)
  )
names(code.county.muni.list)[1] <- "COUNTY_DISTRICT"

make_download_url <- function(YEAR) {
  ifelse(YEAR %in% c(2009, 2011, 2012), # these years do not have year in file name 
         URL <- paste0("https://www.state.nj.us/treasury/taxation/lpt/MODIV-Counties/", 
                       YEAR, "/Morris", ".zip"), 
         URL <- paste0("https://www.state.nj.us/treasury/taxation/lpt/MODIV-Counties/", 
                       YEAR, "/Morris", substr(YEAR,3,4), ".zip"))
  return(URL)
}

make_zip_destination <- function(YEAR) {
  dir.create("./Data/Morris")
  paste0("./Data/Morris",  substr(YEAR,3,4), ".zip")
  }

download.zip.file <- function(YEAR) {
  dir.create("./Data/")
  download.file(url = make_download_url(YEAR),
                destfile = make_zip_destination(YEAR))}

untar.zip.file <- function(YEAR){
  ifelse(as.character(Sys.info()[1]) == "Darwin",               # Darwin means Mac 
    untar(make_zip_destination(YEAR), exdir = "./Data/Morris"),
    unzip(make_zip_destination(YEAR), exdir = "./Data/Morris"))
}

delete.zip.file <- function(YEAR){
  fn <- paste0("./Data/Morris",  substr(YEAR,3,4), ".zip")
  if (file.exists(fn)) file.remove(fn)
}

delete.Morris.txt.file <- function(YEAR){
  fn <- paste0("./Data/Morris/Morris",  substr(YEAR,3,4), ".txt")
  if (file.exists(fn)) file.remove(fn)
}

parse.tax.one.year <- function(YEAR) {
  tax.file <- read_fwf(file = paste0("./Data/Morris/Morris",substr(YEAR,3,4),".txt"), 
                       col_positions = fwf_widths(as.numeric(layout1$length))) %>% 
    data.frame()
  names(tax.file) <- layout1$name
  tax.file <- tax.file %>%
    mutate(year = YEAR) %>% 
    mutate(REBATE_BASE_YEAR = as.character(REBATE_BASE_YEAR))
  return(tax.file)
}

combine_all_years_tax <- function(FIRST.YEAR, LAST.YEAR, COUNTY.DISTRICT) {
  
  all.years.tax.data <- FIRST.YEAR:LAST.YEAR %>% 
    map_df(.f = function(YEAR){
      one.year.out <- parse.tax.one.year(YEAR) %>% filter(COUNTY_DISTRICT == COUNTY.DISTRICT)
      #delete.Morris.txt.file(YEAR)
      gc()
      return(one.year.out) 
    }) %>% 
    mutate(COUNTY_DISTRICT = as.character(COUNTY_DISTRICT)) %>% 
    mutate(LAST_YEAR_TAX = ifelse(year == 2009, paste0(substr(LAST_YEAR_TAX,1,8 ),"0"), LAST_YEAR_TAX ))
  
  all.years.tax.data <- code.county.muni.list %>% 
    left_join(all.years.tax.data)
  return(all.years.tax.data)
}

# uses functions to download the Morris County files, unzip and write as .txt to ./Data/Morris
2009:2019 %>% walk(.f = function(YEAR){
  download.zip.file(YEAR)
  untar.zip.file(YEAR)
  delete.zip.file(YEAR)
  if(YEAR == 2009) file.rename(from = "./Data/Morris/Morris.txt", to = "./Data/Morris/Morris09.txt")
  if(YEAR == 2011) file.rename(from = "./Data/Morris/Morris COUNTY.TXT", to = "./Data/Morris/Morris11.txt")
  if(YEAR == 2011) file.rename(from = "./Data/Morris/Morris County.TXT", to = "./Data/Morris/Morris11.txt")
})

# parses each .txt file and build data frame for all years. 
one.town.tax.data <- combine_all_years_tax(2009, 2019, muni.code) %>%            # change town code here
  filter(!is.na(BLOCK))

muni.name <- code.county.muni.list %>% 
  filter(COUNTY_DISTRICT == muni.code) %>% 
  select(county.muni) %>% 
  as.character()

#save(one.town.tax.data,  file = paste0(str_replace_all(muni.name, " ", "_"), "_tax.RData"))

dir.create("./Data_Out/")
write.csv(one.town.tax.data %>% filter(year == 2019), 
          file = paste0("./Data_Out/", str_replace_all(muni.name, " ", "_"), "_tax_record_2019.csv"))
