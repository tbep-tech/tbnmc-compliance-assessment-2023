library(tbeptools)
library(here)
library(dplyr)

# wq data -----------------------------------------------------------------

##
# epc data

# local file path
# xlsx <- here::here('data/data-raw', 'wq_data.xls')
xlsx <- here::here('data/data-raw', 'Results_Provisional.xlsx')

# import and download if new
# wqdat <- read_importwq(xlsx, download_latest = T)
epcdata <- read_importwq(xlsx, download_latest = F) %>% 
  select(
    bay_segment, 
    station = epchc_station, 
    SampleTime, 
    yr, 
    mo, 
    Latitude, 
    Longitude, 
    chla,
    chla_q
  ) %>% 
  mutate(
    station = as.character(station)
  )

##
# BCBS, TCB, and MR chlorophyll data 

# data through 2021, in reasonable assurance repo
# https://github.com/tbep-tech/reasonable-assurance-analysis/blob/main/R/dat_proc.R, line 27
olddatraw <- rdataload('https://github.com/tbep-tech/reasonable-assurance-analysis/raw/main/data/chldat.RData')

olddat <- olddatraw %>% 
  filter(bay_segment %in% c('BCBS', 'TCB', 'MR'))

# shapefile for bcbs spatial subset for pinellas data
bcbsseg <- st_read(here('data-raw/tampabay_ra_seg_watersheds.shp')) %>% 
  st_transform(crs = 4326) %>% 
  filter(BAY_SEGMEN == 5)

mancodataraw <- read_importwqp(org = '21FLMANA_WQX', type = 'wq', trace = T)
pincodataraw <- read_importwqp(org = '21FLPDEM_WQX', type = 'wq', trace = T)
# bcbs

# these are from S. Day, via email 9/6/22 for probabilistic
bcbschlraw3 <- read_excel(here('data-raw/Pinellas Co Boca Ciega data .xlsx'), sheet = '2021')

bcbschl3 <- bcbschlraw3 %>% 
  select(
    station = Site, 
    SampleTime = Date, 
    Latitude = Latitude, 
    Longitude = Longitude, 
    chla = `Chl-a`, 
    chla_q = `Chla_q`, 
    Level
  ) %>% 
  mutate(
    bay_segment = 'BCBS',
    SampleTime = mdy(SampleTime), 
    yr = year(SampleTime), 
    mo = month(SampleTime), 
    Latitude = as.numeric(Latitude), 
    Longitude = -1 * as.numeric(Longitude)
  ) %>% 
  select(bay_segment, station, SampleTime, yr, mo, everything()) %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326, remove = F) %>% 
  .[bcbsseg, ] %>% 
  st_set_geometry(NULL)  

bcbschl <- bcbschl3

# TCB - from Manatee County Water Atlas and original NNC report data
# data source/provider query is STORET_21FLMANA (legacy) and WIN_21FLMANATEE (new) 

# from water atlas
manchlraw <- read.csv(here('data-raw/manchl.txt'), sep = '\t', header = T)

tcbchl1 <- manchlraw %>%
  filter(WaterBodyName == 'Terra Ceia Bay') %>% 
  filter(Parameter %in% c('Chla_ugl', 'ChlaC_ugl')) %>% 
  select(
    station = StationID, 
    SampleTime = SampleDate, 
    Latitude = Actual_Latitude, 
    Longitude = Actual_Longitude, 
    chla = Result_Value, 
    chla_q = QACode
  ) %>% 
  mutate(
    bay_segment = 'TCB',
    station = gsub('^=', '', station), 
    SampleTime = mdy_hms(SampleTime), 
    yr = year(SampleTime), 
    mo = month(SampleTime)
  ) %>% 
  filter(yr > 2010) %>% 
  select(bay_segment, station, SampleTime, yr, mo, everything()) 

tcbchl <- tcbchl1

# MR - from Manatee County Water Atlas add MR and TCB data
# data source/provider query is STORET_21FLMANA (legacy) and WIN_21FLMANATEE (new)

# from water atlas (through June 2023 only)
manchlraw <- read.csv(here('data/data-raw/manchl.txt'), sep = '\t', header = T)

tcbchl1 <- manchlraw %>%
  filter(WaterBodyName == 'Terra Ceia Bay') %>% 
  filter(Parameter %in% c('Chla_ugl', 'ChlaC_ugl')) %>% 
  select(
    station = StationID, 
    SampleTime = SampleDate, 
    Latitude = Actual_Latitude, 
    Longitude = Actual_Longitude, 
    chla = Result_Value, 
    chla_q = QACode
  ) %>% 
  mutate(
    bay_segment = 'TCB',
    station = gsub('^=', '', station), 
    SampleTime = mdy_hms(SampleTime), 
    yr = year(SampleTime), 
    mo = month(SampleTime)
  ) %>% 
  filter(yr > 2010) %>% 
  select(bay_segment, station, SampleTime, yr, mo, everything()) 

# water atlas  
mrchl1 <- manchlraw %>% 
  filter(WaterBodyName == 'Manatee River Estuary') %>% 
  filter(Parameter %in% c('Chla_ugl', 'ChlaC_ugl')) %>% 
  select(
    station = StationID, 
    SampleTime = SampleDate, 
    Latitude = Actual_Latitude, 
    Longitude = Actual_Longitude, 
    chla = Result_Value, 
    chla_q = QACode
  ) %>% 
  mutate(
    bay_segment = 'MR',
    station = gsub('^=', '', station), 
    SampleTime = mdy_hms(SampleTime), 
    yr = year(SampleTime), 
    mo = month(SampleTime)
  ) %>% 
  select(bay_segment, station, SampleTime, yr, mo, everything()) %>% 
  filter(yr > 2021) 

mrchl <- mrchl1

# combine all
chldat <- epcdata %>% 
  bind_rows(olddat) %>% 
  bind_rows(bcbschl) %>% 
  bind_rows(tcbchl) %>% 
  bind_rows(mrchl)

save(chldat, file = here('data/chldat.RData'))
