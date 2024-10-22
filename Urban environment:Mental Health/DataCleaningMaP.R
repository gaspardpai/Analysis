
## Title: Summary of Movement and Place dataset at the SA2-level in Greater Sydney
## Author: Gaspard Pairault
## Date: June 2024


# This code was produced during a university placement to investigate the associations between
# the urban environment and mental health at the community level in Greater Sydney.
# The Movement and Place (MaP) dataset (https://www.movementandplace.nsw.gov.au/) will be used to describe the urban environment.
# The data is stored using polygons and line spatial objects and 29 indicators are summarised at the SA2 level.



library(terra)
library(tidyverse)

##### Selecting study area within Greater Sydney ####
# SA2s with a population density above a chosen treshold were selected for the analysis as the focus is on mental health
# and urban environment so SA2s of low-densities are not of interest.

#All SA2s of NSW
SA2 <- terra::vect('SA2_AUS/SA2_2021_ABS/SA2_2021_AUST_GDA2020.shp')
SA2_NSW <- SA2[SA2$STE_CODE21 == '1', 
               c('SA2_CODE21', 'SA2_NAME21', 'AREASQKM21')]
names(SA2_NSW) <- gsub('AREA', 'SA2_AREA', names(SA2_NSW))


## Population Density

# Using ABS Value
#https://abs.gov.au/statistics/people/population/regional-population/latest-release#data-downloads

PopDensity <- terra::vect('SA2_AUS/PopDensity/SA2 ERP GeoPackage 2022.gpkg')
PopDensity <- terra::project(PopDensity, crs(SA2_NSW))

PopDensity_df <- PopDensity %>%
  data.frame() %>%
  filter(State_code_2021 == 1) %>%
  mutate(SA2_CODE21 = as.character(SA2_code_2021), SA2_NAME21 = SA2_name_2021, PopDensity_mean = Pop_density_2022_people_per_km2) %>%
  select(c(SA2_CODE21, SA2_NAME21, PopDensity_mean))

# Select all SA2s within Greater Sydney
SA2_GSYD <- data.frame(PopDensity) %>%
  filter(GCCSA_code_2021 == "1GSYD") %>%
  mutate(SA2_CODE21 = as.character(SA2_code_2021), SA2_NAME21 = SA2_name_2021, PopDensity_mean = Pop_density_2022_people_per_km2) %>%
  select(c(SA2_CODE21, SA2_NAME21, PopDensity_mean))

dim(SA2_GSYD)

# Determine threshold for population density
GSYD_PopDensity <- SA2_GSYD$PopDensity_mean
summary(GSYD_PopDensity)
quantile(x = GSYD_PopDensity, probs = c(0.05, 0.10, 0.90, 0.95))

# Filter for SA2s above 215 which exclude the bottom 10% within Greater Sydney
Selected_SA2 <- SA2_GSYD %>% filter(PopDensity_mean > 215) %>% select(c(SA2_CODE21, SA2_NAME21))
                       
SA2_selected_vect <- SA2[SA2$SA2_CODE21 %in% Selected_SA2$SA2_CODE21,]

# Polygons objects are stored at different scale in the MaP dataset such as SA1, SAL, MB. 
# Each indicator will be summarised at the SA2 level using similar methods (weighted means)

#### SUBURBS AND LOCALITIES (SAL) scale ####

#Obtaining all SAL of NSW
SAL <- terra::vect('SU/SAL_2021_AUST_GDA94.shp')
SAL_NSW <- SAL[SAL$STE_CODE21 == '1', 
               c('SAL_CODE21', 'SAL_NAME21', 'AREASQKM21')] 
names(SAL_NSW) <- gsub('AREA', 'SAL_AREA', names(SAL_NSW))
SAL_NSW <- terra::project(SAL_NSW, crs(SA2_NSW))
SA2_SAL <- terra::intersect(SAL_NSW, SA2_NSW)
SA2_SAL$AreaInt <- terra::expanse(SA2_SAL, unit = 'km')


## Community Safety 

mean0 <- function(x){
  x <- x[!is.na(x)]
  if(all(x  == 0)){
    0
  } else {
    mean(x, na.rm = TRUE)
  }
}

CommunitySafety <- terra::vect('GSYD/SU_CommunitySafetyMetric3.shp')
CommunitySafety <- terra::project(CommunitySafety, crs(SA2_NSW))
SA2_selected_rast <- terra::rast(SA2_selected_vect, res = 0.0005)
CommunitySafety_r <- terra::rasterize(CommunitySafety, SA2_selected_rast,
                                      field = 'Value')
CommunitySafety_SA2 <- SA2_selected_vect %>% 
  data.frame() %>%
  dplyr::select(SA2_CODE21, SA2_NAME21) %>% 
  dplyr::mutate(CommunitySafety_mean = terra::extract(CommunitySafety_r, 
                                                 SA2_selected_vect,
                                                 fun = mean,
                                                 na.rm = TRUE)$Value)


## Impervious Surface 

ImperviousSurface <- terra::vect('GSYD/SU_ImperviousSurface.shp')
ImperviousSurface <- terra::project(ImperviousSurface, crs(SA2_SAL))


ImperviousSurface_r <- terra::rasterize(ImperviousSurface, SA2_selected_rast,
                                      field = 'Value')
ImperviousSurface_SA2 <- SA2_selected_vect %>% 
  data.frame() %>%
  dplyr::select(SA2_CODE21, SA2_NAME21) %>% 
  dplyr::mutate(ImperviousSurface_mean = terra::extract(ImperviousSurface_r, 
                                                 SA2_selected_vect,
                                                 fun = mean,
                                                 na.rm = TRUE)$Value)



##### SA2 scale ####

# Some metrics were already stores at the SA2 scale but had to be summarise again due to issues
# caused during the data obtention.

## Culture and heritage 
  
CultureHeritage <- terra::vect('GSYD/SA_CultureHeritageMetric1.shp')
CultureHeritage <- terra::project(CultureHeritage, crs(SA2_NSW))

CultureHeritage_r <- terra::rasterize(CultureHeritage, SA2_selected_rast,
                                      field = 'Value')

CultureHeritage_SA2 <- SA2_selected_vect %>% 
  data.frame() %>%
  dplyr::select(SA2_CODE21, SA2_NAME21) %>% 
  dplyr::mutate(CultureHeritage_mean = terra::extract(CultureHeritage_r, 
                                                SA2_selected_vect,
                                                fun = mean,
                                                na.rm = TRUE)$Value)

## Mode Share 

ModeShare <- terra::vect('GSYD/SA2_ModeShare_residence.shp')
ModeShare <- terra::project(ModeShare, crs(SA2_NSW))
ModeShare_r <- terra::rasterize(ModeShare, SA2_selected_rast,
                                       field = 'Value')
ModeShare_SA2 <- SA2_selected_vect %>% 
  data.frame() %>%
  dplyr::select(SA2_CODE21, SA2_NAME21) %>% 
  dplyr::mutate(ModeShare_mean = terra::extract(ModeShare_r, 
                                                 SA2_selected_vect,
                                                 fun = mean,
                                                 na.rm = TRUE)$Value)


#### TRAVEL ZONES (TZ) scale ####

TZ_NSW <- terra::vect('TZ/TZ_NSW_2016.shp')
TZ_NSW <- terra::project(TZ_NSW, crs(SA2_NSW))

SA2_TZ <- terra::intersect(TZ_NSW, SA2_NSW)
SA2_TZ$AreaInt <- terra::expanse(SA2_TZ, unit = 'km')

## Local Jobs 

LocalJobs <- terra::vect('GSYD/TZ_LocalJobs.shp')
LocalJobs <- terra::project(LocalJobs, crs(SA2_TZ))
LocalJobs_TZ <- terra::intersect(LocalJobs, SA2_TZ)

LocalJobs_TZ$AreaInt<- terra::expanse(LocalJobs_TZ, unit = 'km')

LocalJobs_df <- LocalJobs_TZ %>%
  data.frame() %>%
  mutate(Tz16_Area = Tz16_Area*10^-6,
         Area_Pct = AreaInt/ Tz16_Area) %>%
  filter(Area_Pct > 0.5) %>%
  summarise(Value = mean(Value),
            .by = c(TZ16_CODE, TZ16_NAME, SA2_CODE21, SA2_NAME21)) %>%
  summarise(LocalJobs_mean = mean(Value),
            .by = c(SA2_CODE21, SA2_NAME21))

## Population Growth 

PopGrowth <- terra::vect('GSYD/TZ_EconomicDevMetric2.shp')
PopGrowth <- terra::project(PopGrowth, crs(SA2_TZ))
PopGrowth_TZ <- terra::intersect(PopGrowth, SA2_TZ)

PopGrowth_TZ$AreaInt<- terra::expanse(PopGrowth_TZ, unit = 'km')

PopGrowth_df <- PopGrowth_TZ %>%
  data.frame() %>%
  mutate(Tz16_Area = Tz16_Area*10^-6,
         Area_Pct = AreaInt/ Tz16_Area) %>%
  filter(Area_Pct > 0.5) %>%
  summarise(Value = mean(Value),
            .by = c(TZ16_CODE, TZ16_NAME, SA2_CODE21, SA2_NAME21)) %>%
  summarise(PopGrowth_mean = mean(Value),
            .by = c(SA2_CODE21, SA2_NAME21))

## Employment Growth 

EmploymentGrowth <- terra::vect('GSYD/TZ_EconomicDevMetric1.shp')
EmploymentGrowth <- terra::project(EmploymentGrowth, crs(SA2_TZ))
EmploymentGrowth_TZ <- terra::intersect(EmploymentGrowth, SA2_TZ)

EmploymentGrowth_TZ$AreaInt <- terra::expanse(EmploymentGrowth_TZ, unit = 'km')

EmploymentGrowth_df <- EmploymentGrowth_TZ %>%
  data.frame() %>%
  mutate(Tz16_Area = Tz16_Area*10^-6,
         Area_Pct = AreaInt/ Tz16_Area) %>%
  filter(Area_Pct > 0.5) %>%
  summarise(Value = mean(Value),
            .by = c(TZ16_CODE, TZ16_NAME, SA2_CODE21, SA2_NAME21)) %>%
  summarise(EmploymentGrowth_mean = mean(Value),
            .by = c(SA2_CODE21, SA2_NAME21))


## Public space 

PublicSpace <- terra::vect('GSYD/OSM_PublicSpaceMetric2.shp')
PublicSpace <- terra::project(PublicSpace, crs(SA2_TZ))
PublicSpace_TZ <- terra::intersect(PublicSpace, SA2_TZ)
PublicSpace_TZ$AreaInt<- terra::expanse(PublicSpace_TZ, unit = 'km')

PublicSpace_df <- PublicSpace_TZ %>%
  data.frame() %>%
  mutate(Tz16_Area = Tz16_Area*10^-6,
         Area_Pct = AreaInt/ Tz16_Area) %>%
  filter(Area_Pct > 0.5) %>%
  summarise(Value = mean(Value),
            .by = c(TZ16_CODE, TZ16_NAME, SA2_CODE21, SA2_NAME21)) %>%
  summarise(PublicSpace_mean = mean(Value),
            .by = c(SA2_CODE21, SA2_NAME21))


#### MESHBLOCKS (MB) scale ####

MB <- terra::vect('MB/MB_2021_AUST_GDA2020.shp')

## Create a more simple 'lookup' table of NSW Meshblock data, limited to NSW
NSW_MB <- MB[MB$STE_CODE21 == '1', 
             c('MB_CODE21', 'SA2_CODE21', 'SA2_NAME21', 'AREASQKM21')] 

UHI <- terra::vect('GSYD/MB_UrbanHeat.shp')
UHI <- terra::project(UHI, terra::crs(NSW_MB))
UHI_MB <- terra::intersect(UHI, NSW_MB)




## Compute area of each intersected geography
## To see what proportion of the 'whole' meshblock was captured in the intersection
## Different resolutions/shapes/zoom levels will affect this
UHI_MB_area <- terra::expanse(UHI_MB, unit = 'km')

## Convert to data frame, we are done with spatial operations here
## Things run much quicker when we are just dealing with numbers
UHI_MB_df <-  data.frame(UHI_MB)

## - First we want to add on the 'area' we calculated before
## and compute the percentage of the total area each intersection is
## - Then we filter for areas that are at least 50% within their parent meshblock
## - Take the mean value over all of these individual pieces that are above 50%
## to obtain one overall value for each meshblock
## - Then we are fine to aggregate to the SA2 level

UHI_SA2_df <- UHI_MB_df %>%
  dplyr::mutate(AreaIn = UHI_MB_area,
                AreaPct = AreaIn/AREASQKM21) %>% 
  dplyr::filter(AreaPct > 0.5) %>% 
  dplyr::summarise(Value = mean(Value),
                   .by = c(MB_CODE21, SA2_CODE21, SA2_NAME21)) %>% 
  dplyr::summarise(UHI_mean = mean(Value),
                   .by = c(SA2_CODE21, SA2_NAME21))

## Building Height 

BuildingHeight <- terra::vect('GSYD/MB_BuildingHeight.shp')
BuildingHeight <- terra::project(BuildingHeight, terra::crs(NSW_MB))
BuildingHeight_MB <- terra::intersect(BuildingHeight, NSW_MB)
BuildingHeight_MB_area <- terra::expanse(BuildingHeight_MB, unit = 'km')
BuildingHeight_MB_df <-  data.frame(BuildingHeight_MB)

BuildingHeight_SA2_df <- BuildingHeight_MB_df %>%
  dplyr::mutate(AreaIn = BuildingHeight_MB_area,
                AreaPct = AreaIn/AREASQKM21) %>% 
  dplyr::filter(AreaPct > 0.5) %>% 
  dplyr::summarise(Value = mean(Value),
                   .by = c(MB_CODE21, SA2_CODE21, SA2_NAME21)) %>% 
  dplyr::summarise(BuildingHeight_mean = mean(Value),
                   .by = c(SA2_CODE21, SA2_NAME21))

## Tree Canopy 

TreeCanopy <- terra::vect('GSYD/MB_TreeCanopy.shp')
TreeCanopy <- terra::project(TreeCanopy, terra::crs(NSW_MB))

TreeCanopy_MB <- terra::intersect(TreeCanopy, NSW_MB)
TreeCanopy_MB_area <- terra::expanse(TreeCanopy_MB, unit = 'km')

TreeCanopy_MB_df <- data.frame(TreeCanopy_MB)

TreeCanopy_SA2_df <- TreeCanopy_MB_df %>%
  dplyr::mutate(AreaIn = TreeCanopy_MB_area,
                AreaPct = AreaIn/AREASQKM21) %>% 
  dplyr::filter(AreaPct > 0.5) %>% 
  dplyr::summarise(Value = mean(Value),
                   .by = c(MB_CODE21, SA2_CODE21, SA2_NAME21)) %>% 
  dplyr::summarise(TreeCanopy_mean = mean(Value),
                   .by = c(SA2_CODE21, SA2_NAME21))

head(TreeCanopy_SA2_df)

## Public Transport Accessibility Level (PTAL)

PTAL <- terra::vect('GSYD/MB_PTAL.shp')
PTAL <- terra::project(PTAL, terra::crs(NSW_MB))

PTAL_MB <- terra::intersect(PTAL, NSW_MB)
PTAL_MB_area <- terra::expanse(PTAL_MB, unit = 'km')

PTAL_MB_df <- data.frame(PTAL_MB)

PTAL_SA2_df <- PTAL_MB_df %>%
  dplyr::mutate(AreaIn = PTAL_MB_area,
                AreaPct = AreaIn/AREASQKM21) %>% 
  dplyr::filter(AreaPct > 0.5) %>% 
  dplyr::summarise(Value = mean(Value),
                   .by = c(MB_CODE21, SA2_CODE21, SA2_NAME21)) %>% 
  dplyr::summarise(PTAL_mean = mean(Value),
                   .by = c(SA2_CODE21, SA2_NAME21))
dim(unique(PTAL_SA2_df)) == dim(PTAL_SA2_df)


## Building Density 

BuildingDensity <- terra::vect('GSYD/MB_BuildingDensity.shp')
BuildingDensity <- terra::project(BuildingDensity, terra::crs(NSW_MB))

BuildingDensity_MB <- terra::intersect(BuildingDensity, NSW_MB)
BuildingDensity_MB_area <- terra::expanse(BuildingDensity_MB, unit = 'km')

BuildingDensity_MB_df <- data.frame(BuildingDensity_MB)

BuildingDensity_SA2_df <- BuildingDensity_MB_df %>%
  dplyr::mutate(AreaIn = BuildingDensity_MB_area,
                AreaPct = AreaIn/AREASQKM21) %>% 
  dplyr::filter(AreaPct > 0.5) %>% 
  dplyr::summarise(Value = mean(Value),
                   .by = c(MB_CODE21, SA2_CODE21, SA2_NAME21)) %>% 
  dplyr::summarise(BuildingDensity_mean = mean(Value),
                   .by = c(SA2_CODE21, SA2_NAME21))
dim(unique(BuildingDensity_SA2_df)) == dim(BuildingDensity_SA2_df)


##### SUMMARY POLYGON DATAFREAME####


List_Indicators <- list(UHI_SA2_df, BuildingDensity_SA2_df, BuildingHeight_SA2_df, TreeCanopy_SA2_df, PTAL_SA2_df,
                        PublicSpace_df, PopDensity_df, LocalJobs_df, PopGrowth_df, EmploymentGrowth_df,
                        CultureHeritage_SA2, HousingDiversity_SA2, ModeShare_SA2,
                        ImperviousSurface_SA2, CommunitySafety_SA2)


Polygon_SA2_NSW_df <- List_Indicators %>% reduce(full_join, by = c('SA2_CODE21', 'SA2_NAME21'))
head(Polygon_SA2_NSW_df)

#Left join to only keep selected SA2s
GSYD_Polygon_df <- inner_join(
  x = Selected_SA2,
  y = Polygon_SA2_NSW_df,
  by = join_by("SA2_CODE21", "SA2_NAME21")
) 

visdat::vis_miss(GSYD_Polygon_df)





##### Spatial Line data #########

# Loading data 

LocalLiving <- read.csv('Line_summary/LNK_LocalLiving.csv')
PrimarySchool <- read.csv('Line_summary/LNK_PrimarySchoolMetric1.csv')
TransportNode <- read.csv('Line_summary/LNK_TransportNodeMetric3_Table.csv')
LandDivision <- read.csv('Line_summary/LNK_LandDivision.csv')
StreetSpacePedestrian <- read.csv('Line_summary/LNK_StreetSpacePed.csv')
WaterWays <- read.csv('Line_summary/LNK_WaterWaysMetric1.csv')
CrashRate <- read.csv('Line_summary/LNK_FSICrashRate.csv')
CyclingType <- read.csv('Line_summary/LNK_CyclingInfrastructureType_summary.csv')
RoadSafety <- read.csv('Line_summary/LNK_AusRAP.csv')


LocalLiving_df <- LocalLiving %>%
  mutate(LocalLiving_mean = pMEAN_Value) %>%
  select(c(SA2_CODE21, LocalLiving_mean))

LandDivision_df <- LandDivision %>%
  mutate(LandDivision_mean = pMEAN_Value) %>%
  select(c(SA2_CODE21, LandDivision_mean))
  
StreetSpacePedestrian_df <- StreetSpacePedestrian %>%
  mutate(StreetSpacePedestrian_mean = pMEAN_Value) %>%
  select(c(SA2_CODE21, StreetSpacePedestrian_mean))
  

CrashRate_df <- CrashRate %>%
  mutate(CrashRate_mean = pMEAN_Value) %>%
  select(c(SA2_CODE21, CrashRate_mean))

RoadSafety_df <- RoadSafety %>%
  mutate(RoadSafety_mean = pMEAN_Value) %>%
  select(c(SA2_CODE21, RoadSafety_mean))
View(RoadSafety_df)

# 0-800 m distance = within a 10min walk radius


WaterWays_df <- WaterWays %>%
  tidyr::complete(SA2_CODE21, Value, fill = list(sum_length_meters = 0)) %>% 
  summarise(
    Waterways_mean = sum(sum_length_meters[Value == "Walking: 0-800m"]) / sum(sum_length_meters),
    .by = SA2_CODE21) 


PrimarySchool_df <- PrimarySchool %>%
  tidyr::complete(SA2_CODE21, Value, fill = list(sum_length_meters = 0)) %>% 
  # total distance of each stree by SA2
  # mutate(SA2_street_length = sum(sum_length_meters), .by = SA2_CODE21) %>%
  # filter for streets within 800m of school
  # filter(Value == "less than 400m" | Value == "401 to 800m") %>%
  # compute total length of street within 800m and ratio within SA2
  summarise(
    PrimarySchool_mean = sum(sum_length_meters[Value == "less than 400m" | Value == "401 to 800m"]) / sum(sum_length_meters),
            .by = SA2_CODE21) 

# Average number of bus stop per SA2
TransportNode_df <- TransportNode %>%
  #mutate(SA2_street_length = sum(sum_length_meters), .by = SA2_CODE21) %>%
  summarise(BusStopDensity = 1000* sum(Value) / sum(sum_length_meters) , .by = SA2_CODE21) 
  #summarise(n = n(), ndis = n_distinct(SA2_CODE21)) 
  #filter(n() > 1, .by = SA2_CODE21) 

# Proportion of cycling lane in each SA2

#Using land division to obtain length of SA2 streets as it is complete
SA2_length <- LandDivision %>%
  summarise(SA2_l = sum(sum_length_meters), .by = SA2_CODE21)
dim(SA2_length)

CyclingType_df <- CyclingType %>%
  summarise(CyclingLane = sum(sum_length_meters), .by = SA2_CODE21)

CyclingType_df <- right_join(x = CyclingType_df,
                             y = SA2_length
                             ) %>%
  mutate(CyclingLane = ifelse(is.na(CyclingLane), 0, CyclingLane)) %>%
  mutate(CyclingLane_mean = CyclingLane/ SA2_l) %>%
  select(c(SA2_CODE21, CyclingLane_mean))
  
  
visdat::vis_miss(CyclingType_df)
# Using original MaP data for Bus reliability, Street enclosure, Legibility

BusReliability <- terra::vect('GSYD/LNK_BusReliability_delayAMPeak.shp')
BusReliability <- terra::project(BusReliability, crs(SA2_NSW))

BusReliability_NSW <- terra::intersect(BusReliability, SA2_NSW)
BusReliability_NSW$length <- terra::perim(BusReliability_NSW)

BusReliability_df <- BusReliability_NSW %>% 
  data.frame() %>%
  summarise(BusDelay_mean = weighted.mean(Value, length), 
            .by = SA2_CODE21) %>%
  mutate(SA2_CODE21 = as.integer(SA2_CODE21))
  

StreetEnclosure <- terra::vect('GSYD/LNK_StreetEnclosure.shp')
StreetEnclosure <- terra::project(StreetEnclosure, crs(SA2_NSW))
StreetEnclosure_NSW <- terra::intersect(StreetEnclosure, SA2_NSW)
StreetEnclosure_NSW$length <- terra::perim(StreetEnclosure_NSW)

StreetEnclosure_df <- StreetEnclosure_NSW %>% 
  data.frame() %>%
  summarise(StreetAspectRatio_mean = weighted.mean(Value, length), .by = SA2_CODE21) %>%
  mutate(SA2_CODE21 = as.integer(SA2_CODE21))

glimpse(StreetEnclosure_df)


Legibility <- terra::vect('GSYD/LNK_Legibility.shp')
Legibility <- terra::project(Legibility, crs(SA2_NSW))
Legibility_NSW <- terra::intersect(Legibility, SA2_NSW)
Legibility_NSW$length <- terra::perim(Legibility_NSW)


# Should we weight by length as length is already used to obtain legibility score

Legibility_df <- Legibility_NSW %>% 
  data.frame() %>%
  mutate(SA2_CODE21 = as.integer(SA2_CODE21),
         Value = as.numeric(Value)) %>%
  summarise(Legibility_mean = weighted.mean(Value, length), .by = SA2_CODE21) 
 


List_lines <- list(LocalLiving_df, LandDivision_df, StreetSpacePedestrian_df, RoadSafety_df, 
                   PrimarySchool_df, WaterWays_df, CrashRate_df, TransportNode_df, StreetEnclosure_df, 
                   BusReliability_df, Legibility_df, CyclingType_df)

Line_SA2_NSW_df <- List_lines %>% 
  reduce(full_join, by = c('SA2_CODE21')) %>%
  mutate(SA2_CODE21 = as.character(SA2_CODE21))
  

##### FINAL MaP DATA #####

GSYD_Final_MaP <- left_join(
  x = GSYD_Polygon_df,
  y = Line_SA2_NSW_df,
  by = 'SA2_CODE21'
  
)

write.csv(GSYD_Final_MaP , "MovementandPlaceSA2GSYD.csv", row.names=FALSE)

