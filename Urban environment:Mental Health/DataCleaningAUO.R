library(tidyverse)
library(terra)


# SUBSET OF SA1s from ABS

# We want to only select the SA1s that are within the 'Selected SA2s of GSYD'. 
# In order to do so, we perform a  join between the SA1 and SA2 dataframe based on SA2Code & Name

SA1 <- terra::vect('SA1_2021/SA1_2021_AUST_GDA2020.shp')
SA1_df <- data.frame(SA1) %>%
  select(c('SA1_CODE21', 'SA2_CODE21', 'SA2_NAME21'))

data.frame(SA1)
Selected_SA1 <- right_join(
  x = SA1_df,
  y = Selected_SA2,
  by = c('SA2_CODE21', 'SA2_NAME21')
) 
dim(Selected_SA1)

# Checking if all Selected SA2 are included
Selected_SA1 %>% group_by(SA2_NAME21) %>% summarise(n = n()) %>% dim()
dim(Selected_SA2)

# SUBSET OF AUO DATA BASED ON SELECTED SA1s
# FOR SYDNEY 
SYD_AUO_SA1 <- read.csv('/Users/gaspard/Desktop/PHRAME/Analysis/AUO/sydney_2021_sa1.csv') 
glimpse(SYD_AUO_SA1)
SYD_AUO_SA1$sa1 <- as.character(SYD_AUO_SA1$sa1 )


SYD_Selected_AUO_SA1 <- inner_join(
  x = SYD_AUO_SA1,
  y = Selected_SA1,
  by = join_by(sa1 == SA1_CODE21)
) %>%
  mutate(SA1_CODE21 = sa1)


dim(SYD_Selected_AUO_SA1)[1]
dim(Selected_SA1)[1]

# MISSINGNESS OF URBAN LIVEABILITY INDEX for SA1 included in the AUO data

visdat::vis_miss(SYD_Selected_AUO_SA1 %>% select(urban_liveability_index))

# Number of SA1 with missing ULI for each SA2
missing_liveability <-SYD_Selected_AUO_SA1 %>% 
  filter(is.na(urban_liveability_index)) %>% 
  group_by(SA2_NAME21) %>%
  summarise(missing_sa1 = n())

# Number of SA1 in each SA2 based on AUO data

auo_sa2 <- SYD_Selected_AUO_SA1 %>% 
  group_by(SA2_NAME21) %>%
  summarise(sa1_auo_n = n())

# Join to obtain percentage of ULI missingness within each SA2 based on SA1 from AUO
left_join(x = missing_liveability,
          y = auo_sa2) %>%
  mutate(perc_uli_missing = missing_sa1/sa1_auo_n) %>%
  arrange(desc(perc_uli_missing))

# Computing ULI mean for each SA2 and dropping SA1 with missing value
# We are summarising at the SA2 scale and expect nearby SA1 to have similar ULI
# Consequently, dropping the few rows with NA isn't a big issue 


AUO_SYD_SA2_df <- SYD_Selected_AUO_SA1 %>%
  filter(!is.na(urban_liveability_index)) %>% 
  group_by(SA2_CODE21, SA2_NAME21) %>%
  summarise(ULI_mean = mean(urban_liveability_index))

dim(AUO_SYD_SA2_df)


AUO_SYD_SA2_df %>%
  ggplot() +
  aes(x = ULI_mean) +
  geom_histogram(bins = 30, fill = 'darkgreen') +
  theme_minimal()
  

write.csv(AUO_SYD_SA2_df, 'AUO_SYD_SA2.csv', row.names = FALSE)


# MISSINGNESS of SA1s in the AUO data based on selected SA1s from ABS

List_AUO_sa1 <- SYD_AUO_SA1 %>% pull(sa1)
List_found_SA1_syd <- SYD_Selected_AUO_SA1 %>% pull(SA1_CODE21)

length(List_found_SA1_syd)/length(List_AUO_sa1) #97% of SA1s are present

#SA1s present in AUO but are not selected due to either low population density at the SA2 scale or error due to different code
length(List_AUO_sa1) - length(List_found_SA1_syd)

#SA1 selected as part of GSYD and pop density above treshold but absent from AUO data:
Missing_SA1_AUO <- Selected_SA1 %>% filter(SA1_CODE21 %in% List_AUO_sa1 == FALSE)
Missing_SA1_GSYD <- Selected_SA1 %>% filter(SA1_CODE21 %in% List_found_SA1_syd == FALSE)
dim(Missing_SA1_AUO)
dim(Missing_SA1_GSYD) # Same dimensions to be expected due to inner join

#Let's investigate the SA1s that have been selected but are not present in AUO Syd when using SA1code

over5_sa1_missing <- Missing_SA1_AUO %>% 
  group_by(SA2_NAME21) %>% 
  summarise(omitted_SA1_n = n()) %>%
  arrange(desc(omitted_SA1_n)) %>%
  filter(omitted_SA1_n >= 5)


over5_sa1_missing

# Dataframe with SA2 and number of SA1 per SA2 on ULO data
sa1_per_sa2 <- SA1_df %>%
  group_by(SA2_NAME21) %>%
  summarise(sa1_n = n())

inner_join(Missing_SA1_AUO, sa1_per_sa2) %>%
  group_by(SA2_NAME21, sa1_n) %>%
  summarise(missing_SA1_n = n()) %>%
  mutate(missing_perc = missing_SA1_n/ sa1_n) %>%
  filter(missing_perc > 0.1) %>%
  arrange(desc(missing_perc))

# Let's check if any sa1 have been excluded but should have been included

excluded_AUO_syd <- SYD_AUO_SA1 %>% 
  filter(sa1 %in% List_found_SA1_syd == FALSE)

excluded_AUO_syd %>% 
  group_by(lga) %>%
  summarise(omitted_SA1_n = n())

#focus on excluded sa1 in the Northern Beaches
excluded_AUO_syd %>% 
  filter(lga == "Northern Beaches") %>%
  group_by(suburb) %>%
  summarise(omitted_SA1_n = n())
#visualisation with qgis confirmed that those suburbs should indeed by excluded


#Create a new column that split the SA2 name into suburb
list_sa2_over5 <- over5_sa1_missing$SA2_NAME21
df_subs <- str_split_fixed(list_sa2_over5, '-', 3) %>%
  data.frame()
names(df_subs) <- c("sub_1", "sub_2", "sub_3")

over5_sa1_missing <- over5_sa1_missing %>%
  mutate(sub1 = df_subs$sub_1,
         sub2 = df_subs$sub_2,
         sub3 = df_subs$sub_3)
SYD_AUO_SA1 %>%
  filter(suburb %in% over5_sa1_missing$sub1 |
         suburb %in% over5_sa1_missing$sub2 |
         suburb %in% over5_sa1_missing$sub3  )%>%
  select(c(sa1, suburb))


# Checking population of missing SA1s

SA1_population <- read.csv('SA1_2021/SA1_URPs_NSW.csv') %>% mutate(Population = as.numeric(Population))

#Selected SA1 that are absent from the AUO dataset

#Two extremums with high population
SA1_population %>%
  filter(SA1_CODE %in% Missing_SA1_GSYD$SA1_CODE21, Population >1000)

#Visualisation without two extemums

SA1_population %>%
  filter(SA1_CODE %in% Missing_SA1_GSYD$SA1_CODE21, Population > 200)  %>%
  ggplot() +
  aes(x = Population) +
  geom_histogram(fill = 'darkblue')
  
#SA1 generally have between 200 and 800 people, Percentage of missing SA1 that have less than 50 people
SA1_population %>%
  filter(SA1_CODE %in% Missing_SA1_GSYD$SA1_CODE21, Population < 200) %>%
  count()/dim(Missing_SA1_GSYD)

#Selected SA1 that have are present in AUO but have no liveability index

sa1_ULI_missing <- SYD_Selected_AUO_SA1 %>% 
  filter(is.na(urban_liveability_index)) %>%
  pull(SA1_CODE21)

# SA1 with missing ULI have a Popualtion within the expected 200-800 range
SA1_population %>%
  filter(SA1_CODE %in% sa1_ULI_missing ) %>%
  ggplot() +
  aes(x = Population) +
  geom_histogram(bins =25,fill = 'darkblue')

#FOR NEWCASTLE
NC_AUO_SA1 <- read.csv('/Users/gaspard/Desktop/PHRAME/Analysis/AUO/newcastle_maitland_2021_sa1.csv') 
glimpse(NC_AUO_SA1)
NC_AUO_SA1$sa1 <- as.character(NC_AUO_SA1$sa1 )

NC_Selected_AUO_SA1 <- inner_join(
  x = NC_AUO_SA1,
  y = Selected_SA1,
  by = join_by(sa1 == SA1_CODE21)
) %>%
  mutate(SA1_code21 = sa1)

dim(NC_Selected_AUO_SA1)[1]




