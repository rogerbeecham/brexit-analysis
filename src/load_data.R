referendum_data<- read_csv("http://www.electoralcommission.org.uk/__data/assets/file/0014/212135/EU-referendum-result-data.csv")
# Keep only relevant variables
referendum_data <- referendum_data %>% transmute( Region_Code,	
                                                  Region,	Area_Code,
                                                  Area,	
                                                  Electorate,	
                                                  Turnout = Pct_Turnout/100,	
                                                  Valid_Votes,
                                                  Leave = Pct_Leave/100)
# Census 2011 Key Statistics are usefully provided at Chris Gale's GitHub detailing the 2011 Ouput Area Classification: https://github.com/geogale/2011OAC  
census_data<- read_csv("http://staff.city.ac.uk/~sbbm143/datasets/2011_census_oa.csv")
oa_la_lookup <- read.csv("http://staff.city.ac.uk/~sbbm143/datasets/oa_la_lookup.csv")
oa_la_lookup$OA <- as.character(oa_la_lookup$OA)
census_data <- left_join(census_data, oa_la_lookup)
# Iterate over OA level data and compute summary statistics on relevant variables to LA level.
census_data <- census_data %>%
  group_by(LOCAL_AUTHORITY_CODE) %>%
  summarise(
    total_pop = sum(Total_Population),
    younger_adults = sum(Age_20_to_24, Age_25_to_29, Age_30_to_44) / sum(Total_Population), 
    white = sum(White_British_and_Irish) / sum(Total_Population),
    christian = sum(Christian) / sum(Total_Population),
    english_speaking = sum(Main_language_is_English_or_Main_language_not_English__Can_speak_English_very_well)
    / sum(Total_Population),
    single_ethnicity_household = sum(All_household_members_have_the_same_ethnic_group) 
    / sum(Total_Households),
    own_home = sum(Owned_and_Shared_Ownership) / sum(Total_Households),
    not_good_health = sum(Fair_health, Bad_health, Very_bad_health) / sum(Total_Population),
    degree_educated = sum(Highest_level_of_qualification_Level_4_qualifications_and_above) / 
      sum(Highest_level_of_qualification_Level_4_qualifications_and_above,
          Highest_level_of_qualification_Level_3_qualifications,
          Highest_level_of_qualification_Level_1_Level_2_or_Apprenticeship,
          No_qualifications),
    no_car = sum(No_cars_or_vans_in_household) / sum(Total_Households),
    private_transport_to_work = sum(Private_Transport) / sum(Total_Employment_16_to_74),
    professionals = sum(Managers_directors_and_senior_officials, Professional_occupations) /
      sum(Total_Employment_16_to_74)
  )

birth_country_11 <- read_csv("http://staff.city.ac.uk/~sbbm143/datasets/country_of_birth_2011.csv")
birth_country_11 <- left_join(birth_country_11, oa_la_lookup, by=c("oa_code"="OA"))
birth_country_11 <- birth_country_11 %>%
                    group_by(LOCAL_AUTHORITY_CODE) %>%
                    summarise(total_pop = sum(POPULATION),
                              eu_born = sum(eu_born)) %>%
                    transmute(geo_code = LOCAL_AUTHORITY_CODE,
                              total_pop = total_pop,
                              eu_born = eu_born/total_pop)
attribute_data <- inner_join(census_data, birth_country_11, by=c("LOCAL_AUTHORITY_CODE"="geo_code"))
attribute_data$total_pop.y<- NULL
colnames(attribute_data)[1:2]<- c("geo_code","total_pop")
attribute_data$geo_code <- as.character(attribute_data$geo_code)

# Recode 2011 Census data to match thise of referendum and boundary data
attribute_data$geo_code[attribute_data$geo_code=="E07000097"] <- "E07000242"
attribute_data$geo_code[attribute_data$geo_code=="E07000101"] <- "E07000243"
attribute_data$geo_code[attribute_data$geo_code=="E07000104"] <- "E07000241"
attribute_data$geo_code[attribute_data$geo_code=="E07000100"] <- "E07000240"
attribute_data$geo_code[attribute_data$geo_code=="E08000020"] <- "E08000037"
attribute_data$geo_code[attribute_data$geo_code=="E06000048"] <- "E06000057"

# Join 2011 Census and referendum data.
attribute_data <- inner_join(attribute_data, referendum_data, by=c("geo_code"="Area_Code"))

# Shapefile containing GB LA boundaries --  made available from ONS Open Geography Portal. 
# We simplify the geometries using the "rmapshaper" library.
download.file("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_3.zip", "boundaries_gb.zip")
unzip("boundaries_gb.zip")
gb_boundaries <- readOGR(dsn = ".", layer = "Local_Authority_Districts_December_2015_Super_Generalised_Clipped_Boundaries_in_Great_Britain")
# Convert from EGS 84 to OSGB-- in this case OSGB: https://epsg.io/27700.
gb_boundaries <- spTransform(gb_boundaries, CRS("+init=epsg:27700"))
# Simplify polygon. This may take a little time to execute.
gb_boundaries <- ms_simplify(gb_boundaries, keep=0.2)

# Note that "gb_boundaries" is an R SpatialDataFrame. A DataFrame containing LA names, codes and
# summary statistics can be accessed through "gb_boundaries@data" 
gb_boundaries@data$geo_code <- as.character(gb_boundaries@data$lad15cd)
# Merge attribute data with SpatialDataFrame containing LA geomoetries.
gb_boundaries@data <- inner_join(gb_boundaries@data, attribute_data)
# Let's rename this SpatialDataFrame now containing the boundary information and the attribute data. 
data_gb <- gb_boundaries
# In order keep a clean workspace, remove the redundant data.
rm(census_data)
rm(referendum_data)
rm(attribute_data)
rm(gb_boundaries)
rm(birth_country_11)
rm(oa_la_lookup)
# We'll calculate another variable that might be discriminating: population density. As well as a margin (leave/remain) variable for charting.
data_gb@data$area <- gArea(data_gb, byid=TRUE)/1000
colnames(data_gb@data)[3] <- "geo_label"
data_gb@data$Area <- NULL
data_gb@data$lad16cd <- NULL
data_gb@data$lad16nmw <- NULL
data_gb@data$st_areasha <- NULL
data_gb@data$st_lengths <- NULL
data_gb@data <- data_gb@data %>%
  mutate(leave_remain = Leave-0.5,
         population_density = total_pop/area) 
# For spatially arranged charts by region, we generate row and column indices for each region using Meulemans et al.'s (2017) SMWG approach: https://www.gicentre.net/smwg.
region_layout <- read_tsv("http://staff.city.ac.uk/~sbbm143/datasets/region_layout.tsv")
region_layout$region[region_layout$region=="East of England"] = "East"
data_gb@data <- inner_join(data_gb@data, region_layout, by=c("Region" = "region"))
data_gb$lat <- NULL
data_gb$long <- NULL
# Hex cartogram for LAs from ESRI: http://www.arcgis.com/home/item.html?id=593037bc399e460bb7c6c631ceff67b4
download.file("http://staff.city.ac.uk/~sbbm143/datasets/gb_hexagonal", "gb_hexagonal.zip")
unzip("gb_hexagonal.zip")
gb_hex <- readOGR(dsn = "gb_hexagonal", layer = "GB_Hex_Cartogram_LAs")
gb_hex@data$geo_code <- as.character(gb_hex@data$LAD12CD)
gb_hex@data$geo_label <- as.character(gb_hex@data$LAD12NM)
gb_hex@data <- inner_join(gb_hex@data, data_gb@data[,6:ncol(data_gb@data)])