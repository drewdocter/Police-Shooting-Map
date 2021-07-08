#Annotated Code for Fatal Police Shooting Map


# Reading in Data for Total Population and Prepping for Leaflet ------------------------------------------------------

#Read in My Data
library(tidyverse)
shoot.fatal <- read_csv("https://github.com/washingtonpost/data-police-shootings/releases/download/v0.1/fatal-police-shootings-data.csv")
shoot.fatal

#Compute the total number of shooting deaths per state since 2015.
stat.tot <- shoot.fatal %>% 
  count(state) %>% #Compute count by state
  arrange(-n) #Arrange in descending order
stat.tot

#Need to adjust deaths by population of each state, should merge our subset with the 
#population totals for each state. To do this, first read in a dataset including state populations:
  #Original Source: https://www.infoplease.com/us/states/state-population-by-rank
  #Accessed on May 20th.
  #Copied spreadsheet into my own excel file, dowloaded below.
#Added in Black, Hispanic, and White (non-Hispanic) populations as well: 
  #https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_African-American_population#cite_note-1
  #https://en.wikipedia.org/wiki/List_of_U.S._states_by_Hispanic_and_Latino_population
  #https://en.wikipedia.org/wiki/White_Americans
us_pop <- read_csv("state_pop_2020.csv")
us_pop

#Now merge the ordered state shooting count with the state population datafile.
state.shoot <- stat.tot %>% 
  arrange(state) %>% #Sort by state name to align us_pop and stat.tot datasets
  left_join(us_pop, stat.tot, by = "state") %>% #Use left_join to join tables by "state"
  select(state, n, population) #select all but the "rank" variable from population
state.shoot

#Now that they are joined, calculate population adjusted police shooting death rate.
state.shoot <- state.shoot %>% 
  mutate(rateper100k = ((n/population)*100000)) %>% #Calculate rate of shooting
  #victims per 100000 population in the state.
  mutate(rateper100k.rd = round(rateper100k, digits = 2)) %>%  #Rounded rateper100k
#variable so we have a smoother statistic to visualize.
  mutate(group = "Total") #Added Total label to the dataframe
state.shoot

##Now, prep the data for importing into the map itself.

#This package allows us to read in shapefile of the states,
#as opposed to the html in the tutorial
# install.packages("tigris")

#Read in "states" shapefile.
library(tigris)
states <- states(cb=T)

states$STUSPS #This variable has state abbreviation,
#so we should be able to merge with our state.shoot file

## - Now, join my data to the state shapefile:
#We use the Tigris function geo_join to bring together the states shapefile 
#and the state.shoot dataframe -- STUSPS and state are the two columns they'll 
#be joined by.
states_merged_shoot <- geo_join(states, state.shoot, "STUSPS", "state")
states_merged_shoot #Looks like things succesfully joined

# Getting rid of rows with NA values
# Using the Base R method of filtering subset() because we're dealing with a 
#SpatialPolygonsDataFrame and not a normal data frame, thus filter() wouldn't work
states_merged_shoot <- subset(states_merged_shoot, !is.na(rateper100k.rd))
states_merged_shoot #shapefile with NA rows removed


# Reading in Data for Black Victims and Preparing for Leaflet ---------------------------------

fatal.black <- shoot.fatal %>% 
  filter(race == "B") %>% 
  count(state) %>% 
  arrange(-n)
fatal.black #Notice that there are only 45 states with Black victims, need to manually
  #add in 0's for the others

fatal.black <- read_csv("fatal.black.csv") #Manually added in states missing (6 total)
fatal.black

#Now merge the Black victim shooting count with the state population datafile.
fatal.black <- fatal.black %>% 
  arrange(state) %>% #Sort by state name to align us_pop and fatal.black datasets
  left_join(us_pop, fatal.black, by = "state") %>% #Use left_join to join tables by "state"
  select(state, n, population, black.pop) #select all but the "rank" variable from population
fatal.black

#Now that they are joined, calculate population-adjusted police shooting death rate
#for Black Victims.
fatal.black <- fatal.black %>% 
  mutate(rateper100k = ((n/black.pop)*100000)) %>% #Calculate rate of Black shooting
  #victims per 100000 population in the state.
  mutate(rateper100k.rd = round(rateper100k, digits = 2)) %>% #Rounded rateper100k
#variable so we have a smoother statistic to visualize.
  mutate(group = "Black") #Added "Black" group
fatal.black


## - Now, join the black fatality data to the state shapefile:
#We use the Tigris function geo_join to bring together the states shapefile 
#and the state.shoot dataframe -- STUSPS and state are the two columns they'll 
#be joined by.
states_merged_shoot.blk <- geo_join(states, fatal.black, "STUSPS", "state")
states_merged_shoot.blk #Looks like things succesfully joined

# Getting rid of rows with NA values
# Using the Base R method of filtering subset() because we're dealing with a 
#SpatialPolygonsDataFrame and not a normal data frame, thus filter() wouldn't work
states_merged_shoot.blk <- subset(states_merged_shoot.blk, !is.na(rateper100k.rd))
states_merged_shoot.blk #shapefile with NA rows removed




# Reading in Data for Hispanic Victims and Preparing for Leaflet ----------

fatal.hisp <- shoot.fatal %>% 
  filter(race == "H") %>% 
  count(state) %>% 
  arrange(-n)
fatal.hisp #Notice that there are only 41 states with Hispanic victims, need to manually
#add in 0's for the others
# View(fatal.hisp)

fatal.hisp <- read_csv("fatal.hisp.csv") #Manually added in states missing (10 total)
fatal.hisp

#Now merge the Hispanic victim shooting count with the state population datafile.
fatal.hisp <- fatal.hisp %>% 
  arrange(state) %>% #Sort by state name to align us_pop and fatal.hisp datasets
  left_join(us_pop, fatal.hisp, by = "state") %>% #Use left_join to join tables by "state"
  select(state, n, population, hisp.pop) #select all but the "rank" variable from population
fatal.hisp

#Now that they are joined, calculate population-adjusted police shooting death rate
#for Hispanic Victims.
fatal.hisp <- fatal.hisp %>% 
  mutate(rateper100k = ((n/hisp.pop)*100000)) %>% #Calculate rate of Hispanic shooting
  #victims per 100000 population in the state.
  mutate(rateper100k.rd = round(rateper100k, digits = 2)) %>% #Rounded rateper100k
#variable so we have a smoother statistic to visualize.
  mutate(group = "Hispanic") #Added Hispanic label to each row
fatal.hisp


## - Now, join the Hispanic fatality data to the state shapefile:
#We use the Tigris function geo_join to bring together the states shapefile 
#and the state.shoot dataframe -- STUSPS and state are the two columns they'll 
#be joined by.
states_merged_shoot.hisp <- geo_join(states, fatal.hisp, "STUSPS", "state")
states_merged_shoot.hisp #Looks like things succesfully joined

# Getting rid of rows with NA values
# Using the Base R method of filtering subset() because we're dealing with a 
#SpatialPolygonsDataFrame and not a normal data frame, thus filter() wouldn't work
states_merged_shoot.hisp <- subset(states_merged_shoot.hisp, !is.na(rateper100k.rd))
states_merged_shoot.hisp #shapefile with NA rows removed




# Reading in Data for White Victims and Preparing for Leaflet -------------

fatal.white <- shoot.fatal %>% 
  filter(race == "W") %>% 
  count(state) %>% 
  arrange(-n)
fatal.white #All 51 states have White Victims

#Now merge the White victim shooting count with the state population datafile.
fatal.white <- fatal.white %>% 
  arrange(state) %>% #Sort by state name to align us_pop and fatal.white datasets
  left_join(us_pop, fatal.white, by = "state") %>% #Use left_join to join tables by "state"
  select(state, n, population, white.pop) #select all but the "rank" variable from population
fatal.white

#Now that they are joined, calculate population-adjusted police shooting death rate
#for White Victims.
fatal.white <- fatal.white %>% 
  mutate(rateper100k = ((n/white.pop)*100000)) %>% #Calculate rate of White shooting
  #victims per 100000 population in the state.
  mutate(rateper100k.rd = round(rateper100k, digits = 2)) %>%  #Rounded rateper100k
#variable so we have a smoother statistic to visualize.
  mutate(group = "White") #Added White label to each row
fatal.white


## - Now, join the White fatality data to the state shapefile:
#We use the Tigris function geo_join to bring together the states shapefile 
#and the state.shoot dataframe -- STUSPS and state are the two columns they'll 
#be joined by.
states_merged_shoot.white <- geo_join(states, fatal.white, "STUSPS", "state")
states_merged_shoot.white #Looks like things succesfully joined

# Getting rid of rows with NA values
# Using the Base R method of filtering subset() because we're dealing with a 
#SpatialPolygonsDataFrame and not a normal data frame, thus filter() wouldn't work
states_merged_shoot.white <- subset(states_merged_shoot.white, !is.na(rateper100k.rd))
states_merged_shoot.white #shapefile with NA rows removed

# Create Map Pre-Sets and Theme -----------------------------------

library(leaflet)
library(tigris)
library(colorspace)

#Create custom pop-up for the "Total" polygon
popup_shoot.tot <- paste0("<strong>", states_merged_shoot$NAME, 
                        "</strong><br />Total: ", 
                        states_merged_shoot$n,
                        "<br />Rate: ", 
                        as.character(states_merged_shoot$rateper100k.rd))

#Create custom pop-up for the "Black" group
popup_shoot.black <- paste0("<strong>", states_merged_shoot.blk$NAME, 
                          "</strong><br />Total: ", 
                          states_merged_shoot.blk$n,
                          "<br />Rate: ", 
                          as.character(states_merged_shoot.blk$rateper100k.rd))

#Create custom pop-up for the "Hispanic" group
popup_shoot.hisp <- paste0("<strong>", states_merged_shoot.hisp$NAME, 
                            "</strong><br />Total: ", 
                            states_merged_shoot.hisp$n,
                            "<br />Rate: ", 
                            as.character(states_merged_shoot.hisp$rateper100k.rd))

#Create custom pop-up for the "White" group
popup_shoot.white <- paste0("<strong>", states_merged_shoot.white$NAME, 
                           "</strong><br />Total: ", 
                           states_merged_shoot.white$n,
                           "<br />Rate: ", 
                           as.character(states_merged_shoot.white$rateper100k.rd))


pal.all <- colorNumeric("YlOrRd", domain=c(0,12)) #Set manually, since we 
#want to map all groups on the same scale. Manual domain entry seemed to be the only way
#to do this. This negates the need to have the color palettes for 



# Create Map With Race/Ethnicity Layers -----------------------------------


# Mapping it with the new tiles CartoDB.Positron
library(leaflet)
leaf.map.all <- leaflet() %>% #hideGroup("Outline")
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = states_merged_shoot, #Use Independent dataframe for Total group
              fillColor = ~pal.all(states_merged_shoot$rateper100k.rd), #Apply overall palette to
              #"Total" dataframe
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_shoot.tot, #Custom popup for Total group
              group = "Total",
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>% 
  addPolygons(data = states_merged_shoot.blk, #Independent dataframe for Black group
              fillColor = ~pal.all(states_merged_shoot.blk$rateper100k.rd), #Apply overall palette to
              #"Black" dataframe
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_shoot.black, #Custom popup for Black group
              group = "Black",
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>% 
  addPolygons(data = states_merged_shoot.hisp, #Independent dataframe for Hispanic group
              fillColor = ~pal.all(states_merged_shoot.hisp$rateper100k.rd), #Apply overall palette to
              #"Hispanic" dataframe
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_shoot.hisp, #Custom popup for Hispanic group
              group = "Hispanic",
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>% 
  addPolygons(data = states_merged_shoot.white, #Independent dataframe for White group
              fillColor = ~pal.all(states_merged_shoot.white$rateper100k.rd), #Apply overall palette to
              #"White" dataframe
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_shoot.white, #Custom popup for White group
              group = "White",
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>% 
  addLayersControl(baseGroups = c("Total", "Black",
                                  "Hispanic", "White"), #Allows "slicer" capability for groups
                   #baseGroups only allow one to be selected at a time.
    options = layersControlOptions(collapsed = FALSE,
                                   position = "bottomright")) %>% 
  addLegend(pal = pal.all, #Use manual palette created earlier, for 0-12
            values = c(0,12), #Again, custom values for this legend
            position = "bottomright", 
            title = "Fatal Police<br /> Shooting Rate<br />
            per 100,000 People") %>% 
  addEasyButton(easyButton(icon="fa-globe", title="Zoom to US",
    onClick=JS("function(btn, map){ map.setZoom(3); }"))) #Added "Easy Button" in order to
  #be able to return to full frame of US with one click.
leaf.map.all








