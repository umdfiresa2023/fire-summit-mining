# Team Mining
Luke, Ashmita, Jack, Alex

## Research Question

Does cobalt mining influence conflict more than other types of mining in
the Democratic Republic of the Congo?

<img src="Congo_Mining.jpeg" width="493" />

The Shabara artisanal mine, near the Congolese boomtown of Kolwezi.
Impoverished miners dig out cobalt and copper by hand.

## Data Wrangling

**Outcome variable**

Our outcome variable is the average number of conflicts that occur
within a 10km radius of each mine between 1997 and 2023. The conflict
data is sourced from the Armed Conflict Location & Event Data Project
(ACLED) which contains all of the documented conflicts, both large and
small, in the entire world between 1997 and 2023. The conflict data can
be downloaded from this webpage: https://acleddata.com/data-export-tool/

<img src="conflict_locations.png" width="428" height="433" />

The map above displays all of the conflicts that occurred in the
Democratic Republic of the Congo between 1997 and 2023. Each conflict is
represented by a singular red circle on the map.

**Wrangling Methodology**

The following R code contains the instructions for extracting the
spatial data for each conflict from the conflict data provided by the
ACLED.

``` r
library("tidyverse")
library("terra")
library("simplermarkdown")
library("readxl")

#Reads in the data from the spreadsheet produced by the ACLED and filters the data to obtain solely conflicts from the Democratic Republic of the Congo.
Africa_1997_2023_Sep22<-read_excel("Africa_1997-2023_Sep22.xlsx")
acl<-Africa_1997_2023_Sep22 %>%
  filter(COUNTRY=="Democratic Republic of Congo")

#Turns each conflict in the dataset into a spacial vector based on its Longitude and Latitude values.
acl_points<-vect(acl, 
                 geom=c("LONGITUDE", "LATITUDE"), 
                 crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
plot(acl_points, col = "red", xlab = "Latitude", ylab = "Longitude")
```

**Treatment variable**

Our treatment variable is whether a mine is active or inactive (open or
closed) when a conflict within a 10km radius of that mine occurs. We
sourced our data on mining permits directly from the Democratic Republic
of the Congo’s Ministry of Mining who had made the data publicly
available on the Global Forest Watch website here:
https://data.globalforestwatch.org/datasets/democratic-republic-of-the-congo-mining-permits/explore

![](mining_locations.png)

The chart above displays all of the mining permits granted in the
Democratic Republic of the Congo between 1997 and September of 2023
according to the country’s official data from the Ministry of Mines.
Each mining permit is represented by a singular blue square on the map.

**Wrangling Methodology**

The following R code contains the instructions for reading in the
spatial data for each mining permit as provided by the Democratic
Republic of the Congo’s Ministry of Mines

``` R
```

``` r
library("tidyverse")
library("terra")
library("simplermarkdown")

#Getting data from file provided by the Democratic Republic of the Congo Ministry of Mines
data_name <- "Democratic_Republic_of_the_Congo_mining_permits"

#Changing name of "objectid" column to "mine_number"
colnames(df_mines)[colnames(df_mines) == "objectid"] <- "mine_number"

#Getting the shapefile which contains the spatial data on where the mining permits are located
data_shp_path <- paste0(data_name, "/", data_name, ".shp")
mines <- vect(data_shp_path)

#Ensuring correct coordinate system
mines_data <- project(mines, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
```

**Intersection Process**

Now that we have read in the data for both the conflicts and the mining
permits, we need to determine how many active and inactive conflicts
occurred within a 10km radius of each mine location. We do this with the
following R code:

``` r
library("tidyverse")
library("terra")
library("simplermarkdown")

#uses the buffer function to create a 10km buffer (radius) around each mine location
mining_center<-centroids(mines_data)
mining_area<-buffer(mining_center, width=10000, capstyle="round")

#initializes a list to store the dataframe of all the conflicts that occured within each mine location's 10km buffer.
intersect_list <- list()

#Looping through all of the mine locations
for(i in 1:nrow(mining_area)) {
  #Gets mine location
  mining_location <- mining_area[i,]
  #Uses the terra package to detirmine all of the conflicts (in acl_points) that occured within the 10km buffer (aka "intersected") with each mine location
  intersections <- terra::relate(mining_location, acl_points, relation = "intersects")
  #Turning the result into a dataframe
  intersections_vec <- as.vector(intersections)
  intersecting_conflicts <- acl[intersections_vec,]
  #Matching each conflict to its corresponding mine location by mine number
  intersecting_conflicts$mine_number <- i
  #adding the dataframe which contains all of the conflicts that intersected with that mining location to the list
  intersect_list[[i]] <- intersecting_conflicts
}
#Turning the final result back into a dataframe
intersect_df <- do.call(rbind, intersect_list_data)

#Merging the final result with the mines dataframe based on mine number in order to determine if conflicts are active or inactive.
merged_df <- merge(intersect_df, df_mines, by = "mine_number", all.x = TRUE, all.y = TRUE) %>%
  mutate(date=as.Date(EVENT_DATE, "%Y-%m-%d")) %>%
  mutate(active = ifelse(date > date_do & date < date_de1, 1, 0)) %>%
  mutate(inactive = ifelse(active == 0, 1, 0))

#Creating one last dataframe that contains the following columns for each mine: number of active conflicts, number of inactive conflicts, year the mine opened, year the mine closed, mine status (open or closed), and the type of minerals mined
final_df <- merged_df %>%
  group_by(mine_number) %>%
  summarize(
    num_conflicts_active = sum(active),
    num_conflicts_inactive = sum(inactive),
    year_start = first(date_do),
    year_end = first(date_de1),
    status = first(statut),
    type = first(resource)) %>%
    mutate(num_conflicts_active = ifelse(is.na(num_conflicts_active), 0, num_conflicts_active)) %>%
    mutate(num_conflicts_inactive = ifelse(is.na(num_conflicts_inactive), 0, num_conflicts_inactive)) %>%
    mutate(is_cobalt_mine = ifelse(grepl(substring_co, type), 1, 0)) %>%
    filter(!is.na(year_start)) %>%
    filter(!is.na(type))
```

![](intersection.png)

The map above demonstrates the overlap between mining locations and
conflict in the Democratic Republic of the Congo.

**Control variables**

Our control variable is the type of mine, specifically cobalt mines vs
other types of mines. The data on mining permits provided by the DRC’s
Ministry of Mining contains information on the types of minerals that
are mined at each mining location. A mine is considered a cobalt mine if
cobalt is one of the minerals that are mined there.

**Wrangling Methodology**

``` r
library("tidyverse")
library("terra")
library("simplermarkdown")

#Given a list of all the possible types of minerals that are present in the mines data, this loop computes the mean number of active and inactive conflicts for mines of each type of mineral.
for(mineral in unique_resources) {
  f2<-final_df %>%
  filter(str_detect(final_df_op$type, mineral)) %>%
  summarize(
    mineral = mineral,
    mean_active=mean(num_conflicts_active), 
    mean_inactive=mean(num_conflicts_inactive))
  final_mineral_df <- rbind(final_mineral_df, f2)
}

#filtering the data by mineral type (cobalt or non-cobalt)
cobalt_df <- final_mineral_df %>%
  filter(mineral == "Co")

non_cobalt_df <- final_mineral_df %>%
  filter(mineral != "Co")

#Computes the mean number of active and inactive conflicts for all non-cobalt mines
total_non_cobalt_df <- data.frame(mineral = "Non_Cobalt", 
                       mean_active = mean(non_cobalt_df$mean_active),
                       mean_inactive = mean(non_cobalt_df$mean_inactive))

#Combines the two dataframes into one for the following graph
cobalt_comp_df <- rbind(cobalt_df, total_non_cobalt_df)
```

## Preliminary Results

<img src="final_bargraph.png" width="530" />

The graph above displays the differences in the mean number of active
and inactive conflicts between cobalt mines and non-cobalt mines. Cobalt
mines had slightly more active conflicts (2.43 active conflicts for
cobalt mines to 1.64 active conflicts for non-cobalt mines on average)
while non-cobalt mines were more likely to have conflicts before they
opened (1.64 conflicts before mine open for non-cobalt mines to 0.33
conflicts before mine open for cobalt mines) and also after they opened
(9.04 conflicts after mine open for non-cobalt mines to 1.63 conflicts
after mine open for cobalt mines). Overall, it appears that cobalt mines
did not cause a disproportionate amount of conflict when compared to
other types of mines.
