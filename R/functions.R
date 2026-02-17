#Functions
#requires two sf objects
dist_points_to_path <- function(points, paths){
  nearest_index <- st_nearest_feature(points, paths)
  nearest_distances <- st_distance(points, paths[nearest_index, ], by_element = TRUE)
  near_dist <- as.data.frame(nearest_distances)
  near_dist <- near_dist %>%
    mutate_at(vars(nearest_distances), ~ as.numeric(gsub(" \\[m\\]", "", .)))
  near_dist$near_dist <- near_dist$nearest_distances/1000 #unit change m to km
  near_dist <- cbind(near_dist, points)
 # next three lines are new in case it causes error with the old code 
  nearest_lines <- st_nearest_points(points, paths[nearest_index, ], pairwise = TRUE)
  nearest_points_geom <- st_cast(nearest_lines, "POINT")[seq(2, length(nearest_lines)*2, by = 2)]
  near_dist$nearest_point_geom <- nearest_points_geom    
  
  return(near_dist)
}


#turning occurrences records into shape file for compatibility
galah_record_to_sf <-function(records){
occ_ll <- records %>% select(decimalLatitude,decimalLongitude, scientificName, eventDate)
colnames(occ_ll) <- c("lat", "long","species", "date")
occ_ll <- na.omit(occ_ll)
occ_sp <- st_as_sf(occ_ll, coords= c("long", "lat"), crs= 4326) #dont forget long then lat. otherwise its the wrong way around
}

