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
      occ_ll <- records %>% dplyr::select(decimalLatitude,decimalLongitude, scientificName, eventDate, occurrenceID,coordinateUncertaintyInMeters)
      colnames(occ_ll) <- c("lat", "long","species", "date", "ID", "uncertainty")
      occ_ll <- na.omit(occ_ll)
      occ_sp <- st_as_sf(occ_ll, coords= c("long", "lat"), crs= 4326) #dont forget long then lat. otherwise its the wrong way around
}


pts_cost <- function(occ, cost_surface){
      pts <- do.call(rbind, st_geometry(occ$geometry)) %>% 
        as_tibble() %>% setNames(c("x","y"))
      pts_cost <- extract_point_costs(cost_surface, pts)
      occ <- cbind(occ, pts_cost)
      occ$pts_cost<-round(occ$pts_cost,1)
     return(occ)
}

plot_cost <- function(cost_surface, tr, occ_cost, occ_text, cell, fn){
        cost_plot <-  
        ggplot() +
        # cost surface 
        geom_spatraster(data = cost_surface, maxcell=cell) +
        scale_fill_gradientn(colours=grDevices::hcl.colors(50, "YlOrRd", rev = TRUE), na.value = "grey90", name="Cost") +
        # Tracks
        geom_sf(data = tr, aes(geometry=geometry, color = highway_com), key_glyph = draw_key_path, lwd=0.6) +
        scale_color_manual(values=c("#7a7a7a","#4E4E4E","#E69F00","black")) +
        ggnewscale::new_scale_fill() +
        ggnewscale::new_scale_colour() +
        # points
        geom_sf(data = occ_cost, aes(fill = as.factor(status.native_anywhere_in_aus), col=source, geometry = geometry), shape = 21, size = 3, stroke = 1, inherit.aes = FALSE) +
        scale_fill_manual(values=c("#ABC7C9", "#666633", "#669900"), name = "Native Status") +
        scale_color_manual(values=c("#FFD700","black"),name = "Data Source") + # only include this when both herbarim and inat data are avalible. 
        # labels
        geom_text_repel(data = occ_text, aes(label = pts_cost, geometry = geometry), stat = "sf_coordinates", size = 3, segment.size= 0.2,  max.overlaps = Inf) + #fontface = "bold",
        # plot design
        coord_sf() +
        theme_bw() + theme(legend.key = element_rect(fill="grey90")) +
        scale_x_continuous( expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        labs(title = "Cost Surface with Tracks")
      
      ggsave(
        filename = fn,
        width = 40,
        height = 30,
        dpi = 300
      )
}

grid_points_cost <- function (n, np_outline, tr,cost_surface){
        # Grid points of n
        grid <- st_sample(st_as_sf(np_outline), size = n, type = "regular")
        grid_sf <- st_sf(geometry = grid)
        grid_sf <- dist_points_to_path(grid_sf, tr) # Calculates crow distance from point to the path.
        
        pts_grid <- do.call(rbind, st_geometry(grid_sf$geometry)) %>% 
          as_tibble() %>% setNames(c("x","y"))
        pts_grid_cost <- extract_point_costs(cost_surface, pts_grid)
        grid_sf <- cbind(grid_sf, pts_grid_cost)
        grid_sf$pts_cost<-round(grid_sf$pts_grid_cost,1)
    return(grid_sf)
}


cost_v_dist_plot <- function(grid_sf, occ, extra, top_layer_points, fn, cols){
        cost_v_dist <- 
          ggplot() +
          geom_point(data=grid_sf, mapping=aes(x=near_dist, y=pts_cost), colour="grey90") +
          geom_point(data=occ, mapping=aes(x=near_dist, y=pts_cost, fill=status.native_anywhere_in_aus), shape=21, colour="black", size=2) +
          scale_fill_manual(values=cols,name = "Native Status") +
          new_scale_fill() +
          {if(extra)geom_point(data=top_layer_points, mapping=aes(x=near_dist, y=pts_cost), shape=21, fill="#ABC7C9", colour="black", size=2)} +
          theme_bw() + 
          theme(legend.position = c(0.98, 0.02),
                legend.justification = c(1, 0),
                legend.background = element_rect(fill = "white", colour = "grey80"),
                legend.key = element_rect(fill = "white"),
                plot.margin = margin(0, 0, 0, 0,  unit = "pt"),
          ) +
          labs(title = NULL, x= "Distance (km)", y="Cost") +
          scale_x_continuous(expand = c(0.01,0.01)) +
          scale_y_continuous(expand = c(0.01,0.01))

        dens1 <- 
          ggplot() + 
          geom_histogram(data=grid_sf, mapping=aes(x=near_dist), , bins = 100, position = "identity",fill="grey90", colour="grey90") +
          geom_histogram(data=occ, mapping= aes(x = near_dist, fill = status.native_anywhere_in_aus), bins = 100, position = "identity",) + 
          scale_fill_manual(values=cols,name = "Native Status") +
          {if(extra)geom_histogram(data=top_layer_points, mapping= aes(x = near_dist), bins = 100, position = "identity",fill="#ABC7C9")} +    
          theme_bw() +
          theme(axis.line = element_blank(), 
                axis.text.x = element_blank(),
                axis.ticks = element_blank(), 
                axis.title.x = element_blank(),
                legend.position = "none", 
                panel.border = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                plot.margin = margin(0,0,0,0)
          )+
          scale_x_continuous(expand = c(0.01,0.01)) +
          scale_y_continuous(expand = c(0.01,0.01)) +
          labs( y="Count") 

        dens2 <- 
          ggplot() + 
          geom_histogram(data=grid_sf, mapping=aes(x=pts_cost), bins = 100, position = "identity", fill="grey90", colour="grey90") +
          geom_histogram(data=occ, mapping= aes(x = pts_cost, fill = status.native_anywhere_in_aus),bins = 100, position = "identity",) + 
          scale_fill_manual(values=cols,name = "Native Status") +
          {if(extra)geom_histogram(data=top_layer_points, mapping= aes(x = pts_cost), bins = 100, position = "identity", fill="#ABC7C9")} +
          theme_bw() + 
          theme(axis.line = element_blank(), 
                axis.text.y = element_blank(),
                axis.ticks = element_blank(), 
                axis.title.y = element_blank(),
                legend.position = "none", 
                panel.border = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                plot.margin = margin(0,0,0,0)
          )+
          scale_x_continuous(expand = c(0.01,0.01)) +
          scale_y_continuous(expand = c(0.01,0.01)) +
          labs( y="Count")+
          coord_flip()

      dens1 + plot_spacer() + cost_v_dist + dens2 + 
        plot_layout(ncol = 2, nrow = 2, widths = c(8, 2), heights = c(2, 8))# & theme(plot.margin = margin(0,0,0,0))
      
      ggsave(
        filename = fn,
        width = 7,
        height = 7,
        dpi = 300
      )
}
