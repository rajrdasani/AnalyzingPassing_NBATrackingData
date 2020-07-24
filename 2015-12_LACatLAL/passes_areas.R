passes_areas <- function(passes, selected_play, nodes) {

  court_areas <- data.frame(NULL)
  
  for (i in 1:length(passes)) {
    dfall <- selected_play %>% 
      filter(game_clock == passes[i]) %>% 
      filter(lastname != "ball") %>% 
      select(lastname,team_id,x_loc,y_loc)
    colnames(dfall) <- c('Name','ID','X','Y')
    
    clippers_hull <- dfall %>% 
      filter(ID == min(ID)) %>% 
      select(X,Y)
    clippers.c.hull <- chull(clippers_hull)  #Calculates convex hull
    clippers.c.hull <- c(clippers.c.hull, clippers.c.hull[1]) #You need five points to draw four line segments, so we add the first set of points at the end
    clippers_positions <- as.data.frame(cbind(1,
                                              clippers_hull[clippers.c.hull ,]$X,
                                              clippers_hull[clippers.c.hull ,]$Y))
    colnames(clippers_positions) <- c('ID','X','Y')
    
    clippers.chull.coords <- clippers_hull[clippers.c.hull ,]
    clippers.chull.poly <- Polygon(clippers.chull.coords, hole=F)  #From the package sp
    clippers.chull.area <- clippers.chull.poly@area
    clippers_centroid <- c(mean(clippers_hull[clippers.c.hull ,]$X),
                           mean(clippers_hull[clippers.c.hull ,]$Y))
    
    
    lakers_hull <- dfall %>% 
      filter(ID != min(ID)) %>% 
      select(X,Y)
    lakers.c.hull <- chull(lakers_hull)  #Calculates convex hull
    lakers.c.hull <- c(lakers.c.hull, lakers.c.hull[1]) #You need five points to draw four line segments, so we add the first set of points at the end
    lakers_positions <- as.data.frame(cbind(1,
                                            lakers_hull[lakers.c.hull ,]$X,
                                            lakers_hull[lakers.c.hull ,]$Y))
    colnames(lakers_positions) <- c('ID','X','Y')
    
    lakers.chull.coords <- lakers_hull[lakers.c.hull ,]
    lakers.chull.poly <- Polygon(lakers.chull.coords, hole=F)  #From the package sp
    lakers.chull.area <- lakers.chull.poly@area
    lakers_centroid <- c(mean(lakers_hull[lakers.c.hull ,]$X),
                         mean(lakers_hull[lakers.c.hull ,]$Y))
    
    court_areas <- rbind(court_areas, 
                         data.frame(clippers.chull.area, clippers_centroid[1], clippers_centroid[2],
                                    lakers.chull.area, lakers_centroid[1], lakers_centroid[2]))
    
  }
  
  court_areas$pass_number <- c(rep(1:nrow(nodes), each = 2))
  
  return(court_areas)
}


