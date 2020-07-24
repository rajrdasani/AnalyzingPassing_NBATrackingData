source("../RajivFunctions.R")
library(trajr)
library(dplyr)

get_nodes <- function(moments_data, selected_play, event_id) {
  ball_distance <- player_dist_matrix(data = moments_data, eventID = event_id) %>%
    select(starts_with("ball_"))
  
  selected_play_ball <- selected_play %>% 
    filter(event.id == event_id, lastname == "ball") %>% 
    mutate(ball_carrier = 
             apply(ball_distance, 1, which.min),
           ball_carrier_dist = 
             apply(ball_distance, 1, min))
  coords <- selected_play_ball %>% 
    select(x_loc, y_loc, game_clock) %>% 
    TrajFromCoords(fps = 25)
  traj_data <- coords %>% 
    mutate(inst_speed = c(NA, TrajDerivatives(coords)[[1]]),
           inst_acceleration = c(NA, NA, TrajDerivatives(coords)[[3]]),
           dir_change = c(NA, NA, TrajDirectionalChange(coords)))
  
  nodes <- selected_play_ball %>% 
    left_join(traj_data, by = c("x_loc" = "x", "y_loc" = "y", "game_clock" = "game_clock")) %>% 
    unique() %>% 
    filter(abs(dir_change) > 200 | inst_acceleration > 250) %>% 
    mutate(next_node_dist = c(sqrt(diff(x_loc)^2 + diff(y_loc)^2), NA))
  
  nodes <- nodes %>% 
    mutate(next_node_same_carrier = unlist(
      lapply(1:nrow(nodes), function(x) nodes[x,]$ball_carrier == nodes[x+1,]$ball_carrier))) %>% 
    filter(next_node_same_carrier == F,
           radius < 8,
           game_clock < 720)
  
  return(nodes)
} 
  
  

