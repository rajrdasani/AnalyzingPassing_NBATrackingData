
library(tidyverse)
source("../RajivFunctions.R")

load(file = "full_game.rda")

get_nodes <- function(frames){
  selected_play <- full_game %>%
    filter(frame_num %in% frames)
  selected_play <- selected_play %>%
    group_by(frame_num) %>%
    mutate(ball_handler = ifelse(length(minN(dist_to_ball, 2)) > 1,
                                 lag(ball_handler),
                                 which(dist_to_ball == minN(dist_to_ball, 2)))) %>%
    ungroup()
  
  ball_handler_vec <- (selected_play %>%
                         group_by(frame_num) %>%
                         slice(ball_handler) %>%
                         unique())$lastname
  selected_play <- selected_play %>%
    filter(lastname == "ball") %>%
    mutate(ball_handler = ball_handler_vec)
  
  
  coords <- selected_play %>% 
    select(x_loc, y_loc, game_clock) %>% 
    TrajFromCoords(fps = 25)
  
  traj_data <- coords %>% 
    mutate(inst_speed = c(TrajDerivatives(coords)[[1]], NA),
           inst_acceleration = c(NA, TrajDerivatives(coords)[[3]], NA),
           dir_change = c(NA, TrajDirectionalChange(coords), NA))
  
  # Data Cleaning: Remove frames where the shot clock is stopped
  prob_sc_stoppages <- selected_play %>% 
    group_by(shot_clock) %>% 
    summarise(count = n()) %>%
    filter(count > 11) %>%
    pull(shot_clock)
  
  # Filtering is divided into two stages. The variables "next_node_same_handler"
  # and "next_node_dist" must be generated between the stages.
  nodes <- selected_play %>% 
    left_join(traj_data, by = c("x_loc" = "x", "y_loc" = "y", "game_clock" = "game_clock")) %>% 
    unique() %>% 
    filter(abs(dir_change) > 200 | inst_acceleration > 250,
           !(shot_clock %in% prob_sc_stoppages))
  
  nodes <- nodes %>% 
    mutate(next_node_same_handler = unlist(
      lapply(1:nrow(nodes), function(x) nodes[x,]$ball_handler == nodes[x+1,]$ball_handler))) %>% 
    filter(next_node_same_handler == F,
           radius < 9,
           game_clock < 720) %>% 
    mutate(next_node_dist = c(sqrt(diff(x_loc)^2 + diff(y_loc)^2), NA)) %>% 
    filter(next_node_dist > 3 | is.na(next_node_dist))
  
  return(nodes)
}

all_ball_frames <- filter(data = full_game, lastname = "ball")


partitions <- c(seq(from = 1, to = nrow(all_ball_frames), by = 5000), nrow(all_ball_frames))
nodes_dfs <- list()
for(i in 1:length(partitions)){
  nodes_dfs[[i]] <- get_nodes(partitions[i]:partitions[i + 1])
}



nodes <- bind_rows(nodes_dfs) %>% 
  filter(dir_change < 2000)