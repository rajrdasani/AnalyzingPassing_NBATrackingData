
library(tidyverse)
library(trajr)
library(dplyr)
source("../RajivFunctions.R")

load("C:/Users/rajda/OneDrive/Desktop/CMU/NBATrackingData/2015-12_LACatLAL/full_game.rda")

get_nodes_new <- function(frames){
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
           game_clock < 720) %>% 
    mutate(next_node_dist = c(sqrt(diff(x_loc)^2 + diff(y_loc)^2), NA)) %>% 
    filter(next_node_dist > 3)# | is.na(next_node_dist))
  
  return(nodes)
}

all_ball_frames <- full_game[full_game$lastname == "ball", ]


final_nodes_shots <- function() {
  lastframe_q1 <- all_ball_frames[all_ball_frames$quarter == 2, ]$frame_num[1] - 1
  lastframe_q2 <- all_ball_frames[all_ball_frames$quarter == 3, ]$frame_num[1] - 1
  lastframe_q3 <- all_ball_frames[all_ball_frames$quarter == 4, ]$frame_num[1] - 1
  lastframe_q4 = nrow(all_ball_frames)
  
  
  nodes_q1 <- get_nodes_new(1:lastframe_q1)
  nodes_q2 <- get_nodes_new(lastframe_q1+1:lastframe_q2)
  nodes_q3 <- get_nodes_new(lastframe_q2+1:lastframe_q3)
  nodes_q4 <- get_nodes_new(lastframe_q3+1:lastframe_q4)
  
  all_nodes <- do.call("rbind", list(nodes_q1, nodes_q2, nodes_q3, nodes_q4))
  
  return(all_nodes)
}

