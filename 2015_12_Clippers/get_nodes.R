source("../RajivFunctions.R")
library(tidyverse)
library(trajr)
library(hms)

get_nodes_full <- function(full_game, team1, team2) {
  
  full_game = read.csv(full_game)
  
  full_game$game_clock = hms::as_hms(full_game$game_clock)
  
#  ball_handler_vec <- (full_game %>%
#                         group_by(frame_num) %>%
#                         slice(ball_handler) %>%
#                         unique())$lastname
  all_ball_frames <- full_game %>%
    filter(lastname == "ball") #%>%
#    mutate(ball_handler = ball_handler_vec)
  
  player_info <- full_game %>% 
    select(player_id, lastname, firstname, jersey, position, team_id, team) %>% 
    unique()
  
  team1_vec <- player_info %>% filter(team == team1) %>% pull(lastname)
  team2_vec <- player_info %>% filter(team == team2) %>% pull(lastname)
  
  # Part 1 - Retrieving the starting nodes
  
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
    start_nodes <- selected_play %>% 
      left_join(traj_data, by = c("x_loc" = "x", "y_loc" = "y", "game_clock" = "game_clock")) %>% 
      unique() %>% 
      filter(abs(dir_change) > 200 | inst_acceleration > 250,
             !(shot_clock %in% prob_sc_stoppages))
    
    start_nodes <- start_nodes %>% 
      mutate(next_node_same_handler = unlist(
        lapply(1:nrow(start_nodes), function(x) start_nodes[x,]$ball_handler == start_nodes[x+1,]$ball_handler))) %>% 
      filter(next_node_same_handler == F,
             radius < 9,
             game_clock < 720) %>% 
      mutate(next_node_dist = c(sqrt(diff(x_loc)^2 + diff(y_loc)^2), NA)) %>% 
      filter(next_node_dist > 3 | is.na(next_node_dist))
    
    return(start_nodes)
  }
  
  partitions <- c(seq(from = 1, to = nrow(all_ball_frames), by = 5000), nrow(all_ball_frames))
  nodes_dfs <- list()
  for(i in 2:length(partitions)){
    if (i < length(partitions)) {
      nodes_dfs[[i]] <- get_nodes(partitions[i-1]:partitions[i])
    }
  }
  
  start_nodes <- bind_rows(nodes_dfs) %>% 
    filter(dir_change < 2000)
  
  
  
  # Part 2 - Retrieving the end nodes
  
  ## Extract new features for all_ball_frames
  
  all_ball_frames$displacement <- 0
  for (i in 1:nrow(all_ball_frames)) {
    all_ball_frames$displacement[i] = 
      sqrt(((all_ball_frames$x_loc[i+1] - all_ball_frames$x_loc[i])^2) +
             ((all_ball_frames$y_loc[i+1] - all_ball_frames$y_loc[i])^2))
  }
  all_ball_frames$sig_mov <- all_ball_frames$displacement > 0.5
  
  traj_data <- all_ball_frames %>% 
    select(x_loc, y_loc, radius, game_clock) %>% 
    TrajFromCoords(fps = 25) %>% 
    mutate(inst_speed = c(TrajDerivatives(.)[[1]], NA),
           inst_acceleration = c(NA, TrajDerivatives(.)[[3]], NA),
           dir_change = c(NA, TrajDirectionalChange(.), NA))
  
  all_ball_frames <- bind_cols(all_ball_frames, select(traj_data, inst_speed, inst_acceleration, dir_change))
  
  ## Create dataframe of end nodes
  end_nodes <- NULL
  for (i in 1:nrow(start_nodes)) {
    postnode_df <- all_ball_frames[(all_ball_frames$game_clock < start_nodes$game_clock[i] - 0.08) & all_ball_frames$quarter == start_nodes$quarter[i], ]
    endnode <- postnode_df[(((postnode_df$inst_acceleration < -200)|(postnode_df$sig_mov == F)) & postnode_df$radius > 1),][1,]
    end_nodes <- rbind(end_nodes, endnode)
  }
  
  
  # Part 3 - Use end nodes to further filter the starting nodes
  
  start_nodes$receiver <- end_nodes$ball_handler
  start_nodes$ball_handler_team <- start_nodes$ball_handler %>% 
    fct_collapse(team1 = team1_vec,
                 team2 = team2_vec,
                 other_level = "ball")
  
  start_nodes$receiver_team <- start_nodes$receiver %>% 
    fct_collapse(team1 = team1_vec,
                 team2 = team2_vec,
                 other_level = "ball")
  
  shots_left_basket <- which(end_nodes$x_loc < 6 & end_nodes$x_loc > 4 & end_nodes$y_loc < 26 & end_nodes$y_loc > 24)
  shots_right_basket <- which(end_nodes$x_loc < 92 & end_nodes$x_loc > 88 & end_nodes$y_loc < 27 & end_nodes$y_loc > 23)
  shots_all_baskets <- c(shots_left_basket, shots_right_basket)
  
  passes <- start_nodes[-shots_all_baskets,] %>% 
    filter(ball_handler_team == receiver_team)
  
  return(passes)
  
}
