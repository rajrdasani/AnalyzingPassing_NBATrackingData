library(tidyverse)
library(janitor)
library(lubridate)
library(hms)
library(jsonlite)

input <- commandArgs(TRUE)[1]
output <- commandArgs(TRUE)[2]

### --- Load functions --- ####

dist_to_ball <- function(data, index){
  players <- data %>%
    filter(frame_num == index) %>% 
    pull(full_name)
  
  dist_matrix <- data %>%
    filter(frame_num == index) %>%
    select(x_loc, y_loc) %>%
    dist(diag = T, upper = T) %>% 
    as.matrix() %>% 
    as.data.frame()
  
  colnames(dist_matrix) <- players
  
  return(dist_matrix$ball)
}

minN <- function(x, N=2){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=N)[N]
}

sportvu_convert_json <- function (file.name) {
  the.data.file<-fromJSON(file.name)
  ##Get the sports vu data
  moments <- the.data.file$events$moments
  
  ##Function for extracting infomration from JSON
  extractbb <- function (listbb)
  {#df <- unlist(listbb,recursive = FALSE)
    df <- listbb
    # str(df)
    quarters <- unlist(lapply(df, function(x) x[[1]]))
    game.clock <- unlist(lapply(df, function(x) x[[3]]))
    shot.clock <- unlist(lapply(df, function(x) ifelse(is.null(x[[4]]), 'NA', x[[4]])))
    moment.details <- (lapply(df, function(x) x[[6]]))
    x3 <-  mapply(cbind, moment.details, game.clock, shot.clock,quarters, SIMPLIFY=F)
    x4 <- do.call('rbind', x3)
    return (x4)
  }
  
  test2 <- lapply(moments, function (x) {extractbb(x)})
  lengthmm <- the.data.file$events$eventId
  test2 <- mapply(cbind, test2, "event.id"=lengthmm, SIMPLIFY=F)
  
  #Remove events that are NAs
  final <- (lapply(test2, function(x) {
    if ((length(unlist(x)))<=1) {x <- NA}
    return(x)
  }))
  
  ###Merge the file
  test2 <- do.call('rbind', final)
  test2 <- as.data.frame(test2)
  test2[test2 == "NA" ] = NA
  all.movement <- test2
  #all.movement<-test2[order(test2$game.clock),]
  
  ##Lets join the movement to player id
  headers = c("team_id", "player_id", "x_loc", "y_loc", "radius", "game_clock", "shot_clock", "quarter","event.id")
  colnames(all.movement) <- headers
  all.movement<-data.frame(all.movement)
  all.movement<-all.movement[order(all.movement$game_clock),]
  
  home.players <- the.data.file$events$home$players[[1]]
  away.players <- the.data.file$events$visitor$players[[1]]
  colnames(home.players)[3] <- "player_id"
  colnames(away.players)[3] <- "player_id"
  
  ## Add the player name information to each movement moment
  home.movements<-merge(home.players, all.movement, by="player_id")
  away.movements<-merge(away.players, all.movement, by="player_id")
  ball.movement<-all.movement %>% filter(player_id == -1)
  ball.movement$jersey <- NA
  ball.movement$position <- NA
  ball.movement$team_id <- NA
  ball.movement$lastname <- "ball"
  ball.movement$firstname <- NA
  all.movements <- rbind(home.movements, away.movements,ball.movement)
  all.movements[, 6:13] <- lapply(all.movements[, 6:13], as.numeric)
  all.movements <- as.data.frame(all.movements) %>% dplyr::arrange(quarter,desc(game_clock),x_loc,y_loc)
  return(all.movements)
}




get_full_game <- function(tracking, plays){
  moments <- sportvu_convert_json(tracking) %>% 
    mutate(team = case_when(team_id == 1610612737 ~ "Hawks",
                            team_id == 1610612738 ~ "Celtics",
                            team_id == 1610612739 ~ "Cavaliers",
                            team_id == 1610612740 ~ "Pelicans",
                            team_id == 1610612741 ~ "Bulls",
                            team_id == 1610612742 ~ "Mavericks",
                            team_id == 1610612743 ~ "Nuggets",
                            team_id == 1610612744 ~ "Warriors",
                            team_id == 1610612745 ~ "Rockets",
                            team_id == 1610612746 ~ "Clippers",
                            team_id == 1610612747 ~ "Lakers",
                            team_id == 1610612748 ~ "Heat",
                            team_id == 1610612749 ~ "Bucks",
                            team_id == 1610612750 ~ "Timberwolves",
                            team_id == 1610612751 ~ "Nets",
                            team_id == 1610612752 ~ "Knicks",
                            team_id == 1610612753 ~ "Magic",
                            team_id == 1610612754 ~ "Pacers",
                            team_id == 1610612755 ~ "76ers",
                            team_id == 1610612756 ~ "Suns",
                            team_id == 1610612757 ~ "Trailblazers",
                            team_id == 1610612758 ~ "Kings",
                            team_id == 1610612759 ~ "Spurs",
                            team_id == 1610612760 ~ "Thunder",
                            team_id == 1610612761 ~ "Raptors",
                            team_id == 1610612762 ~ "Jazz",
                            team_id == 1610612763 ~ "Grizzlies",
                            team_id == 1610612764 ~ "Wizards",
                            team_id == 1610612765 ~ "Pistons",
                            team_id == 1610612766 ~ "Hornets",
                            is.na(team_id) ~ "ball"),
           game_clock = hms::as_hms(game_clock),
           firstname = replace_na(firstname, ""),
           full_name = paste(firstname, lastname)) %>% 
    group_by(quarter, event.id, game_clock) %>% 
    mutate(count = n()) %>% 
    ungroup() %>% 
    filter(count == 11) %>% 
    arrange(quarter, event.id, desc(game_clock), desc(shot_clock))
  
  full_game <- moments %>%
    select(-event.id) %>% 
    unique() %>%
    arrange(quarter, desc(game_clock), desc(shot_clock)) %>%
    slice(1:33) %>% 
    mutate(frame_num = rep(1:(nrow(.)/11), each = 11)) %>% 
    mutate(dist_to_ball = unlist(lapply(1:nrow(.), function(x) dist_to_ball(., x)))) #%>%
  #   group_by(frame_num) %>%
  #   mutate(ball_handler = ifelse(length(minN(dist_to_ball, 2)) > 1,
  #                                lag(ball_handler),
  #                                which(dist_to_ball == minN(dist_to_ball, 2)))) %>%
  #   ungroup()
  # 
  # ball_handler_vec <- (full_game %>%
  #                        group_by(frame_num) %>%
  #                        slice(ball_handler) %>%
  #                        unique())$full_name
  # 
  # full_game <- full_game %>%
  #   mutate(ball_handler = rep(ball_handler_vec, each = 11))
  # 
}



write_csv(get_full_game(input), output)