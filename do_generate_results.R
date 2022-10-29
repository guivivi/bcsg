# Arguments:
# gk_pos: Team's goalkeeper position at each period.
# ids_folders: id of the folder related to each match.
# half: period of the match.

do_generate_results <- function(gk_pos, ids_folders, half) {
  
  # Find the goalkeepers' position for the corresponding game's id:
  gk_pos_id <- gk_pos[names(gk_pos) == ids_folders][[1]]
  
  # Locate the match's id folder in the right directory:
  path_match <- paste0("data/data/matches/", ids_folders)
  
  # Read the match data:
  id_match <- fromJSON(paste0(path_match, "/match_data.json"))
  
  # Get the teams' ids and names, and add the goalkeepers' position in each period:
  id_teams_aux1 <- bind_rows(id_match$home_team, id_match$away_team) %>%
    rename(team_id = id, team_name = name) %>%
    select(team_id, team_name) %>%
    mutate(position_goalkeeper = gk_pos_id,
           period = 1) %>%
    mutate(ids_folders = ids_folders, .before = team_id)
  
  id_teams_aux2 <- id_teams_aux1 %>%
    mutate(position_goalkeeper = rev(id_teams_aux1$position_goalkeeper),
           period = 2)
  
  id_teams <- rbind(id_teams_aux1, id_teams_aux2)
  
  # Process the players' information:
  df_pl0 <- id_match$players
  
  # Remove the goalkeepers and substitutes:
  df_pl00 <- df_pl0 %>%
    filter(!player_role$name %in% c("Goalkeeper", "Substitute"))
  
  # Get the players' main information to work with:
  df_pl1 <- df_pl00 %>%
    select(first_name, last_name, birthday, trackable_object, team_id) %>%
    unite("name", c(first_name, last_name)) %>%
    mutate(role_name = df_pl00$player_role$name) %>%
    arrange(team_id, name)
  
  # Add the teams' names to the players' data:
  df_pl2 <- left_join(df_pl1, id_teams, by = "team_id")
  
  # Filter by period (half), since in each period the team's goalkeeper will
  # be located in a different side and the tactical units will be 
  # defined differently:
  df_pl3 <- df_pl2 %>%
    filter(period == half)
  
  # Read the tracking data:
  id_match_tr <- fromJSON(paste0(path_match, "/structured_data.json"))
  
  # Get all timestamps in which data is available:
  data_avai <- c()
  for (i in 1:nrow(id_match_tr)) {
    if (length(id_match_tr[i,]$data[[1]]) != 0 & !is.na(id_match_tr[i,]$time)) {
      data_avai <- c(data_avai, i)
    }
  }  
  
  data_time_all <- id_match_tr[data_avai,]$time
  
  # Choose timestamps every minute to have fewer observations (less computational burden)
  # and because the players' position will vary more every ten timestamps than if one by one.
  data_time <- data_time_all[seq(1, length(data_time_all), 10)]
  
  # Extract the tracking data from those timestamps at the period (half) of the match:
  df_tr0 <- id_match_tr %>%
    filter(time %in% data_time & period == half) 
  
  # Assign for each timestamp, period and team the corresponding player's tactical unit:
  df_res <- data.frame()
  for (i in 1:nrow(df_tr0)) {
    #cat("ITERATION:", i, "\n")
    
    if (nrow(df_tr0$data[[i]]) == 0) {
      next
    }else if (!"trackable_object" %in% colnames(df_tr0$data[[i]])) {
      next
    }else{
      df_tr1 <- df_tr0$data[[i]] %>%
        select(x, y, trackable_object)
      
      df_tr2 <- left_join(df_pl3, df_tr1, by = "trackable_object")
      
      df_tr3 <- df_tr2 %>%
        filter(role_name != "Substitute")
      
      df_tr4 <- df_tr3 %>%
        group_by(period, team_name, name) %>%
        do(data.frame(tactical_unit = do_assign_players_tactical_unit(.))) %>%
        ungroup()
      
      df_tr5 <- left_join(df_tr3, df_tr4, by = c("period", "team_name", "name"))      
      
      df_res <- rbind(df_res, df_tr5) 
    }
  }
  
  return(df_res)
}
  