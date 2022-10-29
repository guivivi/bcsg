# Arguments:
# ids_folders: id of the folder related to each match.

do_plot_first_positioning <- function(ids_folders) {
  
  # Locate the match's id folder in the right directory:
  path_match <- paste0("data/data/matches/", ids_folders)
  
  # Read the match data:
  id_match <- fromJSON(paste0(path_match, "/match_data.json"))
  
  # Get the teams' ids and names:
  id_teams <- bind_rows(id_match$home_team, id_match$away_team) %>%
    rename(team_id = id, team_name = name) %>%
    select(team_id, team_name)
  
  # Process the players' information:
  df_pl0 <- id_match$players
  
  # Get the players' main information to work with:
  df_pl1 <- df_pl0 %>%
    select(first_name, last_name, birthday, trackable_object, team_id) %>%
    unite("name", c(first_name, last_name)) %>%
    mutate(role_name = df_pl0$player_role$name) %>%
    arrange(team_id, name)
  
  # Add the teams' names to the players' data:
  df_pl2 <- left_join(df_pl1, id_teams, by = "team_id")
  
  # Read the tracking data:
  id_match_tr <- fromJSON(paste0(path_match, "/structured_data.json"))
  
  # Get the first timestamp in which data is available:
  first_data <- c()
  for (i in 1:nrow(id_match_tr)) {
    if (length(id_match_tr[i,]$data[[1]]) != 0 & !is.na(id_match_tr[i,]$time)) {
      first_data <- i
      break
    }
  }  
  start_time <- id_match_tr[first_data,]$time
  
  # Extract the tracking data from that first timestamp at the 
  # first period (half) of the match:
  df_tr0 <- id_match_tr %>%
    filter(time == start_time & period == 1) 
  
  df_tr1 <- df_tr0$data[[1]] %>%
    select(-track_id)
  
  # Add the tracking coordinates to the players' data:
  df_tr2 <- left_join(df_pl2, df_tr1, by = "trackable_object")
  
  # Visualize the players' positioning in this first timestamp:
  gg <- ggplot(df_tr2, aes(x = x, y = y, label = name)) +
    geom_point() +
    geom_label_repel(aes(fill = team_name)) +
    guides(fill = guide_legend(override.aes = aes(label = ""))) +
    xlim(c(-52.5, 52.5)) +
    ylim(c(-34, 34)) +
    labs(title = "Players' position at the first timestamp with data available.",
         subtitle = paste("Timestamp:", start_time))
  
  return(gg)
}