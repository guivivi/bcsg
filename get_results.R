# Load packages:
library(jsonlite)
library(tidyverse)
library(openxlsx)

# Set working directory to the main root folder:
setwd("")

# Source functions:
source("code/do_assign_players_tactical_unit.R")
source("code/do_generate_results.R")

# Games's ids:
ids_folders <- list.files("data/data/matches/")

# Teams' positions at the start of the match looking at the plots
# obtained with do_plot_first_positioning():
gk_pos <- list(c("left", "right"), c("right", "left"), c("right", "left"),
               c("right", "left"), c("left", "right"), c("left", "right"),
               c("left", "right"), c("left", "right"), c("left", "right"))
names(gk_pos) <- ids_folders 

# For each match, assign the tactical units at each period:
df_res_id <- data.frame()
s0 <- Sys.time()
for (i in 1:length(ids_folders)) {
  cat("ID MATCH:", ids_folders[i], "\n")
  for (j in 1:2) {
    cat("HALF:", j, "\n")
    
    res_iter <- do_generate_results(gk_pos, ids_folders[i], j) # "2417", 1
    df_res_id <- rbind(df_res_id, res_iter)
  }  
  cat("DONE", "\n")
}
s1 <- Sys.time()
s1 - s0
#Time difference of 32.33749 mins
#save(df_res_id, file = "code/df_res_id.RData")
#load("code/df_res_id.RData")

# Count the number of times that each tactical unit is assigned and compute
# the percentage of them.
df_res_id_perc <- df_res_id %>%
  filter(!tactical_unit %in% c("not_available", "off_field")) %>%
  group_by(team_name, name, tactical_unit) %>%
  summarise(n = n()) %>%
  mutate(perc = (n / sum(n)) * 100) %>%
  ungroup() %>%
  mutate(perc = round(perc, 2))
#write.xlsx(df_res_id_perc, "code/assignation_players_tactical_units.xlsx")

# Extract the most common tactical unit for each team's player:
df0 <- df_res_id_perc %>%
  group_by(team_name, name) %>%
  filter(perc == max(perc)) %>%
  ungroup() 

# Filter by team:
teams <- sort(unique(df0$team_name))
teams
df1 <- df0 %>%
  filter(team_name == teams[1]) %>%
  select(name, tactical_unit)

# Add the percentages:
df1_tu <- df_res_id_perc %>%
  mutate(perc = paste0(perc, "%")) %>%
  filter(team_name == teams[1]) %>%
  select(name, tactical_unit, perc) %>%
  pivot_wider(names_from = tactical_unit, values_from = perc)

df1 <- left_join(df1, df1_tu, by = "name")

# Other aspects:
df_res_id_perc %>%
  filter(tactical_unit == "defensive_line") %>%
  slice_max(perc, n = 10)

df_res_id_perc %>%
  filter(tactical_unit == "offensive_line") %>%
  slice_max(perc, n = 10)
