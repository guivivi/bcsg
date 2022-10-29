# Arguments:
# data: Data frame with the needed tracking information.

do_assign_players_tactical_unit <- function(data) {
 
 # Collect the x coordinates and the goalkeeper's position:
 x <- data$x
 pos_gk <- data$position_goalkeeper
 
 # Assign tactical units according the x coordinates:
 if (is.na(x)) {
   tactical_unit <- "not_available"
 }else{
   if (pos_gk == "left") {
     if (x >= -52.5 & x <= -30) {
       tactical_unit <- "defensive_line"
     }else if (x > -30 & x <= 30) {
       tactical_unit <- "midfield_line"
     }else if (x > 30 & x <= 52.5) {
       tactical_unit <- "offensive_line"
     }else{
       tactical_unit <- "off_field" # when coordinates go beyond field limits.
     }
   }else if (pos_gk == "right") {
     if (x <= 52.5 & x >= 30) {
       tactical_unit <- "defensive_line"
     }else if (x < 30 & x >= -30) {
       tactical_unit <- "midfield_line"
     }else if (x < -30 & x >= -52.5) {
       tactical_unit <- "offensive_line"
     }else{
       tactical_unit <- "off_field" # when coordinates go beyond field limits.
     }
   }else{
     stop("position_goalkeeper must be either 'left' or 'right'.")
   }
 }
  
  return(tactical_unit)
}