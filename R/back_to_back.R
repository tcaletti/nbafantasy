#' 
#' Returns back to back player splits
#' 
#' Player averages for back to back games compared to others
#' 
#' @param player Nae of player
#' @return dataframe of player splits
#' @export
back_to_back = function(player) {
  
  stats = player_past_games(player)
  
  splits = stats %>%
    group_by(b2b) %>%
    summarise(games = n(),
              avg = mean(fppg, na.rm=T),
              vsLastTen = mean(ifelse(fppg >= lastTen, 1, 0), na.rm=T)) %>%
    mutate(player = player) %>%
    select(player, b2b, games, avg, vsLastTen)
  
  return(splits)
}