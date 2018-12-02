#'
#'Get player splits for home vs away
#'
#'Return number of games and average fantasy points for home and away
#'
#'@param player Name of player
#'@return dataframe of player splits
#'@export
home_vs_away = function(player) {
  
  stats = player_past_games(player)
  
  splits = stats %>%
    group_by(home) %>%
    summarise(games = n(),
              avg = mean(fppg),
              vsLastTen = mean(ifelse(fppg >= lastTen, 1, 0), na.rm=T)) %>%
    mutate(player = player) %>%
    select(player, home, games, avg, vsLastTen)
  
  return(splits)
}