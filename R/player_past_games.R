#' Get player statistics for all games
#' 
#' Retrieve player fantasy points and opponent statistics
#' for all games in 2019
#' 
#' @param player String of player name
#' @return Dataframe of all player games
#' @export 
player_past_games = function(player){
  
  games = game_logs(result_types = 'team')
  
  playerGames = game_logs() %>%
    filter(namePlayer == player) %>%
    mutate(dk = dkPoints(pts, treb, ast, stl, blk, fg3m, tov))
  
  playerOpp = data.frame()
  
  for(i in 1:nrow(playerGames)){
    
    df = last_ten_games(games, playerGames$slugOpponent[i], playerGames$numberGameTeamSeason[i])
    
    playerOpp = playerOpp %>%
      rbind(df)
  }
  
  stats = playerOpp %>%
    select(pace, dEff, rebs, oppEFG, opp3r, oppTov)
  
  load('cluster_algorithm')
  
  cluster = cl_predict(clusterAlg, newdata = stats) %>%
    as.vector()
  
  fppg = playerGames$dk
  
  pl = playerOpp %>%
    cbind(cluster) %>%
    cbind(fppg)
  
  seasonAvg = c(NA)
  lastTen = c(NA)
  
  for(i in 2:nrow(pl)){
    
    gn = pl$gameNum[i]
    
    gs = pl %>%
      filter(gameNum < gn) %>%
      summarise(m = mean(fppg))
    
    recent = pl %>%
      filter(gameNum < gn, gameNum >= gn-10) %>%
      summarise(m = mean(fppg))
    
    seasonAvg = c(seasonAvg, gs$m)
    lastTen = c(lastTen, recent$m)
  }
  
  results = pl %>%
    cbind(seasonAvg) %>%
    cbind(lastTen) %>%
    arrange(-gameNum) %>%
    select(team, gameNum, fppg, seasonAvg, lastTen, cluster, pace, dEff, rebs, oppEFG, opp3r, oppTov)
  
  return(results)
}