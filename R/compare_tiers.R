#'
#' Compare players within each tier
#' 
#' Call similar_games function for multiple players to get
#' aggregate statistics and their next game info
#' 
#' @param Vector of player names
#' @return list containing similar_games dataframes along with aggregate info
#' @export
compare_tiers = function(players){
  
  num = length(players)
  outputList = vector(mode = 'list', length = 0)
  
  next_game = get_next_opponent()
  
  games = suppressMessages(suppressWarnings(game_logs()))
  
  aggregate = data.frame(player = c(),
                         opponent = c(),
                         location = c(),
                         b2b = c(),
                         cluster = c(),
                         similar_games = c(),
                         similar_average = c(),
                         improvement = c(),
                         lastTen = c())
  
  current_clusters = suppressWarnings(suppressMessages(current_team_clusters()))
  
  for(i in players){
    
    currentTeam = games %>% 
      filter(namePlayer == i) %>%
      arrange(-numberGameTeamSeason) %>%
      select(slugTeam) %>%
      filter(row_number() == 1)
    
    currentTeam = currentTeam$slugTeam[1]
    
    next_opponent = next_game %>%
      filter(teamAbrv == currentTeam) %>%
      select(oppAbrv)
    
    next_opponent = next_opponent$oppAbrv[1]
    
    sim = similar_games(i, next_opponent)
    
    outputList[[i]] = sim
    
    thisTeam = next_game %>%
      filter(teamAbrv == currentTeam)
    
    oppCluster = current_clusters %>%
      filter(opp == next_opponent)
    
    simAvg = ifelse(is.data.frame(sim), mean(sim$fppg), NA)
    simGames = ifelse(is.data.frame(sim), nrow(sim), 0)
    simPast = ifelse(is.data.frame(sim), mean(sim$lastTen), NA)
    
    past_games = player_past_games(i)
    player_last_game = max(past_games$gameNum)
    pgs = past_games %>%
      filter(gameNum >= player_last_game - 10)
    
    df = data.frame(player = i,
                    opponent = next_opponent,
                    location = thisTeam$location,
                    b2b = thisTeam$b2b,
                    cluster = oppCluster$cluster,
                    similar_games = simGames,
                    similar_average = simAvg,
                    improvement = simAvg-simPast,
                    lastTen = round(mean(pgs$fppg)), 2)
    
    aggregate = aggregate %>%
      rbind(df)
  }
  
  outputList[['Total']] = aggregate
  
  return(outputList)
}
