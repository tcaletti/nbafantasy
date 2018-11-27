#' Get current statistics for every team
#'
#' Gets the last ten games advanced statistics for
#' every team up to date
#'
#' @return Dataframe of team statistics
#' @export
currentTeamStatus = function(){

  games = game_logs(result_types = 'team')

  gameStatus = games %>%
    group_by(slugTeam) %>%
    summarise(lastGame = max(numberGameTeamSeason)) %>%
    mutate(nextGame = lastGame + 1)

  currentTeams = data.frame()

  for(i in 1:nrow(gameStatus)){

    df = last_ten_games(games, gameStatus$slugTeam[i], gameStatus$nextGame[i]) %>%
      rename(opp = team) %>%
      select(-oEff, -net, -efg, -drebs, -orebs, -tov, -oppFtr, -oppAsts)

    currentTeams = currentTeams %>%
      rbind(df)
  }

  return(currentTeams)
}
