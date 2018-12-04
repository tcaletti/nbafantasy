#' Player games against similar opponents
#'
#' Get 5 most recent games the player had against similar
#' teams as their upcoming games
#'
#' @param player String of player name
#' @param team String of upcoming team
#' @return Dataframe of games against similar teams
#' @export
similar_games = function(player, team) {

  games = suppressWarnings(suppressMessages(player_past_games(player, extra=T)))

  opponent = suppressWarnings(suppressMessages(current_team_clusters())) %>%
    filter(opp == team)

  teamCluster = opponent$cluster

  similar = games %>%
    filter(cluster == teamCluster) %>%
    head(5) %>%
    select(opponent, gameNum, fppg, seasonAvg, lastTen, pace, dEff, rebs, oppEFG, opp3r, oppTov, cluster)

  if(nrow(similar) == 0) {
    return('No similar games this season')
  } else {
    return(similar)
  }
}
