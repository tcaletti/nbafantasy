#' Current Team Cluster Groups
#'
#' Get all teams current advanced statistics and clusters
#'
#' @return Dataframe of team statistics and cluster group
#' @export
current_team_clusters = function(){

  clusterAlg = nbafantasy:::clusterAlg

  teams = currentTeamStatus()

  team_stats = teams %>%
    select(-opp, -gameNum, -lastGame, -firstGame)

  cluster = cl_predict(clusterAlg, newdata=team_stats) %>%
    as.vector()

  teamClusters = teams %>%
    cbind(cluster)

  return(teamClusters)
}
