#' Cluster Definitions
#' 
#' Get explanations of cluster teams, statistical similarities
#' 
#' @return Dataframe of cluster definitions
#' @export 
cluster_definitions = function() {
  
  df = data.frame(cluster = seq(1,8),
                  definition = c('Average team, allows lots of 3s',
                                 'Fast paced team, average otherwise',
                                 'Average pace, bad defense and poor rebounding',
                                 'Slow paced, bad defense and allows 3s',
                                 'Slow paced, elite defense, good rebounding and forces turnovers',
                                 'Slow paced, good defense',
                                 'Very slow paced, average otherwise',
                                 'Fast paced, good defense and good rebounding'))
  
  return(df)
}