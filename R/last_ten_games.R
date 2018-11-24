#' Calculate last 10 games statistics
#' 
#' Calculate a teams advanced statistics in their 
#' last ten games
#' 
#' @param games Dataframe of games taken from the nbastatR package
#' @param team String of team abbreviation to get their statistics
#' @param game Integer for team's game number of season
#' @return Dataframe with team advanced statistics
#' @export 
last_ten_games = function(games, team, game) {
  
  teamGames = games %>%
    filter(slugTeam == team, numberGameTeamSeason >= game-10, numberGameTeamSeason < game) 
  
  teamStats = teamGames %>%
    summarise(reb = sum(trebTeam),
              pts = sum(ptsTeam),
              oreb = sum(orebTeam),
              fga = sum(fgaTeam),
              fgm = sum(fgmTeam),
              fg3a = sum(fg3aTeam),
              fg3m = sum(fg3mTeam),
              tov = sum(tovTeam),
              fta = sum(ftaTeam),
              n = n()) %>%
    mutate(possessions = fga+tov-oreb+.475*fta)
  
  opponentStats = games %>%
    filter(slugOpponent == team, idGame %in% teamGames$idGame) %>%
    summarise(oppReb = sum(trebTeam),
              oppPts = sum(ptsTeam),
              oppOReb = sum(orebTeam),
              oppFGA = sum(fgaTeam),
              oppFGM = sum(fgmTeam),
              opp3A = sum(fg3aTeam),
              opp3M = sum(fg3mTeam),
              oppTov = sum(tovTeam),
              oppFta = sum(ftaTeam),
              oppAst = sum(astTeam),
              firstGame = min(dateGame),
              lastGame = max(dateGame),
              oppN = n()) %>%
    mutate(oPossessions = oppFGA+oppTov-oppOReb+.475*oppFta)
  
  
  results = teamStats %>%
    cbind(opponentStats) %>%
    mutate(team = team,
           gameNum = game,
           pace = (possessions+oPossessions)/(n+oppN),
           oEff = pts*100/possessions,
           dEff = oppPts*100/possessions,
           net = oEff - dEff,
           efg = (fgm + .5*fg3m)/fga,
           rebs = reb/(reb+oppReb),
           drebs = (reb-oreb)/(reb-oreb+oppOReb),
           orebs = oreb/(oreb+oppReb-oppOReb),
           oppFtr = oppFta/oppFGA,
           oppEFG = (oppFGM + .5*opp3M)/oppFGA,
           opp3r = opp3A/oppFGA,
           tov = tov/possessions,
           oppAsts = oppAst/oppFGM,
           oppTov = oppTov/oPossessions) %>%
    select(team, gameNum, firstGame, lastGame, pace, oEff, dEff, net, efg, rebs, drebs, orebs, tov, oppFtr, oppEFG, opp3r, oppAsts, oppTov)
  
  return(results)
}