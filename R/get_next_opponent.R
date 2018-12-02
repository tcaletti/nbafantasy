#' 
#' Function to get every teams next game in 2018-2019 NBA season
#' 
#' Function pulls season schedule and figures out teams next game
#' 
#' @return Dataframe of every teams next game
get_next_opponent = function() {
  
  url = 'https://fixturedownload.com/results/nba-2018'
  x_path = '/html/body/div[2]/div/table'
  
  table = suppressWarnings(url %>%
    rvest::html() %>%
    rvest::html_nodes(xpath = x_path) %>%
    rvest::html_table())
  
  stats = table[[1]]
  names(stats) = c('roundNum', 'date', 'location', 'homeTeam','awayTeam', 'result')
  stats$date = as.Date(stats$date, '%d/%m/%Y %H:%M')
  
  home = stats %>%
    filter(result == '-') %>%
    group_by(homeTeam) %>%
    filter(row_number() == 1)
  
  away = stats %>%
    filter(result == '-') %>%
    group_by(awayTeam) %>%
    filter(row_number() == 1)
  
  next_game = data.frame(team = c(),
                         opponent = c(),
                         location = c(),
                         date = c())
  
  for(i in unique(home$homeTeam)){
    
    h = home %>%
      filter(homeTeam == i)
    a = away %>%
      filter(awayTeam == i)
    
    if(h$date < a$date) {
      
      next_game = next_game %>%
        rbind(data.frame(team = i, opponent = h$awayTeam, location = 'H', date = h$date))
    } else {
      
      next_game = next_game %>%
        rbind(data.frame(team = i, opponent = a$homeTeam, location = 'A', date = a$date))
    }
  }
  
  teamAbrv = c()
  oppAbrv = c()
  
  for(i in 1:30){

    teamAbrv = c(teamAbrv, team_abbreviations(next_game[i,1]))
    oppAbrv = c(oppAbrv, team_abbreviations(next_game[i,2]))

  }

  next_game = next_game %>%
  cbind(teamAbrv) %>%
  cbind(oppAbrv)
  
  return(next_game)
}





