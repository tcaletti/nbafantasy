#' 
#' Function to get every teams next game in 2018-2019 NBA season
#' 
#' Function pulls season schedule and figures out teams next game
#' 
#' @return Dataframe of every teams next game
get_next_opponent = function() {
  
  team_abbreviations = c('Boston Celtics' = 'BOS', 
                         'Golden State Warriors' = 'GSW',
                         'Charlotte Hornets' = 'CHA',
                         'Detroit Pistons' = 'DET',
                         'Indiana Pacers' = 'IND',
                         'Orlando Magic'= 'ORL',
                         'New York Knicks' = 'NYK',
                         'Toronto Raptors' = 'TOR',
                         'Houston Rockets' = 'HOU',
                         'San Antonio Spurs' = 'SAS',
                         'Sacramento Kings' = 'SAC',
                         'LA Clippers' = 'LAC',
                         'Phoenix Suns' = 'PHX',
                         'Philadelphia 76ers' = 'PHI',
                         'Washington Wizards' = 'WAS',
                         'Portland Trail Blazers' = 'POR',
                         'Brooklyn Nets' = 'BKN',
                         'Memphis Grizzlies' = 'MEM',
                         'Minnesota Timberwolves' = 'MIN',
                         'New Orleans Pelicans' = 'NOP',
                         'Milwaukee Bucks' = 'MIL',
                         'Utah Jazz' = 'UTA',
                         'Miami Heat' = 'MIA',
                         'Chicago Bulls' = 'CHI',
                         'Dallas Mavericks' = 'DAL',
                         'Denver Nuggets'= 'DEN',
                         'Los Angeles Lakers' = 'LAL',
                         'Cleveland Cavaliers' = 'CLE',
                         'Oklahoma City Thunder' = 'OKC',
                         'Atlanta Hawks' = 'ATL')
  
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
  
  next_game = next_game %>%
    mutate(teamAbr = unname(team_abbreviations[team]),
           oppAbr = unname(team_abbreviations[opponent]))
  
  return(next_game)
}





