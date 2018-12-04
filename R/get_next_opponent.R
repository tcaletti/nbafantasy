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

  last_home_game = stats %>%
    filter(result != '-') %>%
    arrange(desc(date)) %>%
    group_by(homeTeam) %>%
    filter(row_number() == 1)

  last_away_game = stats %>%
    filter(result != '-') %>%
    arrange(desc(date)) %>%
    group_by(awayTeam) %>%
    filter(row_number() == 1)

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
                         date = c(),
                         b2b = c())

  for(i in unique(home$homeTeam)){

    h = home %>%
      filter(homeTeam == i)
    a = away %>%
      filter(awayTeam == i)

    h_last = last_home_game %>%
      filter(homeTeam == i)

    a_last = last_away_game %>%
      filter(awayTeam == i)

    if(h_last$date < a_last$date) {
      last_game = a_last$date
    } else {
      last_game = h_last$date
    }

    if(h$date < a$date) {

      if(last_game == h$date - 1){
        is_b2b = T
      } else {
        is_b2b = F
      }

      next_game = next_game %>%
        rbind(data.frame(team = i, opponent = h$awayTeam, location = 'H', date = h$date, b2b = is_b2b))
    } else {

      if(last_game == a$date - 1){
        is_b2b = T
      } else {
        is_b2b = F
      }

      next_game = next_game %>%
        rbind(data.frame(team = i, opponent = a$homeTeam, location = 'A', date = a$date, b2b = is_b2b))
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





