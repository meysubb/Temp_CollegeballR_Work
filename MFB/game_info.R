game_info <- function(year,
                      week = NULL,
                      team = NULL,
                      conference = NULL) {
  ## check if year is numeric
  assert_that(is.numeric(year),msg='Enter valid year')
  assert_that(is.numeric(week),msg='Enter valid week')
  options(stringsAsFactors = FALSE)
  play_base_url <-
    paste0("https://api.collegefootballdata.com/games?year=",
           year,
           "&")
  if(!is.null(team)){
    team = URLencode(team, reserved = T)
  }
  ### check for week
  ### then check for team
  ### check for conference
  param_len <- length(c(week, team, conference))

  if (param_len == 1) {
    if (!is.null(week)) {
      #week only
      url <- paste0(play_base_url, "week=", week)
    } else if (!is.null(team)) {
      # team
      url <- paste0(play_base_url, "team=", week)
    } else{
      # conference
      url <- paste0(play_base_url, "conference=", conference)
    }
  } else if(param_len == 2) {
    if (is.null(conference)) {
      ## team,week
      url <- paste0(play_base_url, "team=", team, "&week=", week)
    } else{
      ## conference,week
      url <-
        paste0(play_base_url, "week=", week, "&conference=", conference)
    }
  }
  game_df <- fromJSON(url)
  return(game_df)
}


