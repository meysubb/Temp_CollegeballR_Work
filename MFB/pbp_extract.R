##play-by-play scrape
pbp_data <- function(year,week=1,team=NULL){
  ## Inputs
  ## Year, Week, Team
  require(jsonlite)
  options(stringsAsFactors = FALSE)
  play_base_url <- "https://api.collegefootballdata.com/plays?"
  full_url <- paste0(play_base_url,"year=",year,"&week=",week)
  raw_play_df <- fromJSON(full_url)
  raw_play_df <- do.call(data.frame,raw_play_df)

  ## look for team plays
  if(!is.null(team)){
    l_teams <- c(unique(raw_play_df$offense),unique(raw_play_df$defense))
    unique_teams <- unique(l_teams)
    assert_that(team %in% unique_teams,msg="Can't seem to find that team.")
    play_df <- raw_play_df %>% filter(offense == team | defense == team)
  }
  else{
    play_df <- raw_play_df
  }

  return(play_df)
}
