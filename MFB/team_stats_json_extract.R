### Scrape NCAA College Football
### Team stats
library(jsonlite)

pre_string <- "http://data.ncaa.com/sites/default/files/data/"
post_string_options <- "teamStats.json"

tamu_json <- fromJSON("http://data.ncaa.com/sites/default/files/data/game/football/fbs/2017/09/03/texas-am-ucla/teamStats.json")

team_info <- tamu_json$meta$teams

team_stats <- tamu_json$teams[[2]]


### pull breakdown out, and look through the dataframe and append all of these
### neeed to do to this for both home and away team
### what should the end result look like for team stats?
extract_team_stats <- function(df){
  main_stat <- df
  add_lst <- df$breakdown
  names(add_lst) <- main_stat$stat
  main_stat$breakdown <- NULL
  add_df <- dplyr::bind_rows(add_lst,.id="id")
  add_df$stat <- paste0(add_df$id,"_",add_df$stat)
  add_df$id <- NULL
  stat_df <- rbind(main_stat,add_df)
}

team_stats <- function(team_names,team_lst){
  l_teams <- team_lst$teams$stats
  len <- length(l_teams)
  stat_df <- data.frame()
  for(i in 1:len){
    df <- extract_team_stats(l_teams[[i]])
    df$name <- team_names[[i]]
    stat_df <- rbind(stat_df,df)
  }
  return(stat_df)
}
