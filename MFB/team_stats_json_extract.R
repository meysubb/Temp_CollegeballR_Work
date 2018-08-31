### Scrape NCAA College Football


### Team stats
library(jsonlite)

pre_string <- "http://data.ncaa.com/sites/default/files/data/"
post_string_options <- "teamStats.json"

tamu_json <- fromJSON("http://data.ncaa.com/sites/default/files/data/game/football/fbs/2017/09/03/texas-am-ucla/teamStats.json")

team_info <- tamu_json$meta$teams

team_stats <- tamu_json$teams[[2]]

p <- team_stats[[1]]
### pull breakdown out, and look through the dataframe and append all of these
### neeed to do to this for both home and away team
### what should the end result look like for team stats?
l <- p$breakdown[[1]]
l$stat <- paste0(p$stat[1],"-",l$stat)
