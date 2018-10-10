conf_url <- "https://api.collegefootballdata.com/conferences"
conf_types_df <- fromJSON(conf_url)

conference_team <- function(conference){
  ### check whether conference exists
  base_url <- "https://api.collegefootballdata.com/teams?conference="
  url <- paste0(baes_url,conference)
  df <- fromJSON(url)
  return(df)
}
