conf_url <- "https://api.collegefootballdata.com/conferences"
conf_types_df <- fromJSON(conf_url)
conf_types_df$short_name <- conf_types_df$short_name %>% str_replace("Conference", "") %>% trimws()
conf_types_df$short_name[6] <- "Conference USA"

conference_team <- function(conference) {
  ### check whether conference exists
  base_url <-
    "https://api.collegefootballdata.com/teams?conference="
  if(length(conference)==3){
    url <- paste0(base_url, conference)
  }else{
    assert_that(conference  %in%  conf_types_df$short_name)
    ind <- which(conference == conf_types_df$short_name)
    url <- paste0(base_url,conf_types_df$abbreviation[ind])
  }
  df <- fromJSON(url)
  return(df)
}

