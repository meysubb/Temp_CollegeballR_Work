library(dplyr)
##play-by-play scrape
## Store this as a data object
pt_url <- "https://api.collegefootballdata.com/play/types"
play_types <- fromJSON(pt_url)
play_type_df <- play_types %>% mutate(text = tolower(text))

library(assertthat)

pbp_data <- function(year,
                     week = 1,
                     team = NULL,
                     play_type = NULL) {
  require(jsonlite)
  options(stringsAsFactors = FALSE)
  if (!is.null(play_type)) {
    text <- play_type %in% play_type_df$text
    abbr <- play_type %in% play_type_df$abbreviation
    pt <-
      assert_that((text |
                     abbr) == T, msg = "Incorrect play type selected, please look at the available options in the Play Type DF.")
    if (text) {
      pt_id = play_type_df$id[which(play_type_df$text == play_type)]
    } else{
      pt_id = play_type_df$id[which(play_type_df$abbreviation == play_type)]
    }
  }
  ## Inputs
  ## Year, Week, Team
  play_base_url <- "https://api.collegefootballdata.com/plays?"
  if (!is.null(play_type) & !is.null(team)) {
    # no play type, no team
    full_url <- paste0(play_base_url, "year=", year, "&week=", week)
  } else{
    # no team, play_type
    if (!is.null(play_type)) {
      full_url <-
        paste0(play_base_url,
               "year=",
               year,
               "&week=",
               week,
               "&playType=",
               pt_id)
    } else if (!is.null(team)) {
      # no team, play_type
      full_url <-
        paste0(
          play_base_url,
          "year=",
          year,
          "&week=",
          week,
          "&team=",
          URLencode(team, reserved = T)
        )
    } else{
      # team & play type
      full_url <-
        paste0(
          play_base_url,
          "year=",
          year,
          "&week=",
          week,
          "&team=",
          URLencode(team, reserved = T),
          "&playType=",
          pt_id
        )
    }
  }
  
  raw_play_df <- fromJSON(full_url)
  raw_play_df <- do.call(data.frame, raw_play_df)
  play_df <- raw_play_df
  
  return(play_df)
}


