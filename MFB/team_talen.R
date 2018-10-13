team_talent <- function(year = NULL) {
  if (is.null(year)) {
    url <- "https://api.collegefootballdata.com/talent"
  } else{
    assert_that(is.numeric(year), msg = "Enter valid year")
    url <-
      paste0("https://api.collegefootballdata.com/talent?year=",
             year)
  }
  df <- fromJSON(url)
  return(df)
}
