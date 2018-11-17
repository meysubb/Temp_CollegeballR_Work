library(tidyverse)
library(rvest)
library(snakecase)

stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

# base url
base_url <- "http://stats.ncaa.org"

get_game_id=function(x){
  # Get results
  game_id <- as.numeric(gsub("^.*game/index/([0-9]*).*$", "\\1", x))
  return(game_id)
}


# Get one team's schedule in one year
get_team_schedule <- function(teamid, year, division=1){
  ### need a stop criteria here to make sure that the team id is correct
  assert_that(year>=2013,msg='you must provide a year that is equal to or greater than 2013')
  
  
  base_url <- "http://stats.ncaa.org"
  yearid = ncaa_season_id_lu[ncaa_season_id_lu$season == year,"id"] %>% pull()
  team_url <- paste(base_url, "/team/index/", yearid, "?org_id=", teamid, sep="")
  
  team_read <- team_url %>% read_html()
  x <- team_read %>% html_nodes(".smtext") %>% html_text()
  x_df <- apply(x %>% matrix(ncol=3,byrow=T),2,stripwhite) %>% data.frame(stringsAsFactors = F)
  colnames(x_df) <- team_read %>% html_nodes("th") %>% html_text()
  
  game_ids <- get_game_id(team_read %>% html_nodes(".smtext .skipMask") %>% html_attr("href"))
 
  team <- master_ncaa_team_lu[(master_ncaa_team_lu$year == year) & (master_ncaa_team_lu$school_id == teamid),"school"] %>% pull()
  x_df <- x_df %>% mutate(
    game_id = game_ids,
    team = team,
    year = year,
    loc = case_when(
      grepl("^@", Opponent) ~ "A",
      grepl(" @ ", Opponent, fixed = TRUE) ~ "N",
      TRUE ~ "H"
    ),
    just_runs_result = gsub("\\([^()]*\\)", "", str_sub(Result,3,-1)),
    team_runs = as.numeric(gsub("\\D", "", gsub("-.*$", "", just_runs_result))),
    opp_runs = as.numeric(gsub(".*-", "", just_runs_result)),
    win = case_when(team_runs<opp_runs ~ "L",
                    team_runs>opp_runs ~ "W"),
    innings = as.numeric(ifelse(
      gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", Result, perl = T) == "",
      9,
      gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", Result, perl = T)
    )),
    opp = case_when(
      loc == "A" ~ gsub("@", "", Opponent),
      loc == "H" ~ Opponent,
      loc == "N" ~ substring(Opponent, 1, regexpr("@", Opponent) - 2)
    ),
    total_w = cumsum(ifelse(win == "W",1,0)),
    total_l = cumsum(ifelse(win == "L",1,0)),
    record = paste0(total_w,"-",total_l)
  )
  
  team_schedule <- x_df %>% select(year,Date,game_id,team,opp,loc,win,team_runs,opp_runs,record)
  ## proper capitlization because I'm OCD.
  colnames(team_schedule) <- to_upper_camel_case(colnames(team_schedule))

  return(team_schedule)
}


############################################
### come back and use purrr map 
### to go through all teams for taht year
### and scrape schedule. 
### maybe don't display records? or figure that out

# Get schedule for every team over years
get_season_schedule = function(year_start, year_end, division=1){
  if(year_start>year_end){
    print('End year must same as, or later than, starting year')
  }
  else{
    year_ids = get_year_ids(year_start, year_end, division=1)
    team_ids = get_team_ids(year_start, year_end, division=1)
    
    for (i in 1:nrow(team_ids)){
      for (j in 1:nrow(year_ids)){
        team <- toString(team_ids[i,'team'])
        teamid <- toString(team_ids[i,'id'])
        
        year <- toString(year_ids[j,'year'])
        yearid <- toString(year_ids[j,'year_id'])
        
        team_url <- paste(base_url, "/team/index/", yearid, "?org_id=", teamid, sep="")
        
        x <- try(scan(team_url, what="", sep="\n"))
        if (class(x)=='try-error'){
          print(paste('Cannot connect to server for', team, 'in', year))}
        else{
          if (length(grep(">\\d\\d/\\d\\d/\\d\\d\\d\\d<", x))==0){
            print(paste(team,' did not play any games in ', year, sep=""))}
          else{
            game_results=get_game_results(x)
            game_dates=get_game_date(x)
            game_opploc=get_game_opponent(x)
            game_id=as.numeric(get_game_id(x))
            # Put everything in a data frame
            team_schedule <- data.frame(year=year, date=game_dates, game_id=game_id, team=team, opploc=game_opploc, result=game_results, stringsAsFactors=FALSE)%>%
              mutate(
                # location = home (H) / away (A) / neutral (N)
                loc = case_when(
                  grepl("^@", opploc) ~ "A",
                  grepl(" @ ", opploc, fixed=TRUE) ~ "N",
                  TRUE ~ "H"),
                # opponent
                opp = case_when(
                  loc=="A" ~ gsub("@","",opploc),
                  loc=="H" ~ opploc,
                  loc=="N" ~ substring(opploc, 1, regexpr("@", opploc)-2)))%>%
              # select important all but opploc
              select(year, date, game_id, team, opp, loc, result)
            if (i==1){
              schedule <- team_schedule} 
            else{
              schedule <- rbind(schedule, team_schedule)}
          }
        }
      }
    }
    return(schedule)
  }
}




