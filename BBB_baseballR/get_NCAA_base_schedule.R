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
get_team_schedule <- function(teamid, year, division=1,verbose=F){
  ### need a stop criteria here to make sure that the team id is correct
  assert_that(year>=2013,msg='you must provide a year that is equal to or greater than 2013')
  
  team <- master_ncaa_team_lu[(master_ncaa_team_lu$year == year) & 
                                (master_ncaa_team_lu$school_id == teamid) &
                                (master_ncaa_team_lu$division == division)
                                ,"school"] %>% pull()
  base_url <- "http://stats.ncaa.org"
  yearid = ncaa_season_id_lu[ncaa_season_id_lu$season == year,"id"] %>% pull()
  team_url <- paste(base_url, "/team/index/", yearid, "?org_id=", teamid, sep="")
  
  team_read <- try(team_url %>% read_html())
  if (class(team_read)=='try-error'){
    print(paste('Cannot connect to server for', team, 'in', year))
    return(NULL)
  }
  else{
    x <- team_read %>% html_nodes(".smtext") %>% html_text()
    x_df <- apply(x %>% matrix(ncol=3,byrow=T),2,stripwhite) %>% data.frame(stringsAsFactors = F)
    colnames(x_df) <- team_read %>% html_nodes("th") %>% html_text()
    
    game_ids <- get_game_id(team_read %>% html_nodes(".smtext .skipMask") %>% html_attr("href"))
    
    if(verbose){
      print(paste0("Processing data for: ",team))
    }
    
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
  
}


############################################
# Get schedule for every team over years
get_season_schedule = function(yr, div=1,conf=NULL){
  
  assert_that(year>=2013,msg='you must provide a year that is equal to or greater than 2013')
  if(!is.null(conference)){
    assert_that(conf %in% master_ncaa_team_lu$conference,
                msg='Conference name provided is not clear, please check master_ncaa_team_lu DF')
    teams_df <- master_ncaa_team_lu %>% filter(year==yr & division==div,conference==conf) %>% 
      select(school,conference,school_id,year) %>% 
      mutate(
        schedule = purrr::map2(school_id,year,get_team_schedule,division=div,verbose=T)
      ) %>% drop_na()
    teams_clean <- teams_df %>% unnest() %>% select(-school,-year)
  }else{
    teams_df <- master_ncaa_team_lu %>% filter(year==yr & division==div) %>% 
      select(school,conference,school_id,year) %>% 
      mutate(
        schedule = purrr::map2(school_id,year,get_team_schedule,division=div,verbose=T)
      ) %>% drop_na()
    teams_clean <- teams_df %>% unnest() %>% select(-school,-year)
  }
  
  return(teams_clean)  
}




