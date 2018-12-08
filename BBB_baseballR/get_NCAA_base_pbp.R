library(tidyverse)
library(XML)
library(stringr)
library(RCurl)

stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

score_fill=function(score_in){
  m=length(score_in)
  score_in[1]=0
  for(i in 2:m){
    if (is.na(score_in[i])){
      score_in[i]=score_in[i-1]
    }
  }
  return(score_in)
}

clean_games = function(game_id,year){
  base_url='http://stats.ncaa.org/game/play_by_play/'
  x= paste(base_url, game_id, sep='/')
  Sys.sleep(1)
  y=readHTMLTable(getURL(x))
  # Play by play is in table form
  y = y[which(!sapply(y,is.null))]
  
  if (length(y) == 0 | length(y) < ncol(y[[3]])) {
    print(paste("Play by Play data not available for game", game_id, sep=' '))
    next
  }
  else{
    j=1
    for (i in 1:length(y)){
      # Disgard NULL tables
      if (is.null(y[[i]])==FALSE){
        # Only take pbp tables (3 cols)
        if (ncol(y[[i]])==3){
          inn=as.data.frame(y[[i]])%>%
            mutate(inning=j,
                   game_id=game_id,
                   year=year)%>%
            select(year,game_id,inning,everything())
          j=j+1
          if(j==2){
            pbp = inn
          }else{
            pbp = rbind(pbp,inn)
          }
        }
      }
    }
  }
  pbp = pbp %>% mutate(away_team = colnames(pbp)[4],
             home_team = colnames(pbp)[6],
             away_score = as.integer(gsub('-.*', '', Score)),
             home_score = as.integer(gsub('.*-', '', Score)),
             away_score=score_fill(away_score),
             home_score=score_fill(home_score))%>%
    rename(away_text = 4,
           home_text = 6)%>%
    filter(substr(away_text,1,3)!='R: ')%>%
    select(year, game_id, inning, away_team, home_team, away_score, home_score, away_text, home_text)
  return(pbp)
}

# Get pbp from all games for one team
get_team_pbp=function(teamid, year, division=1){
  all_team_games=get_team_schedule(teamid, year, division=1)
  
  
  games=all_team_games%>%
    distinct(GameId, .keep_all = TRUE)%>%
    mutate(home_team=ifelse(Loc%in%c('H', 'N'), Team, Opp),
           away_team=ifelse(Loc=='H', Opp, Team ))%>%
    select(Year, Date, GameId, home_team, away_team)
  
    ### use purrr map here to
  game_pbp = games %>% mutate(
    pbp_raw = purrr::map2(GameId,Year,clean_games)
  )  
  pbp_final <- game_pbp %>% select(pbp_raw) %>% unnest()  
  return(pbp_final)
  }

# Get pbp from all games in one season
get_season_pbp=function(year, division=1,conference=NULL){
  if(!is.null(conference)){
    all_season_games=get_season_schedule(year, div=divison,conf=conference)
  }
  all_season_games=get_season_schedule(year, div=division)
  
  games=all_season_games %>%
    distinct(GameId, .keep_all = TRUE) %>%
    mutate(home_team=ifelse(Loc%in%c('H', 'N'), Team, Opp),
           away_team=ifelse(Loc=='H', Opp, Team )) %>% 
    select(Year, Date, GameId, home_team, away_team)
 
  season_pbp = games %>% mutate(
    pbp_raw = purrr::map2(GameId,Year,clean_games)
  )
  pbp_final <- season_pbp %>% select(pbp_raw) %>% unnest()  
  return(pbp_final)
}

