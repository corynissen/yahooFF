
library(httr)
library(XML)
library(RJSONIO)
library(ggplot2)

# saved my yahoo keys to a file, now, read them in...
creds <- read.table("~/cn/personal/keys/yahoo.txt", stringsAsFactors=F)
consumer.key <- creds[1,1]
consumer.secret <- creds[2,1]
token.url <- "https://api.login.yahoo.com/oauth/v2/"

yahoo <- oauth_endpoint("get_request_token", "request_auth", "get_token",
  base_url = token.url)
myapp <- oauth_app("yahoo", key = consumer.key, secret= consumer.secret)
token <- oauth1.0_token(yahoo, myapp)
sig <- sign_oauth1.0(myapp, token$oauth_token, token$oauth_token_secret)

# need to get game id for my league...
ff.url <- "http://fantasysports.yahooapis.com/fantasy/v2/game/nfl?format=json"
game.key.json <- GET(ff.url, sig)
game.key.list <- fromJSON(as.character(game.key.json), asText=T)
game.key <- game.key.list$fantasy_content$game[[1]]["game_key"]

# my personal leagueid, you will have to use your own, mine is private
league.id <- "418826"
league.key <- paste0(game.key, ".l.", league.id)
league.url <- "http://fantasysports.yahooapis.com/fantasy/v2/league/"

standings.json <- GET(paste0(league.url, league.key, "/standings?format=json"), 
  sig)
standings.list <- fromJSON(as.character(standings.json), asText=T)

my.team.id <- "4"
my.team.key <- paste0(league.key, ".t.", my.team.id)
team.url <- "http://fantasysports.yahooapis.com/fantasy/v2/team/"
# lots of endpoints to play with, more here... 
# http://developer.yahoo.com/fantasysports/guide/
my.team.stats.json <- GET(paste0(team.url, my.team.key, "/stats?format=json"), 
  sig)
my.team.standings.json <- GET(paste0(team.url, my.team.key, 
  "/standings?format=json"), sig)
my.team.matchups.json <- GET(paste0(team.url, my.team.key, 
  "/matchups?format=json"), sig)
my.team.matchups.list <- fromJSON(as.character(my.team.matchups.json), asText=T)

# get the opponent scores for my matchups for the entire season
tmp <- my.team.matchups.list$fantasy_content["team"][[1]][[2]]$matchups
opp.score <- tmp$'0'$matchup$`0`$teams$`1`$team[[2]]$team_points["total"]
opp.score <- c(opp.score, sapply(as.character(1:12),   
  function(x)tmp[x][[x]]$matchup$`0`$teams$`1`$team[[2]]$team_points$total))
my.score <- tmp$'0'$matchup$`0`$teams$`0`$team[[2]]$team_points["total"]
my.score <- c(my.score, sapply(as.character(1:12),   
  function(x)tmp[x][[x]]$matchup$`0`$teams$`0`$team[[2]]$team_points$total))

my.df <- data.frame(cbind(game=rep(1:length(my.score), 2), 
  team=c(rep("me", length(my.score)), rep("them", length(my.score))),
  score=as.numeric(c(my.score, opp.score))))
my.df$game <- factor(my.df$game, levels=1:13)
my.df$score <- as.numeric(as.character(my.df$score))
  
# create a plot of my game outcomes.
p1 <- ggplot(my.df, aes(x=game, y=score, color=team, group=team)) + 
  geom_point() + geom_line() + scale_y_continuous()
ggsave("FF_regular_season.jpg")

# get a graph for all 10 teams...
get.team.data <- function(team.id){
  team.key <- paste0(league.key, ".t.", team.id)  
  team.matchups.json <- GET(paste0(team.url, team.key, 
    "/matchups?format=json"), sig)
  team.matchups.list <- fromJSON(as.character(team.matchups.json), asText=T)
  team.name <- team.matchups.list$fantasy_content["team"][[1]][[1]][[3]]['name']
  opp.score <- tmp$'0'$matchup$`0`$teams$`1`$team[[2]]$team_points["total"]
  opp.score <- c(opp.score, sapply(as.character(1:12),   
    function(x)tmp[x][[x]]$matchup$`0`$teams$`1`$team[[2]]$team_points$total))
  score <- tmp$'0'$matchup$`0`$teams$`0`$team[[2]]$team_points["total"]
  score <- c(score, sapply(as.character(1:12),   
    function(x)tmp[x][[x]]$matchup$`0`$teams$`0`$team[[2]]$team_points$total))
  df <- data.frame(cbind(game=rep(1:length(score), 2), 
    team=c(rep(as.character(team.name), length(score)), 
      rep("opponent", length(score))),
    score=as.numeric(c(score, opp.score))))
  df$game <- factor(df$game, levels=1:13)
  df$score <- as.numeric(as.character(df$score))
  return(df)
}
  