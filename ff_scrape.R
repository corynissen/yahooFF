
library(httr)
library(XML)
library(RJSONIO)
library(ggplot2)

# saved my yahoo keys to a file, now, read them in...
creds <- read.table("~/cn/personal/keys/yahoo.txt", stringsAsFactors=F)
consumer.key <- creds[1,1]
consumer.secret <- creds[2,1]
oauth_endpoints("yahoo")
myapp <- oauth_app("yahoo", key = consumer.key, secret = consumer.secret)
token <- oauth1.0_token(oauth_endpoints("yahoo"), myapp)

# need to get game id for my league...
ff.url <- "http://fantasysports.yahooapis.com/fantasy/v2/game/nfl?format=json"
game.key.json <- GET(ff.url, config(token = token))
game.key.list <- fromJSON(as.character(game.key.json), asText=T)
game.key <- game.key.list$fantasy_content$game[[1]]["game_key"]

# my personal leagueid, you will have to use your own, mine is private
league.id <- "262101"
league.key <- paste0(game.key, ".l.", league.id)
league.url <- "http://fantasysports.yahooapis.com/fantasy/v2/league/"

my.team.id <- "4"
my.team.key <- paste0(league.key, ".t.", my.team.id)
team.url <- "http://fantasysports.yahooapis.com/fantasy/v2/team/"
# lots of endpoints to play with, more here... 
# http://developer.yahoo.com/fantasysports/guide/
my.team.stats.json <- GET(paste0(team.url, my.team.key, "/stats?format=json"), 
                          config(token = token))
my.team.standings.json <- GET(paste0(team.url, my.team.key, 
  "/standings?format=json"), config(token = token))
my.team.matchups.json <- GET(paste0(team.url, my.team.key, 
  "/matchups?format=json"), config(token = token))
my.team.matchups.list <- fromJSON(as.character(my.team.matchups.json), asText=T)

# number of games played
game.num <- 2

# get the opponent scores for my matchups for the entire season
tmp <- my.team.matchups.list$fantasy_content["team"][[1]][[2]]$matchups
opp.score <- tmp$'0'$matchup$`0`$teams$`1`$team[[2]]$team_points["total"]
opp.score <- c(opp.score, sapply(as.character(1:(game.num-1)),   
  function(x)tmp[x][[x]]$matchup$`0`$teams$`1`$team[[2]]$team_points$total))
my.score <- tmp$'0'$matchup$`0`$teams$`0`$team[[2]]$team_points["total"]
my.score <- c(my.score, sapply(as.character(1:(game.num-1)),   
  function(x)tmp[x][[x]]$matchup$`0`$teams$`0`$team[[2]]$team_points$total))

my.df <- data.frame(cbind(game=rep(1:length(my.score), 2), 
  team=c(rep("me", length(my.score)), rep("them", length(my.score))),
  score=as.numeric(c(my.score, opp.score))))
my.df$game <- factor(my.df$game, levels=1:game.num)
my.df$score <- as.numeric(as.character(my.df$score))
  
# create a plot of my game outcomes.
p1 <- ggplot(my.df, aes(x=game, y=score, color=team, group=team)) + 
  geom_point() + geom_line() + scale_y_continuous()
ggsave("FF_regular_season.jpg")

# create blog post
library(markdown)
library(knitr)
knit("YahooFF_blog_post.Rmd")
markdownToHTML("YahooFF_blog_post.md", "YahooFF_blog_post.html", fragment.only = TRUE)
