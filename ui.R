### ui.R

### shiny ui for 2014 FIFA World Cup Goal Data Inspector
require(rCharts)
library(devtools)
# install_github("wc14goals", "mpodell")
library(wc14goals)
data(wc14goals)

wc14g <- wc14goals

# create a data.frame from which to build a custom goal count by min historgram using rPlot
#
countbymin <- wc14g[, c("date", "name", "minute", "owngoal", "steam", "team1", "team2", "score1", "score2")]

# create player team (pteam) variable
for (l in 1:nrow(countbymin)){
  if(countbymin$owngoal[l] == "t"){
    if(countbymin$steam[l] == countbymin$team1[l]){
      countbymin$pteam[l] <- countbymin$team2[l]
    }
    else{
      countbymin$pteam[l] <- countbymin$team1[l]
    }
  }
  else{
    countbymin$pteam[l] <- countbymin$steam[l]
  }
}

# add regions to countbymin
tcolorkey <- data.frame( team = c("Mexico", "Costa Rica", "Honduras", "United States",
                                  "Argentina", "Brazil", "Chile", "Colombia", "Ecuador", "Uruguay",
                                  "Cameroon", "Côte d'Ivoire", "Nigeria", "Ghana", "Algeria",
                                  "Japan", "Iran", "South Korea",
                                  "Australia",
                                  "Croatia", "Spain", "Netherlands", "Greece", "Italy", "England",
                                  "Switzerland", "France", "Bosnia-Herzegovina", "Germany", "Portugal",
                                  "Belgium", "Russia"),
                         region = c("CONCACAF","CONCACAF","CONCACAF","CONCACAF",
                                    "CONMEBOL","CONMEBOL","CONMEBOL","CONMEBOL","CONMEBOL","CONMEBOL",
                                    "CAF", "CAF", "CAF", "CAF", "CAF",
                                    "AFC", "AFC", "AFC",
                                    "OFC",
                                    "UEFA", "UEFA", "UEFA", "UEFA", "UEFA", "UEFA", "UEFA", "UEFA",
                                    "UEFA", "UEFA", "UEFA", "UEFA", "UEFA"), stringsAsFactors = FALSE)

for (i in 1:nrow(countbymin)){
  countbymin$region[i] <- tcolorkey[tcolorkey$team == countbymin$pteam[i], "region"]
}


shinyUI(pageWithSidebar(
  headerPanel("2014 FIFA World Cup Goal Data Inspector"),
  
  sidebarPanel(
    selectInput("region",
                label = "Select a region or All",
                choices = sort(c(" All", unique(countbymin$region))),
                selected = " All"),
    selectInput("team",
                label = "Select a team or All",
                '',
                selected = " All")

  ),
  mainPanel(
    showOutput("ghist", "polycharts")
  )
))