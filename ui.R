### ui.R

### shiny ui for 2014 FIFA World Cup Goal Data Inspector

### This app is very simple from structural point of view. It
##  1.  uses two select boxes, the second of which is dynamically updated 
##      based on the selection in the first 
##  2.  selections from the select boxes update a histogram, highlighting the data selected
##  3.  the histogram is build using rPlot from the package rCharts. The highighting is achieved by
##  4.  manipulating the color scale and tooltip arguements for the underlying polychart2 engine 
##        used by rPlot.

##  To make this assignment more interesting and to try building a R package, I used a dataset
##  not included in the base R packages. I extracted data from the openfootball database and 
##  built a dataset of goals from the just ended FIFA 2014 World Cup Finals in Brazil and deployed 
##  it to github so that it could be installed by shinyapp.io for my app.

require(rCharts)
library(devtools)
library(wc14goals)    # my dataset. package at https://github.com/mpodell/wc14goals
data(wc14goals)

wc14g <- wc14goals

##########
##########   Lots of data processing to enable building the select box lists.
##########   Skip down to 'END PROCESSING' to see the shinyUI set up.
##########   


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

########## 
##########    END DATA PROCESSING
########## 


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