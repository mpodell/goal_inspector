## server.R

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

library(rCharts)
library(shiny)
library(devtools)
library(wc14goals)   # my dataset. package at https://github.com/mpodell/wc14goals
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

# define the opponent
for (k in 1:nrow(countbymin)){
  if(countbymin$pteam[k] == wc14g$team1[k]){
    countbymin$opp[k] <- wc14g$team2[k]
  }
  else{
    countbymin$opp[k] <- wc14g$team1[k]
  }
}

# define bins for the histogram (5 minute increments)
for (n in seq(5, 125, by = 5)){
  mdf <- countbymin[countbymin[, "minute"] <= n & countbymin[, "minute"] > n - 5, ]
  if (nrow(mdf) > 0){
    for (i in 1:nrow(mdf)){
      countbymin[countbymin[, "minute"] == mdf[i, "minute"], "min"] = n
    }
  }  
}

# note own goals for players
for (j in 1:nrow(countbymin)){
  if(countbymin$owngoal[j] == "t"){
    countbymin$name[j] <- paste(countbymin$name[j], "(o.g.)", sep = " ")
  }
}

# add regions to countbymin. Doing this by hand is cluncky but I could not quickly come up 
# with a way to do it elegantly programmatically.

regionkey <- data.frame( team = c("Mexico", "Costa Rica", "Honduras", "United States",
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
  countbymin$region[i] <- regionkey[regionkey$team == countbymin$pteam[i], "region"]
}


# create region variable for color matching

for (i in 1:nrow(countbymin)){
  if( countbymin[ i, "owngoal"] == "t") {
    countbymin$key[i] <- "Own Goal"
  } else {
    countbymin$key[i] <- countbymin$region[i]
  }
}
# define the count sequence for each goal (the y value in the histogram)
# put the more prolific regions/teams at the bottom of the histogram

tl <- table(countbymin$pteam)
trank <- names(tl)[order(tl, decreasing = TRUE)]
rl <- table(countbymin$region)
rrank <- names(rl)[order(rl, decreasing = TRUE)]

for (n in seq(5, 125, by = 5)){
  mdf <- countbymin[countbymin[, "min"] == n, ]
  t <- trank[trank %in% mdf$pteam]
  r <- rrank[rrank %in% mdf$region]
  mdf<- mdf[order(match(mdf$region, r), match(mdf$pteam, t)),]
  if (nrow(mdf) > 0){
    for (i in 1:nrow(mdf)){
      countbymin[countbymin[, "name"] == mdf[i, "name"] & 
                   countbymin[, "minute"] == mdf[i, "minute"] & 
                   countbymin[, "opp"] == mdf[i, "opp"], "count"] = i 
    }
  }  
}


# Define region color for each region  some options (#FF00A0  #9E0FFF #FF00FF #FF7400 FFA900)

rcolor <- data.frame(region = c("AFC", "CAF", "CONCACAF", "CONMEBOL", "OFC", "UEFA", 
                                "Own Goal", "Other"), 
                     color = c("#FF00FF", "#00FF00","#1A63FF", "#FFD200", "#FF7400", 
                               "#FF0000", "#002B00", "#F2F2F2"), 
                     ogcolor = c("#363600", "#360000", "#361800", "#170124", "#020C24", 
                                 "#002B00","#002B00", "#F2F2F2"),
                     stringsAsFactors = FALSE)

for (i in 1:nrow(countbymin)){
  if(countbymin$owngoal[i] == "t"){
    countbymin$rcolor[i] <- rcolor[rcolor$region == countbymin[i, "region"], "ogcolor"]
  } else{
    countbymin$rcolor[i] <- rcolor[rcolor$region == countbymin[i, "region"], "color"]
  }
}


# create the tooltip labels for each goal: name, pteam score1:score2 opp

for (i in 1:nrow(countbymin)){
  if(countbymin$pteam[i] == countbymin$team1[i]){
    pscore <- countbymin$score1[i]
    oscore <- countbymin$score2[i]
  } else {
    pscore <- countbymin$score2[i]
    oscore <- countbymin$score1[i]
  }
  pc <- sub("\\'", " ", countbymin$pteam[i])
  oc <- sub("\\'", " ", countbymin$opp[i])
  countbymin$goal[i] <- paste(countbymin$name[i], " ",  pc, " ", pscore, "-", oscore, " vs. ", oc, sep = "")
}

########## 
##########    END DATA PROCESSING
########## 

shinyServer(
  function(input, output, session) {
    
    get_region <- reactive({
      if (input$region == " All") {countbymin}
      else (countbymin[countbymin$region == input$region,])      
    })

    observe({
      updateSelectInput(session, "team", choices = sort(c(" All", unique(get_region()$pteam))))
    })
    
    output$ghist <- renderChart({
      
      # check what is selected in the region and team select boxes to identify what should be 
      # highlighted in the histrogram. 
      # sdf are the selected items
      # ndf are the "not-selected" items
      
      if (input$region == " All" & input$team == " All") {sdf <- countbymin} else {
        if (input$region != " All" & (input$team == " All" | input$team == "")) {
          sdf <- countbymin[countbymin$region == input$region, ]
          ndf <- countbymin[countbymin$region != input$region, ]
        } else {
          if ((input$region == " All" | input$region =="") & input$team != " All") {
            if(input$team == "NA") {                                     
              sdf <- countbymin[countbymin$region == input$region, ]     
              ndf <- countbymin[countbymin$region != input$region, ]      
            } else {                                                     
              sdf <- countbymin[countbymin$pteam == input$team, ] 
              ndf <- countbymin[countbymin$pteam != input$team, ]     
            }                                                            
          } else {
              sdf <- countbymin[countbymin$pteam == input$team, ]
              ndf <- countbymin[countbymin$pteam != input$team, ]
          }
        }
      } 
      
      # build color scale for rPlot. This is java script passed through to polycharts.
      # cscale_all for the case where no goals are "un-highlighted", i.e. color = "Other"
      
      cscale_all <- "#! function(value){\n  color_mapping = {'AFC': '#FF00FF', 'CAF': '#00FF00', 'CONCACAF': '#1A63FF', 'CONMEBOL': '#FFD200', 'OFC': '#FF7400',  'UEFA': '#FF0000', 'Own Goal': '#002B00'}
      return color_mapping[value];                  
    } !#"
      
      # cscale_o for the case where some goals are "un-highlighted, i.e. need color = "Other"
      
      cscale_o <- "#! function(value){\n  color_mapping = {'AFC': '#FF00FF', 'CAF': '#00FF00', 'CONCACAF': '#1A63FF', 'CONMEBOL': '#FFD200', 'OFC': '#FF7400',  'UEFA': '#FF0000', 'Own Goal': '#002B00', 'Other': '#F2F2F2'}
      return color_mapping[value];                  
    } !#"
            
      # select the appropriate color scale
      
      if (nrow(sdf) < nrow(countbymin)) {
        ndf$key <- "Other"
        cscale <- cscale_o
        sdf <- rbind(sdf, ndf)
      } else { cscale <- cscale_all}
      
      # build the rPlot
      
      r1 <- rPlot(count ~ min, data = sdf, type = "point", color = "key", 
                  tooltip = "#!function(item){return item.goal}!#")
      r1$guides( color = list ( title = "Color Key", scale = cscale),
                y = list (min = 0, max = 20, title = "Count"), 
                x = list (min = 0, max = 130, title = "Game Time (5 minute intervals)")
      )
      r1$set(title = "Goals by Game Time")
      r1$addParams(dom = 'ghist')
      return(r1)
    })
  }
)