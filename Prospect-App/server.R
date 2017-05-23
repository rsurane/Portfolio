

# setwd("D:/My Work/Other/RTA")
# install.packages("shiny")
library(shiny)
library(ggplot2)

## ---------------------------------------- ##
## ---------------------------------------- ##


# Enter Prospect Weights (Weights must equal 100%)
# Or enter in value from 0 - 100 for each skill category
# speed.wt <- 70
# throw.wt <- 30
# hit.wt <- 20
# field.wt <- 0

## ---------------------------------------- ##
## ---------------------------------------- ##

# if (sum(speed.wt,throw.wt,hit.wt)!=100) {
#   print("Please enter weights that add to 100%")   
# } else { #continue the script


prospect <- function(df,speed.wt,throw.wt,hit.wt,field.wt) {
  # Import Data
  
  # df <- read.csv(file)
  
  # Clean Data
  df <- df[complete.cases(df),]
  df <- df[-which(df$Primary.position=="-"),]
  df <- df[-which(df$Players.age=="-"),]
  df <- df[-which(df$Grad.year=="-"),]
  
  df$Players.age <- as.numeric(gsub(" years","",df$Players.age))
  
  # rbind(1:length(names(df)),names(df))
  # Select important columns (Names, Emails, Idex, Scores)
  scores <- df[,c(1,2,9,12:18)]
  
  # Standardize score data
  zscores <- cbind(as.character(scores[,1]),apply(scores[,4:10],2, scale))
  
  # Pull three factors to match players (Speed index, Throw index, Hitting index)
  # Right now just using 10-yd, overhand, and exit speed for simplicity until the indices are created
  ranking.dat <- as.data.frame(zscores[,c(1,2,5,7,8)])
  colnames(ranking.dat) <- c("Name","Speed","Throwing","Hitting","Fielding")
  
  # Convert data from factors to numeric
  ranking.dat$Speed <- as.numeric(as.character(ranking.dat$Speed))
  ranking.dat$Throwing <- as.numeric(as.character(ranking.dat$Throwing))
  ranking.dat$Hitting <- as.numeric(as.character(ranking.dat$Hitting))
  ranking.dat$Fielding <- as.numeric(as.character(ranking.dat$Fielding))
  
  speed <- ranking.dat$Speed
  throw <- ranking.dat$Throwing
  hit <- ranking.dat$Hitting
  field <- ranking.dat$Fielding
  
  # Get score percentiles
  pcts <- c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)
  # pcts <- c(0, .2, .4, .6, .8)
  speed.pct <- quantile(speed, pcts) 
  throw.pct <- quantile(throw, pcts) 
  hit.pct <- quantile(hit, pcts) 
  field.pct <- quantile(field, pcts) 
  
  # speed.pct
  # throw.pct
  # hit.pct
  
  # # Enter Prospect Weights (Weights must equal 100%)
  # speed.wt <- 70
  # throw.wt <- 20
  # hit.wt <- 10
  # # Fielding
  
  # Combine percentile vectors
  weights <- cbind(speed.wt, throw.wt, hit.wt,field.wt)
  colnames(weights) <- c("Speed","Throwing","Hitting","Fielding")
  
  # Pull specific zscore for selected percentile
  sp <- speed.pct[names(speed.pct)==paste0(speed.wt,"%")]
  th <- throw.pct[names(throw.pct)==paste0(throw.wt,"%")]
  ht <- hit.pct[names(hit.pct)==paste0(hit.wt,"%")]
  fd <- field.pct[names(field.pct)==paste0(field.wt,"%")]
  
  # Create Data Frame of Players that Meet Criteria
  selectedplayers <- ranking.dat[which(ranking.dat$Speed>sp & ranking.dat$Throwing>th & ranking.dat$Hitting>ht & ranking.dat$Fielding>fd),]
  # selectedplayers
  
  # Determine Importance of Skills
  ndx <- order(-weights)[1:length(weights)]
  
  # Organize the Skill According to Importance (wt1 - highest weight, wt2 - second highest, etc.)
  wt1 <- colnames(weights)[ndx[1]]
  wt2 <- colnames(weights)[ndx[2]]
  wt3 <- colnames(weights)[ndx[3]]
  wt4 <- colnames(weights)[ndx[4]]
  
  # Show the top 10 selected players
  prospects <- head(selectedplayers[with(selectedplayers, order(-selectedplayers[wt1], -selectedplayers[wt2],-selectedplayers[wt3],-selectedplayers[wt4])), ], n = 10L)
  
  return(prospects)
}



# speed.wt <- 70
# throw.wt <- 30
# hit.wt <- 20
# field.wt <- 0
# prospect(speed.wt,throw.wt,hit.wt,field.wt)

shinyServer(function(input, output) {

  
  prospects <- eventReactive(input$EnterButton, {
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath)
    
    speed.wt <- as.numeric(input$Speed)
    throw.wt <- as.numeric(input$Throwing)
    hit.wt <- as.numeric(input$Hitting)
    field.wt <- as.numeric(input$Fielding)
    
    prospects <- prospect(df,speed.wt,throw.wt,hit.wt,field.wt)

    return(prospects)
    
  })
  
  output$outputId <- renderTable({
    prospects()
  })
  
  
})


