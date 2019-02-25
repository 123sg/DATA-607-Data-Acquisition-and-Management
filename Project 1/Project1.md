Data 607 - Project 1
================
SG

Instructions:
-------------

In this project, you’re given a text file with chess tournament results where the information has some structure. Your job is to create an R Markdown file that generates a .CSV file (that could for example be imported into a SQL database) with the following information for all of the players:
Player’s Name, Player’s State, Total Number of Points, Player’s Pre-Rating, and Average Pre Chess Rating of Opponents
For the first player, the information would be:
Gary Hua, ON, 6.0, 1794, 1605
1605 was calculated by using the pre-tournament opponents’ ratings of 1436, 1563, 1600, 1610, 1649, 1663, 1716, and dividing by the total number of games played. If you have questions about the meaning of the data or the results, please post them on the discussion forum. Data science, like chess, is a game of back and forth…
The chess rating system (invented by a Minnesota statistician named Arpad Elo) has been used in many other contexts, including assessing relative strength of employment candidates by human resource departments.
You may substitute another text file (or set of text files, or data scraped from web pages) of similar or greater complexity, and create your own assignment and solution. You may work in a small team. All of your code should be in an R markdown file (and published to rpubs.com); with your data accessible for the person running the script.

Cleaning up data on chess tournament results
--------------------------------------------

``` r
library(stringr)
library(DT)

## Skip the headers, and get the data
fileData <- read.csv(file="//Users/suma/Desktop/CUNY SPS - Masters Data Science/Data 607/Project 1/tournamentinfo.txt", skip = 3, header = F)

## Remove the dashed lines from the data
delimitedData <- str_split(fileData[,], "-", simplify=TRUE)

## Get the Player Names. Apply regex where there is at least a first and last name
PlayerNames <- unlist(str_extract_all(delimitedData[,], "\\w+[[:space:]]\\w+([[:space:]]\\w+)*", simplify=TRUE))
PlayerNames <- PlayerNames[!PlayerNames[,] == "",]

## Get the Player States. Apply regex where there are two capital letters followed by a space and |. Remove blank rows from the data
PlayerStates <- unlist(str_extract_all(delimitedData[,],"[A-Z][A-Z][[:space:]][\\|]"))
PlayerStates <- str_split(PlayerStates, "[[:space:]][\\|]", simplify=TRUE)
PlayerStates <- PlayerStates[, -2]

## Get the Total Number of Points. Apply regex that gets decimal numbers. Remove blank rows from the data
TotalPoints <- unlist(str_extract_all(delimitedData[,], "(\\d+)[.](\\d+)", simplify=TRUE))
TotalPoints <- TotalPoints[!TotalPoints[,] == "",]

## Get the Pre-Ratings. Apply regex that gets numbers after R: and before any number of space. Remove blank rows from the data
PreRatings <- unlist(str_extract_all(delimitedData[,], "[R:]([[:space:]]+)([[:alnum:]]+)([[:space:]]*)", simplify=TRUE))
PreRatings <- unlist(str_extract_all(PreRatings, "\\d+[[:alnum:]]+", simplify=TRUE))
PreRatings <- unlist(str_extract_all(PreRatings, "\\d\\d\\d+", simplify=TRUE))
PreRatings <- PreRatings[!PreRatings[,] == "",]
PreRatings <- as.numeric(PreRatings)

## Get the opponent strings. Apply regex where there is a | followed by a letter, some space, a number, a |
OpponentData <- unlist(str_extract_all(delimitedData[,], "([\\|][A-Z]([[:space:]]+)\\d*[\\|])([A-Z]([[:space:]]+)\\d*[\\|])*", simplify=TRUE))
Opponents <- matrix(ncol=7)

## Get the individual Opponent Indexes into a matrix of 7 columns. Remove any blank rows from the data
Opponents <- unlist(str_extract_all(OpponentData[,], "\\d+", simplify=TRUE))
Opponents <- Opponents[rowSums(Opponents=="")!=ncol(Opponents), ]

##Instantiate Rating Averages 
RatingAverages = NULL

##Loop through each row of Opponent Index. Match each Opponent Index with its corresponding Pre-Rating. Get the average Opponent rating for each row
for(row in 1:nrow(Opponents)){
  numberOfOpponents = 0
  sum = 0
  
  for(col in 1:ncol(Opponents)){
    
    if(Opponents[row, col] != ""){ # Check to make sure we are not looking at a null opponent index value
      index <- Opponents[row, col] # Get the Opponent Index
      index <- strtoi(index, base=0L) # Convert to integer
      sum = sum + strtoi(PreRatings[index]) # Update sum of corresponding pre-ratings
      numberOfOpponents = numberOfOpponents + 1 # Update number of opponents
    }
  }
  
  avg = sum/numberOfOpponents
  RatingAverages = rbind(RatingAverages, data.frame(avg))
}

## Save all data into TournamentResults dataframe
TournamentResults <- data.frame(PlayerNames, PlayerStates, TotalPoints, PreRatings, RatingAverages)
colnames(TournamentResults) <- c("Player Name","Player State", "Total Number of Points", "Player's Pre-Rating", "Average Pre Chess Rating of Opponents")

write.csv(TournamentResults,'//Users/suma/Desktop/CUNY SPS - Masters Data Science/Data 607/Project 1/Results.csv', TRUE)
```

    ## Warning in write.table(TournamentResults, "//Users/suma/Desktop/CUNY SPS
    ## - Masters Data Science/Data 607/Project 1/Results.csv", : appending column
    ## names to file

Cleaned up data that can go to .CSV file
----------------------------------------

``` r
  #tournamentResults
datatable(TournamentResults)
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-28516915ee276d4390a8">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64"],["GARY HUA","DAKSHESH DARURI","ADITYA BAJAJ","PATRICK H SCHILLING","HANSHI ZUO","HANSEN SONG","GARY DEE SWATHELL","EZEKIEL HOUGHTON","STEFANO LEE","ANVIT RAO","CAMERON WILLIAM MC LEMAN","KENNETH J TACK","TORRANCE HENRY JR","BRADLEY SHAW","ZACHARY JAMES HOUGHTON","MIKE NIKITIN","RONALD GRZEGORCZYK","DAVID SUNDEEN","DIPANKAR ROY","JASON ZHENG","DINH DANG BUI","EUGENE L MCCLURE","ALAN BUI","MICHAEL R ALDRICH","LOREN SCHWIEBERT","MAX ZHU","GAURAV GIDWANI","SOFIA ADINA STANESCU","CHIEDOZIE OKORIE","GEORGE AVERY JONES","RISHI SHETTY","JOSHUA PHILIP MATHEWS","JADE GE","MICHAEL JEFFERY THOMAS","JOSHUA DAVID LEE","SIDDHARTH JHA","AMIYATOSH PWNANANDAM","BRIAN LIU","JOEL R HENDON","FOREST ZHANG","KYLE WILLIAM MURPHY","JARED GE","ROBERT GLEN VASEY","JUSTIN D SCHILLING","DEREK YAN","JACOB ALEXANDER LAVALLEY","ERIC WRIGHT","DANIEL KHAIN","MICHAEL J MARTIN","SHIVAM JHA","TEJAS AYYAGARI","ETHAN GUO","JOSE C YBARRA","LARRY HODGE","ALEX KONG","MARISA RICCI","MICHAEL LU","VIRAJ MOHILE","SEAN M MC CORMICK","JULIA SHEN","JEZZEL FARKAS","ASHWIN BALAJI","THOMAS JOSEPH HOSMER","BEN LI"],["ON","MI","MI","MI","MI","OH","MI","MI","ON","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","ON","MI","ON","MI","MI","ON","MI","MI","MI","ON","MI","ON","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","MI","ON","MI","MI","MI"],["6.0","6.0","6.0","5.5","5.5","5.0","5.0","5.0","5.0","5.0","4.5","4.5","4.5","4.5","4.5","4.0","4.0","4.0","4.0","4.0","4.0","4.0","4.0","4.0","3.5","3.5","3.5","3.5","3.5","3.5","3.5","3.5","3.5","3.5","3.5","3.5","3.0","3.0","3.0","3.0","3.0","3.0","3.0","3.0","3.0","2.5","2.5","2.5","2.5","2.5","2.5","2.0","2.0","2.0","2.0","2.0","2.0","2.0","1.5","1.5","1.0","1.0","1.0","3.5"],[1794,1553,1384,1716,1655,1686,1649,1641,1411,1365,1712,1663,1666,1610,1220,1604,1629,1600,1564,1595,1563,1555,1363,1229,1745,1579,1552,1507,1602,1522,1494,1441,1449,1399,1438,1355,980,1423,1436,1348,1403,1332,1283,1199,1242,377,1362,1382,1291,1056,1011,935,1393,1270,1186,1153,1092,917,853,967,955,1530,1175,1163],[1605.28571428571,1469.28571428571,1563.57142857143,1573.57142857143,1500.85714285714,1518.71428571429,1372.14285714286,1468.42857142857,1523.14285714286,1554.14285714286,1467.57142857143,1506.16666666667,1497.85714285714,1515,1483.85714285714,1385.8,1498.57142857143,1480,1426.28571428571,1410.85714285714,1470.42857142857,1300.33333333333,1213.85714285714,1357,1363.28571428571,1506.85714285714,1221.66666666667,1313.5,1144.14285714286,1259.85714285714,1378.71428571429,1276.85714285714,1375.28571428571,1149.71428571429,1388.16666666667,1384.8,1539.16666666667,1429.57142857143,1390.57142857143,1248.5,1149.85714285714,1106.57142857143,1327,1152,1357.71428571429,1392,1355.8,1285.8,1296,1356.14285714286,1494.57142857143,1345.33333333333,1206.16666666667,1406,1414.4,1363,1391,1319,1330.2,1327.28571428571,1186,1350.2,1263,1522.14285714286]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Player Name<\/th>\n      <th>Player State<\/th>\n      <th>Total Number of Points<\/th>\n      <th>Player's Pre-Rating<\/th>\n      <th>Average Pre Chess Rating of Opponents<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
Some statistics
---------------

**Average Player Pre-Rating for each Player State**

``` r
StateAvgPreRatings <- data.frame(aggregate(TournamentResults[,4], list(TournamentResults$`Player State`), mean))
colnames(StateAvgPreRatings) <- c("Player State", "Average Player's Pre-Rating")
StateAvgPreRatings
```

    ##   Player State Average Player's Pre-Rating
    ## 1           MI                      1362.0
    ## 2           OH                      1686.0
    ## 3           ON                      1453.5
