##### PREDICT THE PROBABILITY OF ANY TENNIS MATCH SET RESULT, BASED ON THE FIXED PROBABILITY OF A PLAYER WINNING ONE POINT #####

#CLICK SOURCE TO RUN SCRIPT WHILE WAITING FOR USER RESPONSES (https://stackoverflow.com/questions/27112370/make-readline-wait-for-input-in-r)

setwd("C:/Users/User/Desktop/Chalkboard ideas and res asst applications") #temp file location before GitHub uplod
rm(list=ls())

##### ENTER DESIRED POINT PROBABILITY, RUNNING THIS LINE ON ITS OWN #####
d <- readline(prompt="Enter the fixed probability of Player A winning a point (any decimal or integer from 0 to 1 inclusive): ")

##### AFTER ENTERING YOUR DESIRED POINT PROBABILITY, PRINT DESIRED POINT PROBABILITY #####
if (is.na(d)) {
  printout <- "Error: please re-enter the probability of Player A winning a point (any decimal or integer from 0 to 1 inclusive, e.g., 0.42, with no preceding or trailing characters or spaces)"
} else if (d>=0 & d<=1) {
  printout <- paste("You have selected a probabiliy of",d)
} else {
  printout<- "Error: please re-enter the probability of Player A winning a point (any decimal or integer from 0 to 1 inclusive, e.g., 0.42, with no preceding or trailing characters or spaces)"}
printout

##### CALCULATE PROBABILITY OF PLAYER B WINIING A POINT #####
if (d>=0 & d<=1) {
  d<-as.numeric(d) 
  } else {
      print("Error: please re-run the script")
    }
m<-1-(d) #Probability of Player B winning a point

##### ENTER DESIRED NUMBER OF GAMES WON AT CONCLUSION OF SET, RUNNING THIS LINE ON ITS OWN #####
n <- readline(prompt="Enter the number of games that Player A wins on conclusion of the set (any integer from 0 to 7 inclusive): ")

if (length(n)==0) {
  printout <- "Error: please re-enter the number of games that Player A wins (any integer from 0 to 7 inclusive, e.g., 2, with no preceding or trailing characters or spaces)"  
} else if (is.na(n)) {
  printout <- "Error: please re-enter the number of games that Player A wins (any integer from 0 to 7 inclusive, e.g., 2, with no preceding or trailing characters or spaces)"
} else if (n!=0 && n!=1 && n!=2 && n!=3 && n!=4 && n!=5 && n!=6 && n!=7) {
  printout <- "Error: please re-enter the number of games that Player A wins (any integer from 0 to 7 inclusive, e.g., 2, with no preceding or trailing characters or spaces)"
} else {
  printout <- "Run the next line of code to enter the number of games that Player B wins"
} 
printout

##### ENTER DESIRED NUMBER OF GAMES WON AT CONCLUSION OF SET, RUNNING THIS LINE ON ITS OWN #####
q <- readline(prompt="Enter the number of games that Player B wins on conclusion of the set (any integer from 0 to 7 inclusive): ")

if (length(q)==0) {
  printout <- "Error: please re-enter the number of games that Player B wins (any integer from 0 to 7 inclusive, e.g., 2, with no preceding or trailing characters or spaces)"  
} else if (is.na(q)) {
  printout <- "Error: please re-enter the number of games that Player B wins (any integer from 0 to 7 inclusive, e.g., 2, with no preceding or trailing characters or spaces)"
} else if (q!=0 && q!=1 && q!=2 && q!=3 && q!=4 && q!=5 && q!=6 && q!=7) {
  printout <- "Error: please re-enter the number of games that Player B wins (any integer from 0 to 7 inclusive, e.g., 2, with no preceding or trailing characters or spaces)"
} else {
  printout <- "Run the next line of code"
} 
printout

sr<-paste(n,"-",q,sep = "")
if (sr=="6-0" | sr=="6-1" | sr=="6-2" | sr=="6-3" | sr=="6-4" | sr=="7-5" | sr=="7-6" | sr=="0-6" | sr=="1-6" | sr=="2-6" | sr=="3-6" | sr=="4-6" | sr=="5-7" | sr=="6-7") {
    print(paste("Your desired set result is: Player A wins",n,"games and Player B wins",q,"games"))
} else {
  print(paste("Error: a game cannot conclude ",sr,". Please re-enter the number of games won by Player A and Player B.",sep = ""))
}

###Probabilities for any single game
P_Wtl <- d^4 #Djok wins game to love
P_Wt15 <- 4*(d^4)*m #Djok wins game to 15
#showing factorials working for one game win outcome only:
#for Djok to win to 15, Djok must win last point - d - and 3 of the previous 4 points - d^3(m)
#to est the number of possible ways in which 3 of the previous 4 points can be won, calculate C(n,r) where r = 4 (the number of previous points) and n =4 (the number of these points won by Djok)
#this shows there are 4 possible ways in which this outcome could be reached. So: 
#d*4d^3*m = 4*(d^4)*m
P_Wt30<-10*(d^4)*(m^2) #Djok wins game to 30
P_deuce<-20*d^3*m^3 #deuce reached
P_WfDeuce<-(d^2)/(1-(2*d*m)) #Djok wins game from deuce
P_DW<-P_Wtl+P_Wt15+P_Wt30+(P_deuce*P_WfDeuce) #probability of Djok winning game
P_MW<-1-P_DW #probability of Murray winning game

###Initial calcs for tie break
P_6_6<-504*(P_DW^6)*(P_MW^6)
P_DwT<-d^7+(7*d^7*m)+(28*d^7*m^2)+(84*d^7*m^3)+(210*d^7*m^4)+(462*d^7*m^5)+(924*d^6*m^6*P_WfDeuce)
P_MwT<-(1-P_DwT)    

###If else chain to generate all poss answers
if (sr == "6-0") {
  answer<-P_DW^6
} else if (sr == "0-6") {
  answer<-P_MW^6  
} else if (sr == "6-1") {
  answer<-6*P_DW^6*P_MW
} else if (sr == "1-6") {
  answer<-6*P_MW^6*P_DW
} else if (sr == "6-2") {
  answer<-21*P_DW^6*P_MW^2
} else if (sr == "2-6") {
  answer<-21*P_MW^6*P_DW^2
} else if (sr == "6-3") {
  answer<-56*P_DW^6*P_MW^3
} else if (sr == "3-6") {
  answer<-56*P_MW^6*P_DW^3
} else if (sr == "6-4") {
  answer<-126*(P_DW^6)*(P_MW^4)
} else if (sr == "4-6") {
  answer<-126*(P_MW^6)*(P_DW^4)
} else if (sr == "7-5") {
  answer<-252*(P_DW^7)*(P_MW^5)
} else if (sr == "5-7") {
  answer<-252*(P_MW^7)*(P_DW^5)
} else if (sr == "7-6") {
  answer<-P_6_6*P_DwT
} else if (sr == "6-7") {
  answer<-P_6_6*P_MwT
} else {print("error")}

print(paste("If the probability that Player A wins each point is ",d,", there is a ",answer," chance that the set concludes with them having won ",n," games and Player B having won ",q," games.",sep = ""))

##### GRAPHIC #####
#graphic shows the likelihood of your desired set result vs other possible set results, based on the selected point winning probability #####

plot_frame<-as.data.frame(c("6-0","6-1","6-2","6-3","6-4","7-5","7-6","6-7","5-7","4-6","3-6","2-6","1-6","0-6"))
colnames(plot_frame)<-"result"

plot_frame$probability <- ifelse(plot_frame$result=="6-0",P_DW^6,
                            ifelse(plot_frame$result=="6-1",6*P_DW^6*P_MW,
                             ifelse(plot_frame$result=="6-2",21*P_DW^6*P_MW^2,
                              ifelse(plot_frame$result=="6-3",56*P_DW^6*P_MW^3,
                                ifelse(plot_frame$result=="6-4",126*(P_DW^6)*(P_MW^4),
                                  ifelse(plot_frame$result=="7-5",252*(P_DW^7)*(P_MW^5),                        
                                   ifelse(plot_frame$result=="7-6",P_6_6*P_DwT,  
                            ifelse(plot_frame$result=="0-6",P_MW^6,
                              ifelse(plot_frame$result=="1-6",6*P_MW^6*P_DW,
                               ifelse(plot_frame$result=="2-6",21*P_MW^6*P_DW^2,      
                                ifelse(plot_frame$result=="3-6",56*P_MW^6*P_DW^3,
                                 ifelse(plot_frame$result=="4-6",126*(P_MW^6)*(P_DW^4),
                                  ifelse(plot_frame$result=="5-7",252*(P_MW^7)*(P_DW^5),
                                   ifelse(plot_frame$result=="6-7",P_6_6*P_MwT,NA
                                                                 ))))))))))))))
plot_frame$selected <- ifelse(plot_frame$result==sr,"selected result","other results")
plot_frame$result <- factor(plot_frame$result, levels = plot_frame$result)

p<-ggplot(data=plot_frame, aes(x=result, y=probability, fill = selected)) + 
  geom_bar(stat="identity")
p + theme_minimal()

#to plot the chances of winning any game
 # EqDW<-function(d){(d^4)+(4*(d^4)*(1-d))+(10*(d^4)*((1-d)^2))+((20*d^3*(1-d)^3)*((d^2)/(1-(2*d*(1-d)))))}
 # gp<-ggplot(data.frame(x=c(0, 1)), aes(x=x)) + 
 #   stat_function(fun=EqDW)
 # gp + theme_minimal()
