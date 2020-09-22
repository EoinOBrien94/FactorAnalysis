library(dplyr)

# Import data from 19/20 season from fbref

#Overall stats
BigFiveStandard <- read.csv('OverallStats.txt')

BigFiveStandard <-select(BigFiveStandard,"Player","Squad","Pos",
                         "Comp","Min","Glsp90","Astp90",
                         "npxGp90","xAp90","Age")


#Creation stats
BigFiveCreation <- read.csv('Creation.txt')


BigFiveCreation <-select(BigFiveCreation,"Player","Squad","SCA90",
                         "ShotPassLive","ShotPassDead","ShotDrib",
                         "ShotSh","ShotFoul","GCA90","GoalPassLive",
                         "GoalPassDead","GoalDrib","GoalSh",
                         "GoalFoul")

#Defence stats
BigFiveDefence <- read.csv('Defence.txt')

BigFiveDefence <-select(BigFiveDefence,"Player","Squad",
                        "Tklp90",
                        "Pressp90","SuccPressp90","Def3rdPresp90",
                        "Mid3rdPresp90","Att3rdPresp90",
                        "Def3rdTklp90","Mid3rdTklp90",
                         "Att3rdTklp90","Tkl.","DribbledPastp90",
                         "SuccPressp90","Def3rdPresp90","Mid3rdPresp90",
                         "Att3rdPresp90","Intp90","Clrp90",
                         "Errp90")

#Misc Stats
BigFiveMisc <- read.csv('Misc.txt')

BigFiveMisc <-select(BigFiveMisc,"Player","Squad",
                        "FlsCommittedp90","FlsDrawnp90",
                        "Offp90","Crsp90","RecovBalls",
                        "WonAerialp90","LostAerialp90")


#Passing Stats
BigFivePasses <- read.csv('Passing.txt')

BigFivePasses <- select(BigFivePasses,"Player","Squad",
                     "PassTotDistp90","PassPrgDistp90",
                     "CmpShortp90","AttShortp90","CmpMediump90",
                     "AttMediump90","CmpTotalp90","AttTotalp90",
                     "CmpLongp90","AttLongp90","KPp90","PassIntoFinalThirdp90",
                     "PPAp90","ProgPassp90")

#Passing Stats
BigFivePossession <- read.csv('Possession.txt')

BigFivePossession <-select(BigFivePossession,"Player","Squad",
                       "Touchesp90","DefPenTouchesp90",
                       "Def3rdTouchesp90","Mid3rdTouchesp90","Att3rdTouchesp90",
                       "AttPenTouchesp90","LiveTouches","SuccDribblesp90","AttDribblesp90",
                       "Carriesp90","TotDistCarriedp90","PrgDistCarriedp90",
                       "RecPassp90","Dispossessedp90")

#Shots Stats
BigFiveShots <- read.csv('Shooting.txt')

BigFiveShots <-select(BigFiveShots,"Player","Squad",
                           "Sh.90","SoT.90")

#Pass Type Stats
BigFivePassType <- read.csv('PassTypes.txt')

BigFivePassType <-select(BigFivePassType,"Player","Squad",
                      "ThroughBallp90","PassUnderPress",
                      "Switches","GroundPasses","LowPasses",
                      "HighPasses","PassesInt")


#Join DFs
Overall1920<-merge(x = BigFiveStandard, y = BigFiveCreation, by = c("Player","Squad"), all.x = TRUE)
Overall1920<-Overall1920[complete.cases(Overall1920),]

Overall1920<-merge(x = Overall1920, y = BigFiveDefence, by = c("Player","Squad"), all.x = TRUE)
Overall1920<-Overall1920[complete.cases(Overall1920),]

Overall1920<-merge(x = Overall1920, y = BigFiveMisc, by = c("Player","Squad"), all.x = TRUE)
Overall1920<-Overall1920[complete.cases(Overall1920),]

Overall1920<-merge(x = Overall1920, y = BigFivePasses, by = c("Player","Squad"), all.x = TRUE)
Overall1920<-Overall1920[complete.cases(Overall1920),]

Overall1920<-merge(x = Overall1920, y = BigFivePossession, by = c("Player","Squad"), all.x = TRUE)
Overall1920<-Overall1920[complete.cases(Overall1920),]

Overall1920<-merge(x = Overall1920, y = BigFiveShots, by = c("Player","Squad"), all.x = TRUE)
Overall1920<-Overall1920[complete.cases(Overall1920),]

Overall1920<-merge(x = Overall1920, y = BigFivePassType, by = c("Player","Squad"), all.x = TRUE)
Overall1920<-Overall1920[complete.cases(Overall1920),]

#Create clean version of player name
Overall1920['Name']<-sub("\\\\.*", "", Overall1920$Player)

#Convert recovered balls to per 90
Overall1920$RecovBallsp90<-Overall1920$RecovBalls/(Overall1920$Min/90)

#Filter on players >=500 mins played
Overall1920<-filter(Overall1920,Min>=500)

#Import data from squads to scale some statistics

#Squad possession and touches data
Possession <- read.csv('SquadsTouches.txt')
Possession <- select(Possession,"Squad","Comp","Poss","Touches")

#Squad attempted passes data
Passes <- read.csv('SquadsPasses.txt')
Passes <- select(Passes,"Squad","Comp","AttSquad")

#Squad minutes played data
Mins <- read.csv('SquadsMins.txt')
Mins <- select(Mins,"Squad","Comp","SquadMin")

#Merge squad data into player data
Overall1920<-merge(x = Overall1920, y = Possession, by = c("Squad","Comp"), all.x = TRUE)
Overall1920<-Overall1920[complete.cases(Overall1920),]

Overall1920<-merge(x = Overall1920, y = Passes, by = c("Squad","Comp"), all.x = TRUE)
Overall1920<-Overall1920[complete.cases(Overall1920),]

Overall1920<-merge(x = Overall1920, y = Mins, by = c("Squad","Comp"), all.x = TRUE)
Overall1920<-Overall1920[complete.cases(Overall1920),]

## FACTOR ANALYSIS ON MIDFIELDERS

Midfielders<-filter(Overall1920,Pos=="MF"|Pos=="MFDF"|Pos=="MFFW")

#Select desired stats
Midfielders <- select(Midfielders,"Name","Squad","Pos",
                      "Comp","Min","Touchesp90","DefPenTouchesp90",
                      "Def3rdTouchesp90","Mid3rdTouchesp90",
                      "Att3rdTouchesp90","AttPenTouchesp90",
                      "AttTotalp90","CmpTotalp90",
                      "ThroughBallp90","PassUnderPress",
                      "Switches","GroundPasses","LowPasses",
                      "HighPasses","PassTotDistp90","PassPrgDistp90",
                      "AttShortp90","AttMediump90","AttLongp90","KPp90",
                      "PassIntoFinalThirdp90","PPAp90",
                      "Carriesp90","TotDistCarriedp90","PrgDistCarriedp90",
                      "Tklp90","Pressp90","Intp90",
                      "RecovBallsp90","Sh.90","Poss","Touches","AttSquad",
                      "SquadMin","Def3rdPresp90",
                      "Mid3rdPresp90","Att3rdPresp90",
                      "Crsp90","Age","Comp")



#Scale area Touches by proportion of overall touches
Midfielders[,7:11] <- Midfielders[,7:11] / Midfielders$Touchesp90

#Create Pass completion
Midfielders$PassCompletion <- Midfielders$CmpTotalp90 / Midfielders$AttTotalp90

#Scale passes by proportion of overall passes to get pass preferences
Midfielders[,14:19] <- Midfielders[,14:19] / Midfielders$AttTotalp90

#Get proportion of distance passed forward 
Midfielders$ProgressivePassProportion <- Midfielders$PassPrgDistp90 / Midfielders$PassTotDistp90

#Scale passes by proportion of overall passes to get pass preferences
Midfielders[,22:27] <- Midfielders[,22:27] / Midfielders$AttTotalp90

#Scale carries by touches taken
Midfielders$Carriesp90 <- Midfielders$Carriesp90 / Midfielders$Touchesp90

#Get proportion of carries forward 
Midfielders$ProgressiveCarryProportion <- Midfielders$PrgDistCarriedp90 / Midfielders$TotDistCarriedp90

#Scale defensive actions by possession
Midfielders[,31:34] <- Midfielders[,31:34] * (Midfielders$Poss/50)


#Scale shots by touches taken
Midfielders$Sh.90 <- Midfielders$Sh.90 / Midfielders$Touchesp90


#Get proportion of touches taken by player of team
Midfielders$ProportionOfTeamTouches <- Midfielders$Touchesp90 / (Midfielders$Touches/(Midfielders$SquadMin/90))

#Get proportion of passes taken by player of team
Midfielders$ProportionOfTeamPasses <- Midfielders$AttTotalp90 / (Midfielders$AttSquad /(Midfielders$SquadMin/90))


#Scale defensive pressing actions by possession
Midfielders[,40:42] <- Midfielders[,40:42] * (Midfielders$Poss/50)

#Scale crosses by touches taken
Midfielders$Crsp90 <- Midfielders$Crsp90 / Midfielders$Touchesp90


Midfielders<-select(Midfielders,"Name","Pos","Age","Comp",
                    "ProportionOfTeamTouches",
       "Def3rdTouchesp90","Mid3rdTouchesp90",
       "Att3rdTouchesp90",
       "ProgressivePassProportion",
       "ThroughBallp90","PassUnderPress",
       "Switches","GroundPasses","LowPasses",
       "HighPasses","AttShortp90",
       "AttMediump90","AttLongp90","KPp90",
       "PassIntoFinalThirdp90","PPAp90",
       "Carriesp90","ProgressiveCarryProportion",
       "Tklp90","Intp90",
       "Sh.90","Def3rdPresp90",
       "Mid3rdPresp90","Att3rdPresp90",
       "Crsp90")


# Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(Midfielders[,-c(1,2,3,4)])) # get eigenvalues

ap <- parallel(subject=nrow(Midfielders[,-c(1,2,3,4)]),var=ncol(Midfielders[,-c(1,2,3,4)]),
               rep=100,cent=.05)

nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)

#Look at number of factors
plotnScree(nS)

#Perform factor analysis
res = factanal(Midfielders[,-c(1,2,3,4)],5,scores = "regression")

#View results
res

#Look at loadings
loadings(res,sort=TRUE)

#Look at uniqueness
res$uniquenesses


#Extract factor scores 
MidScores<-cbind.data.frame(Name = Midfielders[,1],res$scores,Position = Midfielders[,2],
                            Age = Midfielders[,3],League = Midfielders$Comp)

#Rename leagues
MidScores$League <- factor(MidScores$League,as.character(unique(MidScores$League)),
                           c("La Liga","Ligue 1","Premier League",
                             "Serie A","Bundesliga"))


#Check example of similar players to Ruben Neves
Similarity <- 1-(rowSums(abs(sweep(MidScores[,2:6],2,unlist(MidScores[MidScores["Name"]=="Rúben Neves",2:6]))))/
                     max(rowSums(abs(sweep(MidScores[,2:6],2,unlist(MidScores[MidScores["Name"]=="Rúben Neves",2:6]))))))


SimilarityResult <- cbind.data.frame(Name = MidScores[,1],Similarity)

#Order results
SimilarityResult<-SimilarityResult[with(SimilarityResult, order(-Similarity)), ]

#Look at most similar players
SimilarityResult[1:10,]
