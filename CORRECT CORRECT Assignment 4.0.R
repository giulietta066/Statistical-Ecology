library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)
read_excel("C://Users/schut/Downloads/Watering Hole Dataset Cleaned Dates.xlsx")
clwathole=read_excel("C://Users/schut/Downloads/Watering Hole Dataset Cleaned Dates.xlsx")
summary(clwathole)
#Rerun after reclass/cleanup
#NoAnimals - Large Max
#DurationMins - Large max-right skewed data
class(clwathole)
str(clwathole)

#RECLASSIFICATIONS --------------------------------------------------------------------
#Species into a character
clwathole$Species<-as.character(clwathole$Species)
#Elephant(1),black rhino(2),white rhino(3)
clwathole$WH<-as.character(clwathole$WH)
#IDs for each WH
clwathole$WHType<-as.character(clwathole$WHType)
#Earth Dam (1), Concrete (2), Pan (3), Reservoir (4), Trough (5)
clwathole$WHSize<-as.character(clwathole$WHSize)
#Small (1), Medium (2), Large (3)
clwathole$Season<-as.character(clwathole$Season)
#Warm/Wet season(1), Cool/Dry season (2)
clwathole$Date<-dmy(clwathole$Date)
clwathole$Period<-as.character(clwathole$Period)
#Daily period - Morning (1), Midday (2), Afternoon (3), Night (4)
clwathole$GroupType<-as.character(clwathole$GroupType)
#Social group type - Bachelor (1), Bachelor Group (2)
#Cow (3),Cow group (4), Cow and calf (5)
#Unknown adult (6),Breeding herd (7), Bull and cow (8)
clwathole$GroundAccess<-as.logical(clwathole$GroundAccess)

#TOA, TOD, LoS Cleanup ----------------------------------------------------------------
#remove false dates from TOA
clwathole$TOAconvert<-as.POSIXct(clwathole$TOA,
                                 format="%Y-%m-%d %H:%M:%S",
                                 tz="UTC",
                                 optional=TRUE)
clwathole$TimeofArrival<-format(clwathole$TOAconvert, "%H:%M:%S")
#TOD
clwathole$TODconvert<-as.POSIXct(clwathole$TOD,
                                 format="%Y-%m-%d %H:%M:%S",
                                 tz="UTC",
                                 optional=TRUE)
clwathole$TimeofDeparture<-format(clwathole$TODconvert, "%H:%M:%S")
#Length of Stay
clwathole$LoSconvert<-as.POSIXct(clwathole$LengthOfStay,
                                 format="%Y-%m-%d %H:%M:%S",
                                 tz="UTC",
                                 optional=TRUE)
clwathole$DurationofStay<-format(clwathole$LoSconvert, "%H:%M:%S")

#Data---------------------------------------------------------------------------------
#Species
clwathole %>%
  group_by(Species) %>%
  summarise(
    n=n(),
    mean_duration=mean(DurationMins,na.rm=TRUE),
    sd_duration=sd(DurationMins,na.rm=TRUE),
    mean_group_size=mean(NoAnimals,na.rm=TRUE),
  )

#Species & Season
clwathole %>%
  group_by(Species,Season) %>%
  summarise(
    n=n(),
    mean_duration=mean(DurationMins, na.rm=TRUE),
    mean_noanimals=mean(NoAnimals, na.rm=TRUE),
    
    .groups = 'drop'
  )

#Species & WHType
clwathole %>%
  group_by(Species,WHType) %>%
  summarise(
    n=n(),
    mean_duration=mean(DurationMins, na.rm=TRUE),
    mean_noanimals=mean(NoAnimals, na.rm=TRUE),
    .groups = 'drop'
  )

#Species & WHT & Season & Period
clwathole %>%
  group_by(Species,WHType,Season,Period) %>%
  summarise(
    n=n(),
    mean_duration=mean(DurationMins, na.rm=TRUE),
    mean_noanimals=mean(NoAnimals, na.rm=TRUE),
    .groups='drop'
  ) %>%
print(n=Inf)

#Questions----------------------------------------------------------------------------
#Q1 - Is there any relationship between duration of visit and number of animals at each species' preferred WHT? Is there any relationship
#between duration of visit and season or period of day?

#Variables - chosen because study established preferred WHT already for each species. Limited variables as each
#WHT has max 3 watering holes= only three different woody heights/densities. Variables with largest range
#= NoAnimals and DurationMins were most interesting to me, as well as
#season and period (water availability may affect tolerance of other individuals)

#Datasets based on WHType
#Elephant
ele_res_data<-clwathole[clwathole$Species=="1"&clwathole$WHType=="4",]
nrow(ele_res_data)
ele_res_data_clean<-ele_res_data%>%
  filter(DurationMins>0)
nrow(ele_res_data_clean)
#Black Rhino
blkrhn_earthdam_data<-clwathole[clwathole$Species=="2"&clwathole$WHType=="1",]
nrow(blkrhn_earthdam_data)
blkrhn_earthdam_data_clean<-blkrhn_earthdam_data%>%
  filter(DurationMins>0)
#Unsupported White Rhino statement in study. Figure out which WHType most popular with provided data
whtrhn_WHT_data1<-clwathole[clwathole$Species=="3"&clwathole$WHType=="1",]
whtrhn_WHT_data2<-clwathole[clwathole$Species=="3"&clwathole$WHType=="2",]
whtrhn_WHT_data3<-clwathole[clwathole$Species=="3"&clwathole$WHType=="3",]
whtrhn_WHT_data4<-clwathole[clwathole$Species=="3"&clwathole$WHType=="4",]
whtrhn_WHT_data5<-clwathole[clwathole$Species=="3"&clwathole$WHType=="5",]
nrow(whtrhn_WHT_data1)
nrow(whtrhn_WHT_data2)
nrow(whtrhn_WHT_data3)
nrow(whtrhn_WHT_data4)
nrow(whtrhn_WHT_data5)
#Greatest White rhino sightings at 4 = Res. 
whtrhn_res_data<-clwathole[clwathole$Species=="3"&clwathole$WHType=="4",]
nrow(whtrhn_res_data)
whtrhn_res_data_clean<-whtrhn_res_data%>%
  filter(DurationMins>0)

#Models-----------------------------------------------------------------------------------
#Elephants - preferred = Reservoir - Naturally dense woody areas
eleQ1mod1<-lm(DurationMins~NoAnimals, data=ele_res_data_clean)
#Number of animals affects duration in minutes at a watering hole
#Baseline model, simple linear model

#See model compare below. Lm returned negative lower bounds = not realistic. Switched to 
#GLM, positive values only. 
eleQ1mod2<-glm(DurationMins~NoAnimals,
               family=Gamma(link="log"),data=ele_res_data_clean)
#Same variables, glm not lm. Number of animals may affect Duration in minutes. 
eleQ1mod3<-glm(DurationMins~NoAnimals + Season,
               family=Gamma(link="log"),data=ele_res_data_clean)
#Number of animals and the season (wet or dry) affect duration in minutes at a watering hole.
eleQ1mod4<-glm(DurationMins~NoAnimals + Season + Period,
               family=Gamma(link="log"),data=ele_res_data_clean)
#Number of animals, the season, and the period of day all affect duration 
#in minutes at watering hole. 
eleQ1mod5<-glm(DurationMins~Season + Period,
               family=Gamma(link="log"),data=ele_res_data_clean)
#Season and period of day affects duration in minutes at watering hole. 
eleQ1mod6<-glm(DurationMins~Period,
               family=Gamma(link="log"),data=ele_res_data_clean)
#Period of day affects duration in minutes at watering hole. 
AIC(eleQ1mod1,eleQ1mod2,eleQ1mod3,eleQ1mod4,eleQ1mod5,eleQ1mod6)

#Same justifications for the following two species. Same variables used,
#in the same order. 

#Black rhinos - preferred = Earth dams - Naturally dense woody areas
blkrhnQ1mod1<-lm(DurationMins~NoAnimals, data=blkrhn_earthdam_data_clean)
blkrhnQ1mod2<-glm(DurationMins~NoAnimals,
                  family=Gamma(link="log"),data=blkrhn_earthdam_data_clean)
blkrhnQ1mod3<-glm(DurationMins~NoAnimals + Season,
                  family=Gamma(link="log"),data=blkrhn_earthdam_data_clean)
blkrhnQ1mod4<-glm(DurationMins~NoAnimals + Season + Period,
                  family=Gamma(link="log"),data=blkrhn_earthdam_data_clean)
blkrhnQ1mod5<-glm(DurationMins~Season + Period,
                  family=Gamma(link="log"),data=blkrhn_earthdam_data_clean)
blkrhnQ1mod6<-glm(DurationMins~Period,
                  family=Gamma(link="log"),data=blkrhn_earthdam_data_clean)
AIC(blkrhnQ1mod1,blkrhnQ1mod2,blkrhnQ1mod3,blkrhnQ1mod4,blkrhnQ1mod5,blkrhnQ1mod6)

#White rhinos - preferred = Reservoir - Naturally dense woody areas
whtrhnQ1mod1<-lm(DurationMins~NoAnimals, data=whtrhn_res_data_clean)
whtrhnQ1mod2<-glm(DurationMins~NoAnimals,
                  family=Gamma(link="log"),data=whtrhn_res_data_clean)
whtrhnQ1mod3<-glm(DurationMins~NoAnimals + Season,
                  family=Gamma(link="log"),data=whtrhn_res_data_clean)
whtrhnQ1mod4<-glm(DurationMins~NoAnimals + Season + Period,
                  family=Gamma(link="log"),data=whtrhn_res_data_clean)
whtrhnQ1mod5<-glm(DurationMins~Season + Period,
                  family=Gamma(link="log"),data=whtrhn_res_data_clean)
whtrhnQ1mod6<-glm(DurationMins~Period,
                  family=Gamma(link="log"),data=whtrhn_res_data_clean)
AIC(whtrhnQ1mod1,whtrhnQ1mod2,whtrhnQ1mod3,whtrhnQ1mod4,whtrhnQ1mod5,whtrhnQ1mod6)

#Model Compare ---------------------------------------------------------------------------
#elephant
predict(eleQ1mod1,interval = "confidence") #negative lower
predict(eleQ1mod1, interval = "prediction") #negative lower
#GLMs - no interval argument, but how can I make use of the data the below lines
#produce?
predict(eleQ1mod2,interval = "confidence")
predict(eleQ1mod2, interval = "prediction")
predict(eleQ1mod3,interval = "confidence")
predict(eleQ1mod3, interval = "prediction")
predict(eleQ1mod4,interval = "confidence")
predict(eleQ1mod4, interval = "prediction")
predict(eleQ1mod5,interval = "confidence")
predict(eleQ1mod5, interval = "prediction")
predict(eleQ1mod6,interval = "confidence")
predict(eleQ1mod6, interval = "prediction")
summary(eleQ1mod2) #|t| higher than 2 and AIC
summary(eleQ1mod3) #lowest AIC, |t| over 2 - best model
summary(eleQ1mod4)
summary(eleQ1mod5)
summary(eleQ1mod6)

#blkrhn
predict(blkrhnQ1mod1,interval="confidence") #no negatives in lower bounds
predict(blkrhnQ1mod1,interval = "prediction") #negative lower
#GLM
predict(blkrhnQ1mod2,interval="confidence")
predict(blkrhnQ1mod2,interval="prediction")
predict(blkrhnQ1mod3,interval="confidence")
predict(blkrhnQ1mod3,interval="prediction")
predict(blkrhnQ1mod4,interval="confidence")
predict(blkrhnQ1mod4,interval="prediction")
predict(blkrhnQ1mod5,interval="confidence")
predict(blkrhnQ1mod5,interval="prediction")
predict(blkrhnQ1mod6,interval="confidence")
predict(blkrhnQ1mod6,interval="prediction")
summary(blkrhnQ1mod2) #lowest AIC, |t| over 2 - best model
summary(blkrhnQ1mod3)
summary(blkrhnQ1mod4)
summary(blkrhnQ1mod5)
summary(blkrhnQ1mod6) 

#whtrhn
predict(whtrhnQ1mod1,interval = "confidence") #negative lower
predict(whtrhnQ1mod1,interval="prediction") #negative lower
#GLM
predict(whtrhnQ1mod2,interval="confidence")
predict(whtrhnQ1mod2,interval="prediction")
predict(whtrhnQ1mod3,interval = "confidence")
predict(whtrhnQ1mod3,interval="prediction")
predict(whtrhnQ1mod4,interval="confidence")
predict(whtrhnQ1mod4,interval="prediction")
predict(whtrhnQ1mod5,interval = "confidence")
predict(whtrhnQ1mod5,interval="prediction")
predict(whtrhnQ1mod6,interval="confidence")
predict(whtrhnQ1mod6,interval="prediction")
summary(whtrhnQ1mod2)
summary(whtrhnQ1mod3)
summary(whtrhnQ1mod4) #lowest AIC, |t| over 2 - best model
summary(whtrhnQ1mod5)
summary(whtrhnQ1mod6)

