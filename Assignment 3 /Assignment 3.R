library(emdbook)
library(ggplot2)
library(readr)
library(tidyverse)
library(bbmle)


# 1a ----------------------------------------------------------------------

# This model is focused on explaining how often papers are cited as a function of different factors.
# r_scripts_available is a binary variable, where papers either share their code (1) or don't (0)
# age_y is the age, in years, of a publication
# open access is a binary variable that tells whether the paper is free to access (1) or not (0)


#load and reformat the data

citation_data <- readr::read_rds("https://github.com/bmaitner/R_citations/raw/refs/heads/main/data/cite_data.RDS") %>%
  mutate(age_y = 2022-year) %>%
  mutate(r_scripts_available = case_when(r_scripts_available == "yes" ~ 1,
                                         r_scripts_available == "no" ~ 0)) %>%
  mutate(citations = as.numeric(citations),
         open_access = as.numeric(open_access)) %>%
  ungroup()%>%
  select(r_scripts_available,citations,open_access,age_y) %>%
  na.omit()

# plot if you like 

citation_data %>%
  ggplot(mapping = aes(x = age_y,y = citations))+
  geom_point()

#Original model
#a=strength of age effect
#b=age
#c=open access
#d=rscripts available
cites.fit <- mle2(citations ~ dpois(lambda = a*age_y^b +
                                      int+
                                      rsa*r_scripts_available^d +
                                      oa*open_access^c),
                  start = list(a=0.17,
                               int=1,
                               rsa=0.1,
                               oa=0.1,
                               b=1,
                               c=1,
                               d=1),
                  data = citation_data)
#Alt. 1 - Age only. 
cites.age <- mle2(citations ~ dpois(lambda = a*age_y^b + int),
                  start = list(a=0.17,
                               int=1,
                               b=1),
                  data = citation_data)
#Alt. 2 - Age with R scripts, no open access
cites.age.rsa <- mle2(citations ~ dpois(lambda = a*age_y^b +
                                      int+
                                      rsa*r_scripts_available^d),
                  start = list(a=0.17,
                               int=1,
                               rsa=0.1,
                               b=1,
                               d=1),
                  data = citation_data)
#Alt. 3 - Age and OA, no R script
cites.age.oa <- mle2(citations ~ dpois(lambda = a*age_y^b +
                                      int+
                                      oa*open_access^c),
                  start = list(a=0.17,
                               int=1,
                               oa=0.1,
                               b=1,
                               c=1),
                  data = citation_data)
#Alt.4- Null/Intercept only  GIULIETTA FIX THIS!!!!!!!!!!
cites.null<-mle2(citations~dpois(lamba=int),
                 start=list(int=1),
                 data=citation_data)

# AIC initial
# dAIC - delta AIC - difference from the best model, 0 is best model.
# df - degrees of freedom: number of parameters
AIC(cites.fit,cites.age,cites.age.rsa,cites.age.oa,cites.null)
AICtab(cites.fit,cites.age,cites.age.rsa,cites.age.oa)
coef(cites.fit)

#ANSWER 1a
#Based off the dAICs, cites.fit is the best model (0). Out of the three
#alternative models, cites.age.rsa is the best model (90.7) although
#if the following is correct:
#General AIC Rules of Thumb:
#dAIC < 2: Models are essentially equivalent
#dAIC 2-7: Some support for the better model
#dAIC 7-10: Considerably less support
#dAIC > 10: Virtually no support for the worse model
#cites.age.rsa may be the best model within the context of 
#alternative models created, it is still not a model that should be used. 

# 1b ----------------------------------------------------------------------

# This model is focused on what determines rates of R code sharing by authors.
# r_scripts_available is a binary variable, where papers either share their code (1) or don't (0)
# year is the year of publication (relative to 2010).
# open_access is a binary variable that tells whether the paper is free to access (1) or not (0)
# data_available is a binary variable that tells whether the data are publicly available (1) or not (0)


code_data <- readr::read_rds("https://github.com/bmaitner/R_citations/raw/refs/heads/main/data/cite_data.RDS") %>%
  mutate(r_scripts_available = case_when(r_scripts_available == "yes" ~ 1,
                                         r_scripts_available == "no" ~ 0)) %>%
  mutate(data_available = case_when(data_available == "yes" ~ 1,
                                    data_available == "no" ~ 0)) %>%
  
  mutate(citations = as.numeric(citations),
         open_access = as.numeric(open_access)) %>%
  mutate(year = year-2010)

#added lines of code to set up for Alt. 2 
head(code_data$citations)
code_data<-code_data%>%
log_citations<-log(code_data$citations)
  mutate(log_citations=log(citations+1))%>%

#note that for this function I use a logistic transform to 
#ensure the probability stays between 0 and 1 during optimization

#Original
#y=year coefficient
#b=year exponent
#d=data availability coefficient
#e=data availability exponent
#o=open access coefficient
#p=open access exponent
sharing.fit <- mle2(
  r_scripts_available ~ dbinom(size = 1,
                               prob = plogis(int +
                                               y * year^b +
                                               d * data_available^e +
                                               o * open_access^p
                               )),
  start = list(int = 0,
               y = 0,
               b = 1,
               d = 0,
               e = 1,
               o = 0,
               p = 1),
  data = code_data
)
#Alt. 1 - Year and data availability
sharing.year.da <- mle2(
  r_scripts_available ~ dbinom(size = 1,
                               prob = plogis(int +
                                               y * year^b +
                                               d * data_available^e
                               )),
  start = list(int = 0,
               y = 0,
               b = 1,
               d = 0,
               e = 1),
  data = code_data
)
#Alt. 2 
#Original with number of citations included - DOES NOT WORK
#Not one of my final answers but I would like to know what I am doing wrong.
#What I intended to do - ensure citations was an object, set it as the log of 
#citations + 1. Add that into my model to see if the year, data available or open
#access had any effect on number of citations recorded. I keep getting various 
#error messages, I suspect maybe its something to do with the type of data "citations"
#is (range larger than "year", not 1 or 0). Removed power terms to maybe help? Did not. :(
sharing.fit.citations <- mle2(
  r_scripts_available ~ dbinom(size = 1,
                               prob = plogis(int +
                                               y * year +
                                               d * data_available +
                                               o * open_access +
                                              c * log_citations #note1
                               )),
  start = list(int =0,
               y = 0,
               d = 0,
               o = 0,
               c = 0),
  data = code_data
)
#note1- use log to show diminishing returns. 1->10 bigger impact than 100->109
#Alt. 3 - Data availability and open access
sharing.data.oa <- mle2(
  r_scripts_available ~ dbinom(size = 1,
                               prob = plogis(int +
                                               d * data_available^e +
                                               o * open_access^p
                               )),
  start = list(int = 0,
               d = 0,
               e = 1,
               o = 0,
               p = 1),
  data = code_data
)
#Alt. 4 - Year and open access
sharing.year.oa <- mle2(
  r_scripts_available ~ dbinom(size = 1,
                               prob = plogis(int +
                                               y * year^b +
                                               o * open_access^p
                               )),
  start = list(int = 0,
               y = 0,
               b = 1,
               o = 0,
               p = 1),
  data = code_data
)
#Alt.5 - Null/Intercept only
sharing.null<-mle2(
  r_scripts_available~dbinom(size=1,prob=plogis(int)),
  start=list(int=0),
  data=code_data
)
AIC(sharing.fit,sharing.year.da,sharing.data.oa,sharing.year.oa,sharing.null)
AICtab(sharing.fit,sharing.year.da,sharing.data.oa,sharing.year.oa,sharing.null)
#The best model is sharing.year.da, with a dAIC of 0. The second best model 
#would be sharing.fit with a dAIC of 1.9.
# 1c ----------------------------------------------------------------------

# This model attempts to explain size variation in the wings of birds
# Wing.length is mean adult wing length
# Mass is mean adult body mass
# Range.size is the area of the geographic range of each species
# Order1 is a categorical variable that lists the taxonomic Order each species fall into.

# Note that I provide two ways to load the avonet dataset in case the csv file won't load for some of you.

avonet <- read_rds("https://github.com/bmaitner/Statistical_ecology_course/raw/refs/heads/main/data/Avonet/AVONET1_BirdLife.rds") %>%
  select(Order1, Wing.Length, Mass, Range.Size) %>%
  na.omit()

avonet  <- read.csv("https://github.com/bmaitner/Statistical_ecology_course/raw/refs/heads/main/data/Avonet/AVONET1_BirdLife.csv") %>%
  select(Order1, Wing.Length, Mass, Range.Size) %>%
  na.omit()

avonet %>%
  ggplot(mapping = aes(y=Wing.Length,x=Mass))+
  geom_point()

# Note: there is a lot of data here, it may take a while to fit the full model
#Original
avonet.fit <- mle2(Wing.Length ~ dlnorm(meanlog = int +
                                          m*log(Mass)^b +
                                          rs*Range.Size,
                                        sdlog = sd),
                   start = list(m = 1,
                                b = 1,
                                sd = 1,
                                int = 0,
                                rs = 10),
                   data = avonet,
                   parameters = list(int ~ Order1))
#Alt 1 - Range size with order
avonet.ran.ord <- mle2(Wing.Length ~ dlnorm(meanlog = int +
                                          rs*Range.Size,
                                        sdlog = sd),
                   start = list(sd = 1,
                                int = 0,
                                rs = 10),
                   data = avonet,
                   parameters = list(int ~ Order1))
#Alt 2 - Mass only
avonet.mass <- mle2(Wing.Length ~ dlnorm(meanlog = int +
                                          m*log(Mass)^b,
                                        sdlog = sd),
                   start = list(m = 1,
                                b = 1,
                                sd = 1,
                                int = 0),
                   data = avonet)
#Alt 3 - Mass only, Linear
avonet.mass.linear <- mle2(Wing.Length ~ dlnorm(meanlog = int +
                                           m*log(Mass),
                                         sdlog = sd),
                    start = list(m = 1,
                                 sd = 1,
                                 int = 0),
                    data = avonet)
#Alt 4 - Null/Intercept only
avonet.null<-mle2(Wing.Length~dlnorm(meanlog = int,
                                     sdlog=sd),
                  start=list(int=0,sd=1),
                  data=avonet)
AIC(avonet.fit,avonet.ran.ord,avonet.mass,avonet.mass.linear,avonet.null)
AICtab(avonet.fit,avonet.ran.ord,avonet.mass,avonet.mass.linear,avonet.null)
#The best model is avonet.mass with a dAIC of 0. The second best model is avonet.mass.linear with a dAIC of 26.9. 

#Notes
#Always include range of models for dAIC:
#Null model, single predictor, additive model, full/complex model

#RECHECK MODELS USING TODAYS NOTES

#PART TWO
library(readxl)
read_excel("C:/Users/schut/Downloads/Waterhole_Data.xlsx")
wathole=read_excel("C:/Users/schut/Downloads/Waterhole_Data.xlsx")
read_excel("C:/Users/schut/Downloads/Waterhole_Data 1.xlsx")
Elephant=read_excel("C:/Users/schut/Downloads/Waterhole_Data 1.xlsx")
read_excel("C:/Users/schut/Downloads/Waterhole_Data 2.xlsx")
BlkRhno=read_excel("C:/Users/schut/Downloads/Waterhole_Data 2.xlsx")
read_excel("C:/Users/schut/Downloads/Waterhole_Data 3.xlsx")
WhtRhno=read_excel("C:/Users/schut/Downloads/Waterhole_Data 3.xlsx")
wathole$Species<-as.factor(wathole$Species)
wathole$WHType<-as.factor(wathole$WHType)
modelNoAnWHT<-glm(NoAnimals~WHType,data=wathole)
AIC(modelNoAnWHT,modelDurNoAni)
modelNoAnWHTEle<-glm(NoAnimals~WHType,data=Elephant)
modelNoAnWHTBlkRh<-glm(NoAnimals~WHType,data=BlkRhno)
modelNoAnWHTWhtRhno<-glm(NoAnimals~WHType,data=WhtRhno)
modelDurNoAni<-glm(DurationMins~NoAnimals,data=wathole)
#modelNoAnWHT is the better model, lower AIC 
