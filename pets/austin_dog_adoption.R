#                                                                               
#                          What matters in Austin pet adoptions?                  
#                                                                               
# Purpose: Generating monthly returning net reg forecasts using datarobot's model
# Created by: Geoff Koch                                                        
# Created on: 4/16/2021                                              
# Validated by:                                               
# Validated on:                                                      
#Begin################################################################################

source("P:/APUS_Corporate/Institutional Research/Data Team/Scripts/Common Code/R Scripts/Functions.R")
if (!require("pacman")) install.packages("pacman")
p_load(dplyr,magrittr,tidyr,readr,stringr,RODBC,dummies,rpart,rpart.plot,caret)


mydata <- read.csv("https://raw.githubusercontent.com/geopkoch/Misc/master/PickYourPassion/Pets/petdata%20(cleaned).csv") %>%
  janitor::clean_names() %>%
  filter(animal_type_intake == "Dog") 


#preserve master
mydata.master <- mydata

#all animal types
mydata <- mydata %>% 
  filter(days_length != "") %>%
  mutate(is_adopted_or_rto = case_when(outcome_type == "Adoption" ~ 1,
                                       outcome_type == "Return to Owner" ~ 1,
                                       outcome_type == "Rto-Adopt" ~ 1,
                             TRUE ~ 0)) %>%
  select(-outcome_type, -color_intake)

#transform into dummy variables
d.type <- as.data.frame(dummies::dummy(mydata$intake_type)) %>%
  janitor::clean_names()
d.condition <- as.data.frame(dummies::dummy(mydata$intake_condition)) %>%
  janitor::clean_names()
d.fixed <- as.data.frame(dummies::dummy(mydata$fixed_intake)) %>%
  janitor::clean_names()
# d.breed <- as.data.frame(dummies::dummy(mydata$breed_intake)) %>%
#   janitor::clean_names()
d.age <- as.data.frame(dummies::dummy(mydata$age_bucket)) %>%
  janitor::clean_names()
# d.length <- as.data.frame(dummies::dummy(mydata$days_length)) %>%
#   janitor::clean_names()

#combine the dummied data
mydata.all <- cbind(mydata,
      d.type,
      d.condition,
      d.fixed,
      d.age
      #d.length
      ) %>%
  select(-intake_type,-intake_condition,-fixed_intake,-breed_intake,-age_bucket,-animal_id,
         -fixed_changed, -has_name, -animal_type_intake) #removing fixed_change and has_name (leak from future and other derived)

#all dogs
mydata.isadopt <- mydata.all %>%
  select(-days_length) 
  

#intake normal dogs
mydata.isadopt.normal <- mydata.all %>%
  filter(intake_condition_normal == 1) %>%
  select(-days_length, -starts_with("intake_condition"))


#adopted dogs, what factors explain length to adoption?
mydata.length <- mydata.all %>%
  filter(is_adopted_or_rto == 1) %>%
  select(-is_adopted_or_rto)



# Split Data --------------------------------------------------------------

# _ isAdopted ---------------------------------------------------------------

set.seed(123)
index.isadopt <- createDataPartition(mydata.isadopt$is_adopted_or_rto,
                                            p = .7)

train.mydata.isadopt <- mydata.isadopt[index.isadopt$Resample1,]
test.mydata.isadopt <- mydata.isadopt[-index.isadopt$Resample1,]

glm.isadopt <- glm(is_adopted_or_rto ~ . , data = train.mydata.isadopt, family = "binomial")

isadopted.pred <-   broom::augment(glm.isadopt, newdata = test.mydata.isadopt, type.predict = "response") %>%
  mutate(actual = is_adopted_or_rto,
         pred = case_when(.fitted > .7 ~ 1, #.5 because, well
                          TRUE ~ 0)
  )

#table of results
xtab <- table(isadopted.pred$pred,
              isadopted.pred$actual)

caret::confusionMatrix(xtab)


pROC::roc(isadopted.pred$actual ~ isadopted.pred$pred,plot=TRUE,print.auc=TRUE,col="green",lwd =4,legacy.axes=TRUE,main="ROC Curves")


##### HORRIBLE RESULTS, so I shall not be continuing this experiment

# Trees -------------------------------------------------------------------

#is adopted?
tree.isadopt <- rpart(is_adopted_or_rto ~ ., data = mydata.isadopt, method = "anova", control = rpart.control(minsplit=100, cp=0.001, xval = 10))

rpart.plot(tree.isadopt, 
           box.palette = "OrBu",
           type = 4,
           varlen = -5,
           main = "What factors impact the incidence of dog adoption?",
           sub = "Austin, TX animal shelter intakes from October 2013 through 2017. 
           More here: https://www.kaggle.com/aaronschlegel/austin-animal-center-shelter-intakes-and-outcomes")


#tree to see who is adopted of only normal intake
tree.isadopt.normal <- rpart(is_adopted_or_rto ~ ., data = mydata.isadopt.normal, method = "anova", control = rpart.control(minsplit=100, cp=0.001, xval = 10))

rpart.plot(tree.isadopt.normal,
           box.palette = "OrBu",
           type = 4,
           varlen = -5,
           main = "What factors impact the incidence of dog adoption for 'normal' intakes?",
           sub = "Austin, TX animal shelter intakes from October 2013 through 2017. 
           More here: https://www.kaggle.com/aaronschlegel/austin-animal-center-shelter-intakes-and-outcomes")

#rattle::fancyRpartPlot(tree.isadopt.normal,main = sprintf("Dogs Adopted"), caption = paste0("Prepared by ",Sys.getenv("USERNAME")," on ",Sys.Date(),"explaining dog adoption."))


#tree for all that have already been adopted. target is length of time (multi class)
tree.adoptedlength <- rpart(days_length ~ ., data = mydata.length, method = "class", control = rpart.control(minsplit=100, cp=0.001))

rpart.plot(tree.adoptedlength, 
           box.palette = "OrBu",
           type = 4,
           varlen = -5,
           main = "What factors impact the length to adoption for dogs?",
           sub = "Austin, TX animal shelter intakes from October 2013 through 2017.
           More here: https://www.kaggle.com/aaronschlegel/austin-animal-center-shelter-intakes-and-outcomes",
           cex.sub = .6)

