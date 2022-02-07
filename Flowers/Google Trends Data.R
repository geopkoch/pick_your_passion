library(gtrendsR)
library(data.table)
library(tidyverse)

data.roses <- gtrends(keyword = "roses",
                        geo = "US",
                        time = "all")
trends.roses <- data.roses$interest_over_time
states.roses <- data.roses$interest_by_region


data.peonies <- gtrends(keyword = "peonies",
                      geo = "US",
                      time = "all")
trends.peonies <- data.peonies$interest_over_time
states.peonies <- data.peonies$interest_by_region


data.tulips <- gtrends(keyword = "tulips",
                      geo = "US",
                      time = "all")
trends.tulips <- data.tulips$interest_over_time
states.tulips <- data.tulips$interest_by_region


data.sunflowers <- gtrends(keyword = "sunflowers",
                      geo = "US",
                      time = "all")
trends.sunflowers <- data.sunflowers$interest_over_time
states.sunflowers <- data.sunflowers$interest_by_region

#union
trends.all <- rbind(trends.roses,trends.peonies,trends.tulips,trends.sunflowers)
states.all <- rbind(states.roses,states.peonies,states.tulips,states.sunflowers) %>%
  rename(State = location)

#don't need old dfs
rm(list = setdiff(ls(),c("trends.all","states.all")))

##TODO: work on applying transformations to the weighted trends per state. also work on creating the scaffold the data needs to be in

alldata <- inner_join(trends.all,states.all, by = "keyword") %>%
  rename(hits.overall = hits.x,
         hits.states = hits.y) %>%
  select(-geo.x,-geo.y,-gprop.x,-gprop.y,-category,-time) %>%
  mutate(hits.adjusted = hits.overall * hits.states/100, #adjust trends and supply coordinates for Tableau
         ) 

state.coordinates <- data.frame(
  stringsAsFactors = FALSE,
                 x = c(0L,6L,5L,5L,5L,4L,3L,5L,
                       4L,8L,7L,8L,3L,3L,3L,3L,5L,4L,6L,2L,4L,0L,
                       2L,2L,4L,6L,2L,5L,2L,4L,1L,3L,6L,4L,2L,3L,
                       6L,4L,3L,2L,6L,3L,5L,7L,5L,5L,1L,2L,2L,4L,3L),
                 y = c(0.5,7.5,6,3,2,3.5,11,12,
                       9.5,8.5,8,0.5,5,2,6,7,5,6.5,5.5,10.5,8.5,
                       11.5,7.5,4.5,5.5,6.5,2.5,9,3.5,4.5,11,10,3.5,
                       2.5,9.5,8,4.5,1.5,9,11.5,8.5,4,7,4,4,8,10,1.5,
                       5.5,7.5,3),
             State = c("Alaska","Alabama","Arkansas",
                       "Arizona","California","Colorado","Connecticut",
                       "District of Columbia","Delaware","Florida","Georgia",
                       "Hawaii","Iowa","Idaho","Illinois","Indiana","Kansas",
                       "Kentucky","Louisiana","Massachusetts","Maryland",
                       "Maine","Michigan","Minnesota","Missouri","Mississippi",
                       "Montana","North Carolina","North Dakota",
                       "Nebraska","New Hampshire","New Jersey","New Mexico",
                       "Nevada","New York","Ohio","Oklahoma","Oregon",
                       "Pennsylvania","Rhode Island","South Carolina",
                       "South Dakota","Tennessee","Texas","Utah","Virginia",
                       "Vermont","Washington","Wisconsin","West Virginia",
                       "Wyoming"),
          StatAbbr = c("AK","AL","AR","AZ","CA",
                       "CO","CT","DC","DE","FL","GA","HI","IA","ID","IL",
                       "IN","KS","KY","LA","MA","MD","ME","MI","MN",
                       "MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV",
                       "NY","OH","OK","OR","PA","RI","SC","SD","TN",
                       "TX","UT","VA","VT","WA","WI","WV","WY")
) #coordinates for flower map in Tableau


alldata.final <- left_join(alldata,state.coordinates, by = "State")

fwrite(alldata.final, file = "PATH")
