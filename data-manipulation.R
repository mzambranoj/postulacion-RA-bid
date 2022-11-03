# ________________________________
# Medicare Advantage Enrollment
# Research assistant postulation
# Author: Miguel Zambrano
# ________________________________

library(dplyr) # Only package required

# Choose you working directory
setwd("C:/Users/jgzj0/Downloads/Miguel/Medicare_Advantage")

# Upload dataset
medicare <- read.csv("scp-1205.csv", header = F)

# Change variable names
names(medicare) <- c("countyname","state","contract","healthplanname","typeofplan",
                     "countyssa","eligibles","enrollees","penetration","ABrate")

# Missing values to 0
medicare$eligibles[is.na(medicare$eligibles)] <- 0
medicare$enrollees[is.na(medicare$enrollees)] <- 0
medicare$penetration[is.na(medicare$penetration)] <- 0

# Generate county level dataset
medicare <- medicare |> group_by(countyname) |> 
  summarise(state = state[1],
            numberofplans1 = sum(enrollees > 10),
            numberofplans2 = sum(penetration > 0.5),
            countyssa = countyssa[1],
            eligibles = eligibles[1],
            totalenrollees = sum(enrollees, na.rm=T)) |> 
  mutate(totalpenetration = totalenrollees/eligibles) |>
  arrange(state) # Sort by state, then by county

# Done!
# In totalpenetration, many values were generated as missing values due to the existence 
# of zero eligibles. I could convert this missing values also into zeros, but I
# prefer to keep them as missing values so there is no confusion between those counties
# and the counties with null penetration due to none enrollment

# As said, in case you only want to keep the 50 states (non territories):
# Also, this lines discard a few observations that where not associated to a state
medicare <- medicare |>
  mutate(state = gsub(" ", "",state)) |>
  subset(state %in% c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID",
                      "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
                      "MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK",
                      "OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV",
                      "WI","WY"))

# Saving dataset as csv
write.csv(medicare, "Medicare_by_county.csv")

