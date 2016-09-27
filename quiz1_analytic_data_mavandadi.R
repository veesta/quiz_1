library(tidyverse)
"1_quiz1_analytic_data_script.R"

#download raw data

raw_data <- read_csv(file="lab_quiz_week2_data.csv")

raw_data <- read_csv(file="lab_quiz_week2_data.csv",na=c("","NA","-999"))

#View(raw_data)

#make categorical variables - university, program year 

categorical_variables <- select(raw_data, univ, prog_year)

categorical_variables$univ <- as.factor(categorical_variables$univ)
levels(categorical_variables$univ) <- list("Waterloo"=1,"Guelph"=2)

categorical_variables$prog_year <- as.factor(categorical_variables$prog_year)
levels(categorical_variables$prog_year) <- list("First Year"=1,"Second Year"=2,"Third Year"=3,"Fourth Year"=4,"Grad School"=5)

#View(categorical_variables)

#create sets of scaled items

pos_affect_items <- select (raw_data, PA1, PA2, PA3, PA4, PA5)
dep_items <- select (raw_data, D1, D2, D3, D4, D5)
prog_sat_items <- select (raw_data, PS1, PS2, PS3, PS4, PS5)

age <- select (raw_data, age)
#check if there are impossible values 

psych::describe(pos_affect_items)
is_bad_value <- pos_affect_items<1 | pos_affect_items>7
pos_affect_items[is_bad_value] <- NA
psych::describe(pos_affect_items)

psych::describe(dep_items)
is_bad_value <- dep_items<1 | dep_items>4
dep_items[is_bad_value] <- NA
psych::describe(dep_items)

psych::describe(prog_sat_items)
is_bad_value <- prog_sat_items<1 | prog_sat_items>6
prog_sat_items[is_bad_value] <- NA
psych::describe(prog_sat_items)

#fix reverse keyed items 

#View(pos_affect_items)
pos_affect_items <- mutate(pos_affect_items,PA1=8-PA1)
#View(pos_affect_items)

dep_items <- mutate(dep_items,D4=5-D4)
dep_items <- mutate(dep_items,D5=5-D5)
#View(dep_items)

#View(prog_sat_items)
prog_sat_items <- mutate(prog_sat_items,PS1=7-PS1)
prog_sat_items <- mutate(prog_sat_items,PS2=7-PS2)
#View(prog_sat_items)

#obtain scaled scores 

pos_affect <- psych::alpha(as.data.frame(pos_affect_items), check.keys=FALSE)$scores
dep <- psych::alpha(as.data.frame(dep_items), check.keys=FALSE)$scores
prog_sat <- psych::alpha(as.data.frame(prog_sat_items), check.keys=FALSE)$scores

#combine everything 
analytic_data <- cbind(categorical_variables,pos_affect,dep,prog_sat)

write_csv(analytic_data,path="quiz1_analytic_data_mavandadi.csv")
