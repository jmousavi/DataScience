## -----------------------------------------------------------------------------

#install.packages("tidyverse", repos="http://cran.us.r-project.org")  #data wrangling 
#install.packages("dplyr", repos="http://cran.us.r-project.org")      #data wrangling
#install.packages("lubridate", repos="http://cran.us.r-project.org")  #date transforming
#install.packages("ggplot2", repos="http://cran.us.r-project.org")    #graphs, plots, visualizations
#install.packages("usmap", repos="http://cran.us.r-project.org")      #usa map visualization 
#install.packages("reshape2", repos="http://cran.us.r-project.org")   #used to reshape data frames 
# If you are using mac, you will need to install XQuarts on your machine: https://www.xquartz.org/
#install.packages("summarytools", repos="http://cran.us.r-project.org") #used to describe data frames 

#load necessary libraries 
library("tidyverse")
library("dplyr")
library("lubridate")
library("ggplot2")
library("usmap")
library("reshape2")
#library("summarytools")
library("knitr")


## -----------------------------------------------------------------------------
gun_violence <- read_csv("./gun-violence-data_01-2013_03-2018.csv")


## -----------------------------------------------------------------------------
#make tibble of selected variables 
Participant_Info <- tibble(
  incident_id=gun_violence$incident_id,
  participant_name=gun_violence$participant_name,
  participant_age=gun_violence$participant_age,
  participant_age_group=gun_violence$participant_age_group,
  participant_gender=gun_violence$participant_gender)

head(Participant_Info)


## -----------------------------------------------------------------------------
#separate rows & match mappings
Participant_Info <- Participant_Info %>%
  separate_rows(participant_name, sep="\\|\\|", convert=TRUE)

Participant_Info <- Participant_Info %>%
  separate_rows(participant_age, sep="\\|\\|", convert=TRUE)

Participant_Info <- subset(Participant_Info, substr(participant_name, 0, 1)==substr(participant_age, 0, 1))

Participant_Info <- Participant_Info %>%
  separate_rows(participant_age_group, sep="\\|\\|", convert=TRUE)
Participant_Info <- Participant_Info %>%
  separate_rows(participant_age_group, sep="\\|", convert=TRUE)

Participant_Info <- subset(Participant_Info, substr(participant_name, 0, 1)==substr(participant_age_group, 0, 1))

Participant_Info <- Participant_Info %>%
  separate_rows(participant_gender, sep="\\|\\|", convert=TRUE)
Participant_Info <- Participant_Info %>%
  separate_rows(participant_gender, sep="\\|", convert=TRUE)

Participant_Info <- subset(Participant_Info, substr(participant_name, 0, 1)==substr(participant_gender, 0, 1))


#take out mappings
Participant_Info$participant_name <- gsub("^\\d+::", "", Participant_Info$participant_name)
Participant_Info$participant_age <- gsub("^\\d+::", "", Participant_Info$participant_age)
Participant_Info$participant_age_group <- gsub("^\\d+::", "", Participant_Info$participant_age_group)
Participant_Info$participant_gender <- gsub("^\\d+::", "", Participant_Info$participant_gender)
Participant_Info$participant_age_group <- gsub("^\\d+:", "", Participant_Info$participant_age_group)
Participant_Info$participant_gender <- gsub("^\\d+:", "", Participant_Info$participant_gender)


## -----------------------------------------------------------------------------
#convert factor 
Participant_Info$participant_age_group <- as.factor(Participant_Info$participant_age_group)
Participant_Info$participant_gender <- as.factor(Participant_Info$participant_gender)
Participant_Info$incident_id <- as.factor(Participant_Info$incident_id)

#make sure that age is numeric and not categorical 
Participant_Info$participant_age <- as.numeric(Participant_Info$participant_age)

head(Participant_Info)


## -----------------------------------------------------------------------------
#make tibble of selected variables 
Participant_Incident_Info <- tibble(
  incident_id=gun_violence$incident_id,
  participant_name=gun_violence$participant_name,
  participant_status=gun_violence$participant_status,
  participant_type=gun_violence$participant_type)

head(Participant_Incident_Info)


## -----------------------------------------------------------------------------
#separate rows & match mappings
Participant_Incident_Info <- Participant_Incident_Info %>%
  separate_rows(participant_name, sep="\\|\\|", convert=TRUE)

Participant_Incident_Info <- Participant_Incident_Info %>%
  separate_rows(participant_status, sep="\\|\\|", convert=TRUE)
Participant_Incident_Info <- Participant_Incident_Info %>%
  separate_rows(participant_status, sep="\\|", convert=TRUE)

Participant_Incident_Info <- subset(Participant_Incident_Info, substr(participant_name, 0, 1)==substr(participant_status, 0, 1))

Participant_Incident_Info <- Participant_Incident_Info %>%
  separate_rows(participant_type, sep="\\|\\|", convert=TRUE)
Participant_Incident_Info <- Participant_Incident_Info %>%
  separate_rows(participant_type, sep="\\|", convert=TRUE)

Participant_Incident_Info <- subset(Participant_Incident_Info, substr(participant_name, 0, 1)==substr(participant_type, 0, 1))


#take out mappings
Participant_Incident_Info$participant_name <- gsub("^\\d+::", "", Participant_Incident_Info$participant_name)
Participant_Incident_Info$participant_name <- gsub("^\\d+:", "", Participant_Incident_Info$participant_name)
Participant_Incident_Info$participant_status <- gsub("^\\d+::", "", Participant_Incident_Info$participant_status)
Participant_Incident_Info$participant_status <- gsub("^\\d+:", "", Participant_Incident_Info$participant_status)
Participant_Incident_Info$participant_type <- gsub("^\\d+::", "", Participant_Incident_Info$participant_type)
Participant_Incident_Info$participant_type <- gsub("^\\d+:", "", Participant_Incident_Info$participant_type)


## -----------------------------------------------------------------------------
#convert to factor
Participant_Incident_Info$participant_status <- as.factor(Participant_Incident_Info$participant_status)
Participant_Incident_Info$participant_type <- as.factor(Participant_Incident_Info$participant_type)
Participant_Incident_Info$incident_id <- as.factor(Participant_Incident_Info$incident_id)

head(Participant_Incident_Info)


## -----------------------------------------------------------------------------
#make tibble of selected variables 
Gun_Info <- tibble(
  incident_id=gun_violence$incident_id,
  participant_name=gun_violence$participant_name,
  gun_stolen=gun_violence$gun_stolen,
  gun_type=gun_violence$gun_type)

head(Gun_Info)


## -----------------------------------------------------------------------------
#separate rows & match mappings
Gun_Info <- Gun_Info %>%
  separate_rows(participant_name, sep="\\|\\|", convert=TRUE)

Gun_Info <- Gun_Info %>%
  separate_rows(gun_stolen, sep="\\|\\|", convert=TRUE)
Gun_Info <- Gun_Info %>%
  separate_rows(gun_stolen, sep="\\|", convert=TRUE)

Gun_Info <- subset(Gun_Info, substr(participant_name, 0, 1)==substr(gun_stolen, 0, 1))

Gun_Info <- Gun_Info %>%
  separate_rows(gun_type, sep="\\|\\|", convert=TRUE)

Gun_Info <- subset(Gun_Info, substr(participant_name, 0, 1)==substr(gun_type, 0, 1))
#remove mappings
Gun_Info$participant_name <- gsub("^\\d+::", "", Gun_Info$participant_name)
Gun_Info$gun_stolen <- gsub("^\\d+::", "", Gun_Info$gun_stolen)
Gun_Info$gun_stolen <- gsub("^\\d+:", "", Gun_Info$gun_stolen)
Gun_Info$gun_type <- gsub("^\\d+::", "", Gun_Info$gun_type)
Gun_Info$gun_type <- gsub("^\\d+:", "", Gun_Info$gun_type)


## -----------------------------------------------------------------------------
#convert to factor 
Gun_Info$gun_stolen <- as.factor(Gun_Info$gun_stolen)
Gun_Info$gun_type <- as.factor(Gun_Info$gun_type)
Gun_Info$incident_id <- as.factor(Gun_Info$incident_id)

head(Gun_Info)


## -----------------------------------------------------------------------------
#make tibble of selected variables 
Incident_Stats <- tibble(
  incident_id=gun_violence$incident_id,
  date=gun_violence$date,
  state=gun_violence$state,
  city_or_county=gun_violence$city_or_county,
  latitude=gun_violence$latitude,
  longitude=gun_violence$longitude,
  n_killed=gun_violence$n_killed,
  n_guns_involved=gun_violence$n_guns_involved,
  n_injured=gun_violence$n_injured)

head(Incident_Stats)


## -----------------------------------------------------------------------------
Incident_Stats$date<- as.POSIXlt(parse_datetime(as.character(Incident_Stats$date), format="%Y-%m-%d"))

#need to make incident id categorical
Incident_Stats$incident_id <- as.character(Incident_Stats$incident_id)
#convert state to factor 
Incident_Stats$state <- as.factor(Incident_Stats$state)
Incident_Stats$city_or_county <- as.factor(Incident_Stats$city_or_county)

head(Incident_Stats)


## -----------------------------------------------------------------------------
#make tibble of selected variables 
Incident_Government_Info <- tibble(
  incident_id=gun_violence$incident_id,
  state_house_district=gun_violence$state_house_district,
  state_senate_district=gun_violence$state_senate_district,
  congressional_district=gun_violence$congressional_district)

head(Incident_Government_Info)


## -----------------------------------------------------------------------------
#need to make incident id categorical
Incident_Government_Info$incident_id <- as.character(Incident_Government_Info$incident_id)
Incident_Government_Info$state_house_district<- as.factor(Incident_Government_Info$state_house_district)
Incident_Government_Info$state_senate_district <- as.factor(Incident_Government_Info$state_senate_district)
Incident_Government_Info$congressional_district <- as.factor(Incident_Government_Info$congressional_district)

head(Incident_Government_Info)


## -----------------------------------------------------------------------------
#make tibble of selected variables 
Incident_Characteristics <- tibble(
  incident_id=gun_violence$incident_id,
  incident_characteristics=gun_violence$incident_characteristics)

head(Incident_Characteristics)


## -----------------------------------------------------------------------------
#separate rows 
Incident_Characteristics <- Incident_Characteristics %>%
  separate_rows(incident_characteristics, sep="\\|\\|", convert=TRUE)


## -----------------------------------------------------------------------------
#convert to factor
Incident_Characteristics$incident_id <- as.factor(Incident_Characteristics$incident_id)
Incident_Characteristics$incident_characteristics<- as.factor(Incident_Characteristics$incident_characteristics)

head(Incident_Characteristics)


## -----------------------------------------------------------------------------
#dfSummary(Incident_Government_Info, style="grid", graph.col=FALSE, max.distinct.values=5, valid.col=FALSE)


## -----------------------------------------------------------------------------
#freq(Incident_Government_Info$state_house_district, plain.ascii = FALSE, style = "rmarkdown", order="freq", rows=1:5)
#freq(Incident_Government_Info$state_senate_district, plain.ascii = FALSE, style = "rmarkdown", order="freq", rows=1:5)
#freq(Incident_Government_Info$congressional_district, plain.ascii = FALSE, style = "rmarkdown", order="freq", rows=1:5)


## -----------------------------------------------------------------------------
incident_stats <- Incident_Stats
incident_stats$date <- ymd(incident_stats$date)  #need to format of date in order to work with dfSummary()
#dfSummary(incident_stats, style="grid", graph.col=FALSE, max.distinct.values=5, valid.col=FALSE)
summary(incident_stats)


## -----------------------------------------------------------------------------
year <- table(year=year(Incident_Stats$date)) #create data frame that separates the year

#Plot the data
barplot(year, main="Incidents per Year", xlab="Year", col="#785EF0")


## -----------------------------------------------------------------------------
#create a data frame that separates the year, month, and day from the date
df <- data.frame(date=Incident_Stats$date)
df$month <- month(df$date)
df$year <- year(df$date)

#group together by year, month, and day
df$year <- as.factor(df$year)
count <- df %>% 
  group_by(year) %>% 
  count(month) %>% 
  group_by(month) %>% 
  summarise(mean=mean(n)) #gather the means of the groups
count$month <- as.factor(count$month) #group together the months into categories/levels

#Plot the data
ggplot(count, aes(x=month, y=mean)) + 
  geom_bar(stat="identity",fill="#648FFF" ) + 
  labs(x="Month", y="Mean")+
  ggtitle("Average Incidents per Month")+
  scale_x_discrete(labels=c("January","Februrary","March","April","May","June","July","August","September","October","November","December"))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), 
        plot.title = element_text(hjust=0.5))


## -----------------------------------------------------------------------------
UsaMap <- Incident_Stats
UsaMap <- UsaMap %>% count(state)  # create a data frame of the counts per state
UsaMap$state <- as.factor(UsaMap$state)
UsaMap
#Plot the data
plot_usmap(data = UsaMap, values = "n", color = "#FE6100") + 
  scale_fill_continuous(
    low = "white", high = "#FE6100", name = "Incidents in the USA", label = scales::comma
  ) + theme(legend.position = "right")


## -----------------------------------------------------------------------------
Stats_Total <- Incident_Stats
Stats_Total$date <- ymd(Stats_Total$date) #change the date format to work better with our graph
Stats_Total$n_injured <- as.numeric(Stats_Total$n_injured) #need to make sure this is numeric in order to obtain total later
Stats_Total$n_killed <- as.numeric(Stats_Total$n_killed) #need to make sure this is numeric in order to obtain total
Stats_Total$total <- Stats_Total$n_injured + Stats_Total$n_killed #get the total

totals <- data.frame(total=Stats_Total$total, 
                     injured=Stats_Total$n_injured, 
                     killed=Stats_Total$n_killed, 
                     date=Stats_Total$date) #create new data frame to reshape 

# reshape data frame in order to have the columns as rows, and also have a column for the frequencies of each variable
melted_totals <- melt(totals, measure.vars = c("total","killed","injured")) 

head(melted_totals)


## -----------------------------------------------------------------------------
#Plot the data
ggplot() + 
  geom_line(data=melted_totals, aes(x=date, y=value, colour=variable)) + 
  scale_color_manual(values=c("#785EF0", "#DC267F", "#FFB000" )) +
  xlab("Date") +
  ylab("Value") +
  ggtitle("Killed or Injured over Time > 0") +
  theme(plot.title = element_text(hjust=0.5))


## -----------------------------------------------------------------------------
#ugly lets limit it
Stats_Total <- Stats_Total %>% filter(total > 10) #filter total to be greater than 10

totals <- data.frame(total=Stats_Total$total, 
                     injured=Stats_Total$n_injured, 
                     killed=Stats_Total$n_killed, 
                     date=Stats_Total$date) #create new data frame to reshape 

# reshape data frame in order to have the columns as rows, and also have a column for the frequencies of each variable
melted_totals <- melt(totals, measure.vars = c("total","killed","injured"))

#Plot the data
ggplot() + 
  geom_line(data=melted_totals, aes(x=date, y=value, colour=variable)) + 
  scale_color_manual(values=c("#785EF0", "#DC267F", "#FFB000" )) +
  xlab("Date") +
  ylab("Value") +
  ggtitle("Killed or Injured over Time > 10") +
  theme(plot.title = element_text(hjust=0.5))



## -----------------------------------------------------------------------------
#dfSummary(Gun_Info, style="grid", graph.col=FALSE, max.distinct.values=5, valid.col=FALSE)
summary(Gun_Info)


## -----------------------------------------------------------------------------
Gun_Info_Type_Count <- Gun_Info %>% count(gun_type) #create a data frame of the counts of types of guns
#There are many different gun types, so we are going to only look at the gun types that occur more than 150 times
Gun_Info_Type_Count <- Gun_Info_Type_Count %>% filter(n > 150) 

#Plot the data
ggplot(Gun_Info_Type_Count, aes(x=gun_type, y=n)) + 
  geom_bar(stat="identity", fill="#DC267F") + 
  labs(x="Type", y="Count")+
  ggtitle("Count of Gun Types > 150") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), plot.title = element_text(hjust=0.5))


## -----------------------------------------------------------------------------
Gun_Info_Stolen_Count <- Gun_Info %>% count(gun_stolen) #Create a data frame of the gun stolen counts

#Plot the data
ggplot(Gun_Info_Stolen_Count, aes(x=gun_stolen, y=n)) + 
  geom_bar(stat="identity", fill="#FFB000") + 
  labs(x="Gun Type", y="Count")+
  ggtitle("Count of Types of Guns")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), plot.title = element_text(hjust=0.5))


## -----------------------------------------------------------------------------
#dfSummary(Participant_Incident_Info, style="grid", graph.col=FALSE, max.distinct.values=5, valid.col=FALSE)
summary(Participant_Incident_Info)


## -----------------------------------------------------------------------------
#freq(Participant_Incident_Info$participant_status, plain.ascii = FALSE, style = "rmarkdown", order="freq", rows=1:5)
#freq(Participant_Incident_Info$participant_type, plain.ascii = FALSE, style = "rmarkdown", order="freq", rows=1:5)


## -----------------------------------------------------------------------------
#dfSummary(Participant_Info, style="grid", graph.col=FALSE, max.distinct.values=5, valid.col=FALSE)
summary(Participant_Info)


## -----------------------------------------------------------------------------
head(sort(Participant_Info$participant_age, decreasing=TRUE), n=150) #view the top 150 sorted from greatest to least


## -----------------------------------------------------------------------------
grouped_participant <- Participant_Info %>%
  group_by(participant_gender, participant_age_group) %>%
  summarise(Count=n()) #group together gender and age group

#Plot the data
ggplot(grouped_participant, aes(x=participant_age_group, y=Count, fill=participant_gender)) + 
  geom_bar(stat="identity", position="fill") + 
  scale_fill_manual(values=c("#DC267F", "#648FFF")) +
  labs(x="Age Group", y="Count")+
  ggtitle("Participant Age Groups")+
  theme(plot.title = element_text(hjust=0.5))


## -----------------------------------------------------------------------------
#dfSummary(Incident_Characteristics, style="grid", graph.col=FALSE, max.distinct.values=5, valid.col=FALSE)
summary(Incident_Characteristics)


## -----------------------------------------------------------------------------
Incident_Characteristics_Count <- Incident_Characteristics %>% count(incident_characteristics) #creating a data frame of counts of each characteristic
Incident_Characteristics_Count <- Incident_Characteristics_Count %>% filter(n > 10000) #Going to limit the characteristics that have counts greater than 10,000

#Plot the data
ggplot(Incident_Characteristics_Count , aes(x=incident_characteristics, y=n)) + 
  geom_bar(stat="identity", fill="#FE6100") + 
  labs(x="Characteristics", y="Count")+
  ggtitle("Incident Characteristics > 10,000")+
  coord_flip()+
  theme(plot.title = element_text(hjust=0.5))

