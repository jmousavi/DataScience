## ----results='hide'-----------------------------------------------------------
#install.packages("knitr", repos="http://cran.us.r-project.org")  #gather data
library(knitr)
if(!file.exists("part1.r")){
   purl("P1.Rmd", output = "part1.r")
}
source("part1.r") #load the data from part 1


## -----------------------------------------------------------------------------
Participant_Info


## -----------------------------------------------------------------------------
Participant_Incident_Info


## -----------------------------------------------------------------------------
Gun_Info


## -----------------------------------------------------------------------------
Incident_Stats$date <- as.POSIXct(Incident_Stats$date) #fixes posixlt
Incident_Stats


## -----------------------------------------------------------------------------
Incident_Government_Info


## -----------------------------------------------------------------------------
Incident_Characteristics


## -----------------------------------------------------------------------------
library(rvest)     #web scrapping
library(tidyr)     #web scrapping
library(stringr)   #web scrapping

url<- "https://www.thoughtco.com/gun-owners-percentage-of-state-populations-3325153"

webpage <- read_html(url) #read in the html from the url

#use table headers as column names
col_name<- webpage %>%
  html_nodes("th") %>%
  html_text()

#save data inside table
mydata <- webpage %>%
  html_nodes("td") %>%
  html_text()

#convert scraped data to data frame
Guns_Per_Capita <- data.frame(matrix(mydata, ncol=4, byrow=TRUE)) 

Guns_Per_Capita <- Guns_Per_Capita[-1, ] 

#rename columns 
colnames(Guns_Per_Capita)[colnames(Guns_Per_Capita)=="X1"] <- "rank"
colnames(Guns_Per_Capita)[colnames(Guns_Per_Capita)=="X2"] <- "state"
colnames(Guns_Per_Capita)[colnames(Guns_Per_Capita)=="X3"] <- "num_of_guns_per_capita"
colnames(Guns_Per_Capita)[colnames(Guns_Per_Capita)=="X4"] <- "num_of_guns_registered"

Guns_Per_Capita <- as_tibble(Guns_Per_Capita)

#clean & tidy data
Guns_Per_Capita$num_of_guns_registered <- sub(",", "", Guns_Per_Capita$num_of_guns_registered)
Guns_Per_Capita$num_of_guns_registered <- as.numeric(Guns_Per_Capita$num_of_guns_registered)
Guns_Per_Capita$num_of_guns_per_capita <- as.character(Guns_Per_Capita$num_of_guns_per_capita)
options(digits = 5)
Guns_Per_Capita$num_of_guns_per_capita <- as.numeric(Guns_Per_Capita$num_of_guns_per_capita)
Guns_Per_Capita$state <- sub("Washington D.C.", "Washington", Guns_Per_Capita$state)

Guns_Per_Capita


## -----------------------------------------------------------------------------
Guns_Per_Capita_Subset <- Guns_Per_Capita %>% filter(num_of_guns_per_capita > 19) #Going to limit the guns per capita for those greater then 19

#Plot the data
ggplot(Guns_Per_Capita_Subset , aes(x=state, y=num_of_guns_per_capita)) + 
  geom_bar(stat="identity", fill="#FE6100") + 
  labs(x="State", y="Guns Per Capita")+
  ggtitle("Guns Per Capita > 19")+
  coord_flip()+
  theme(plot.title = element_text(hjust=0.5))


## -----------------------------------------------------------------------------

url<- "https://www.enchantedlearning.com/usa/states/population.shtml"

webpage <- read_html(url) #read in the html

#use table headers as column names
col_name<- webpage %>%
  html_nodes("th") %>%
  html_text()

#save data inside table
mydata <- webpage %>%
  html_nodes("td") %>%
  html_text()

#convert scraped data to data frame
Population_2017 <- data.frame(matrix(mydata, ncol=2, byrow=TRUE))

Population_2017 <- Population_2017[-1, ] 

#rename columns 
colnames(Population_2017)[colnames(Population_2017)=="X1"] <- "state"
colnames(Population_2017)[colnames(Population_2017)=="X2"] <- "population"


Population_2017 <- as_tibble(Population_2017)

#clean & tidy data
Population_2017$state <- sub("\\d*\\. ", "", Population_2017$state)
Population_2017$population <- sub(",", "", Population_2017$population)
Population_2017$population <- sub(",", "", Population_2017$population)
Population_2017$population <- as.numeric(Population_2017$population)
Population_2017$state <- as.character(Population_2017$state)

Population_2017


## -----------------------------------------------------------------------------
Population_2017_Subset <- Population_2017 %>% filter(population > 6000000) #Going to limit the guns per capita for those greater then 6000000

ggplot(Population_2017_Subset , aes(x=state, y=population)) + 
  geom_bar(stat="identity", fill="#648FFF") + 
  labs(x="State", y="Population")+
  ggtitle("Population > 6000000") +
  coord_flip()+
  theme(plot.title = element_text(hjust=0.5))



## -----------------------------------------------------------------------------
# Join our incident table with our guns per capita table, for only 2017 data
State_Info <- Incident_Stats %>% 
                    subset(format(as.Date(date),"%Y")==2017) %>%
                    left_join(Guns_Per_Capita, by="state") %>%
                    select(state,num_of_guns_per_capita,num_of_guns_registered,n_injured,n_killed,n_guns_involved)

# Now join the talbe we just created with our population table                  
State_Info <- State_Info %>% 
                    left_join(Population_2017, by="state") %>%
                    select(num_of_guns_per_capita,num_of_guns_registered,n_injured,n_killed,n_guns_involved,population,state)
                    
State_Info$state <- as.character(State_Info$state)

#create incidents per capita column by calculating number of incidents and dividing it by the population
State_Info$num_incidents <- as.numeric(ave(State_Info$state, State_Info$state, FUN = length))
State_Info$num_incidents_per_capita <- State_Info$num_incidents/State_Info$population

State_Info


## -----------------------------------------------------------------------------
library(reshape2) #used to melt data set
Grouped_State_Info <- as.tibble(na.omit(State_Info)) %>% 
                      group_by(state, num_of_guns_per_capita, num_incidents_per_capita) %>% 
                      summarise(total=sum(num_of_guns_per_capita, num_incidents_per_capita)) %>%
                      mutate("gun_percentage"=num_of_guns_per_capita/total,"incident_percentage"=num_incidents_per_capita/total) %>% 
                      ungroup() %>%
                      select(state, gun_percentage, incident_percentage) 

Melted_State_Info <- melt(Grouped_State_Info, id.vars='state')
ggplot(Melted_State_Info, aes(state,value,fill=variable)) +
  geom_bar(stat="identity",position=position_stack()) +
  scale_fill_manual(values=c("#DC267F", "#648FFF")) +
  labs(x="State", y="Proportion")+
  ggtitle("Proportion of Capita")+
  theme(plot.title = element_text(hjust=0.5), axis.text.x = element_text(angle = 90, hjust = 1))


## -----------------------------------------------------------------------------
library(caret) #lm model
set.seed(385) #set the seed to get the same results 
data_1 <- as_tibble(na.omit(State_Info)) #must remove na values when creating lm

#Partition into Train(75%) and Test(25%) sets
sample_1 <- createDataPartition(data_1$num_incidents_per_capita, p=0.75, list=FALSE)
train_1 <- data_1[sample_1, ]
test_1 <- data_1[-sample_1, ]

#create linear model
train_model_1 <- lm(formula= num_incidents_per_capita ~ num_of_guns_per_capita + num_of_guns_registered + num_incidents + n_injured + n_killed + n_guns_involved + population + state, data=train_1)

summary(train_model_1) #view our model


## -----------------------------------------------------------------------------
#must remove na values when creating lm
data_2 <- as_tibble(State_Info) %>% 
  drop_na(num_of_guns_per_capita) %>%
  drop_na(num_of_guns_registered) %>%
  drop_na(num_incidents_per_capita) %>%
  drop_na(population) %>%
  drop_na(num_incidents) %>%
  drop_na(n_injured) %>%
  drop_na(state)

#Partition into Train(75%) and Test(25%) sets
sample_2 <- createDataPartition(data_2$num_incidents_per_capita, p=0.75, list=FALSE)
train_2 <- data_2[sample_2, ]
test_2 <- data_2[-sample_2, ]

#create linear model
train_model_2 <- lm(formula= num_incidents_per_capita ~ num_of_guns_per_capita + num_of_guns_registered + n_injured + num_incidents + population + state, data=train_2)

summary(train_model_2)


## -----------------------------------------------------------------------------
#must remove na values when creating lm
data_3 <- as_tibble(State_Info) %>% 
  drop_na(num_of_guns_per_capita) %>%
  drop_na(num_of_guns_registered) %>%
  drop_na(num_incidents_per_capita) %>%
  drop_na(population) %>%
  drop_na(num_incidents) %>%
  drop_na(n_injured) %>%
  drop_na(state)

#Partition into Train(75%) and Test(25%) sets
sample_3 <- createDataPartition(data_3$num_incidents_per_capita, p=0.75, list=FALSE)
train_3 <- data_3[sample_3, ]
test_3 <- data_3[-sample_3, ]

#create linear model
train_model_3 <- lm(formula= num_incidents_per_capita ~ num_of_guns_per_capita + num_of_guns_registered + num_incidents + population + n_injured, data=train_3)

summary(train_model_3)#view our model


## -----------------------------------------------------------------------------
predictions_3 <- train_model_3 %>% predict(test_3)

#R2 is the sqare & root of the correlation (between 0[no correlation] and 1[perfect correlation])
#RMSE is the standard deviation of the predicted errors
#MAE mean absolute error of the distance between the predicted and actual value
errors_3 <- data.frame(R2 = R2(predictions_3, test_3$num_incidents_per_capita),
           RMSE = RMSE(predictions_3, test_3$num_incidents_per_capita),
           MAE = MAE(predictions_3, test_3$num_incidents_per_capita))

errors_3


## -----------------------------------------------------------------------------
ggplot(test_3, aes(x=predictions_3, y=num_incidents_per_capita), ) + geom_point()

