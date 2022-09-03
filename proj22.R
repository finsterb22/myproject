
install.packages ("tidyr", dependencies = T)
install.packages ("stringr")
install.packages ("dplyr")


library(tidyr)
library( dplyr)
library(DescTools)
library(stringr)
library(ggplot2)

data <- read.csv("TransportCSOfile.csv")
new.data <- as.data.frame (data)

#read data as a tibble so it can be viewed more easily on the console
new.data <-  as_tibble(new.data)
df <- new.data




####explore data####
head(df)      #This function will returns the first 6 rows of the dataframe  
str(df)       #The function  will provide structure of the data, We can see that most of the data in the variables are integers



###Tidying data####



##Shorten column names

names(new.data) #get the names of the columns - we can see that the names are very long  in columns 10 to 40 and difficult to view the column names in the tibble

#use dplyr function to rename columns to make them more readable
df <- rename(new.data, on_foot = Population_Aged_5_Over_By_Means_Of_Travel_To_Work_School_College_On_Foot_2011, 
Bicycle= Population_Aged_5_Over_By_Means_Of_Travel_To_Work_School_College_Bicycle_2011,
Bus_Minibus_Coach= Population_Aged_5_Over_By_Means_Of_Travel_To_Work_School_College_Bus_Minibus_Coach_2011,
Train_Dart_Luas = Population_Aged_5_Over_By_Means_Of_Travel_To_Work_School_College_Train_Dart_Luas_2011,
Motorcycle_Scooter = Population_Aged_5_Over_By_Means_Of_Travel_To_Work_School_College_Motorcycle_Scooter_2011,
Car_Driver = Population_Aged_5_Over_By_Means_Of_Travel_To_Work_School_College_Car_Driver_2011,                        
Car_Passenger = Population_Aged_5_Over_By_Means_Of_Travel_To_Work_School_College_Car_Passenger_2011,                      
Van = Population_Aged_5_Over_By_Means_Of_Travel_To_Work_School_College_Van_2011,                                
Other = Population_Aged_5_Over_By_Means_Of_Travel_To_Work_School_College_Other_2011,                              
Soft_Modes = Population_Aged_5_Over_By_Means_Of_Travel_To_Work_School_College_Soft_Modes_Comb_2011,                    
Public_Transport = Population_Aged_5_Over_By_Means_Of_Travel_To_Work_School_College_Public_Transport_Comb_2011,              
Private_Transport = Population_Aged_5_Over_By_Means_Of_Travel_To_Work_School_College_Private_Transport_Comb_2011,             
travel_Mode_Total= Population_Aged_5_Over_By_Means_Of_Travel_To_Work_School_College_Total_2011,                              
Departure_Time_Before_0630 = Population_Aged_5_Over_By_Time_Leaving_Home_To_Travel_To_Work_School_College_Before_0630_2011,            
Departure_Time_0630_0700 = Population_Aged_5_Over_By_Time_Leaving_Home_To_Travel_To_Work_School_College_0630_0700_2011,              
Departure_Time_0701_0730 = Population_Aged_5_Over_By_Time_Leaving_Home_To_Travel_To_Work_School_College_0701_0730_2011,              
Departure_Time_0731_8000 = Population_Aged_5_Over_By_Time_Leaving_Home_To_Travel_To_Work_School_College_0731_8000_2011,              
Departure_Time_0801_0830= Population_Aged_5_Over_By_Time_Leaving_Home_To_Travel_To_Work_School_College_0801_0830_2011,              
Departure_Time_0831_0900= Population_Aged_5_Over_By_Time_Leaving_Home_To_Travel_To_Work_School_College_0831_0900_2011,              
Departure_Time_0901_0930= Population_Aged_5_Over_By_Time_Leaving_Home_To_Travel_To_Work_School_College_0901_0930_2011,              
Departure_Time_After_0930= Population_Aged_5_Over_By_Time_Leaving_Home_To_Travel_To_Work_School_College_After_0930_2011,             
Departure_Time_Not_Stated= Population_Aged_5_Over_By_Time_Leaving_Home_To_Travel_To_Work_School_College_Not_Stated_2011,             
Departure_Time_Total= Population_Aged_5_Over_By_Time_Leaving_Home_To_Travel_To_Work_School_College_Total_2011,            
Travel_Time_Under_15_mins= Population_Aged_5_Over_By_Journey_Time_To_Work_School_College_Under_15_mins_2011,                        
Travel_Time_Quarter_To_Under_Half_Hour= Population_Aged_5_Over_By_Journey_Time_To_Work_School_College_Quarter_To_Under_Half_Hour_2011,           
Travel_Time_Half_Hour_To_Under_Three_Quarter_Hours= Population_Aged_5_Over_By_Journey_Time_To_Work_School_College_Half_Hour_To_Under_Three_Quarter_Hours_2011,
Travel_Time_Three_Quarter_Hours_To_Under_One_Hour= Population_Aged_5_Over_By_Journey_Time_To_Work_School_College_Three_Quarter_Hours_To_Under_One_Hour_2011, 
Travel_Time_One_Hour_To_Under_One_Hour_Thirty_Mins = Population_Aged_5_Over_By_Journey_Time_To_Work_School_College_One_Hour_To_Under_One_Hour_Thirty_Mins_2011,
Travel_Time_One_And_Half_Hours_And_Over = Population_Aged_5_Over_By_Journey_Time_To_Work_School_College_One_And_Half_Hours_And_Over_2011,           
Travel_Time_Not_Stated = Population_Aged_5_Over_By_Journey_Time_To_Work_School_College_Not_Stated_2011,                           
Travel_Time_Total= Population_Aged_5_Over_By_Journey_Time_To_Work_School_College_Total_2011  )

names(df)# Check output
head(df) # easier to read column names in the tibble 



##replacing NAs


names(which(colSums(is.na(df))>0)) #which columns are there missing values or NAs
colSums(is.na(df)) #check how many NAs or missing values in each column

# we know that there are 18 NAs in the planning.region column, check where the NAs are in planning region by viewing the dataframe
View(df)

#we can replace NAs with either North west or eastern and midlands depending on which county is in the same row
df$Planning.Region [with(df, County== "Donegal"  & is.na(Planning.Region))] <- 'North and West'
df$Planning.Region [with(df, County== "Dublin City"  & is.na(Planning.Region))] <- 'Eastern and Midlands'


#check where NAs are in travel_Mode_Total column - we can see that data code 75 has a blank value
#we can replace this blank with the sum of the transport types
df101 <- df %>% filter(df$Datacode == 75) %>% summarise(sum(Other), sum(Soft_Modes), sum(Public_Transport), sum(Private_Transport))
fn1 <- as.integer(rowSums(df101 )) #total of these is 208

df$travel_Mode_Total[which(is.na(df$travel_Mode_Total))] <- fn1 #replaces NA with 208


#check where NAs are in Departure_Time_After_0930 column - we can see that data code 11 has a blank
#we can replace this blank with the value of the Travel_Time_Total less all other travel times in this row
df102 <- df %>% filter(df$Datacode == 11) %>% 
  summarise(sum(Departure_Time_Before_0630), sum( Departure_Time_0630_0700 ), sum(Departure_Time_0701_0730), sum(Departure_Time_0731_8000), sum(Departure_Time_0801_0830), sum(Departure_Time_0831_0900), sum(Departure_Time_0901_0930), sum(Departure_Time_Not_Stated))
fn2 <- as.integer(rowSums(df102 )) # total of these rows is 59

fn3 <-  df %>% 
  filter(df$Datacode == 11) %>% 
  summarise(sum(Departure_Time_Total)) #value of the total departure time for datacode 11 is 93

                                                       
fn4 <- as.integer(fn3 - fn2) #34 is the value of the Travel_Time_Total less all other travel times in this row

df$Departure_Time_After_0930[which(is.na(df$Departure_Time_After_0930))] <- fn4 #replace the NA with 34

names(which(colSums(is.na(df))>0)) #final check if any NAs 


##replace South with Southern
#In the planning region column we can see that one value has "South" instead of "Southern"

df <- df %>% 
  mutate(Planning.Region = recode(Planning.Region, 'South'= 'Southern')) #replace South with Southern
unique(df$Planning.Region) # check output - there should be only three Planning regions



##Delete Columns
#Having reviewed the questions it was decided that these columns were not necessary to answer the questions
df = select(df, -c(NUTS_II,SA_NAME,GEOGID,Electoral.Division.CSO.Code)) 





####Q1 What is the most popular mode of transport nationally? ####

#Assuming mode of transport is Other, soft, public & private

totaltravelmode <- sum(df$travel_Mode_Total) #Total population of travel mode variable

other = sum(df$Other)/totaltravelmode*100       #proportion of other travel mode as a percentage of total travel mode
softmode = sum(df$Soft_Modes)/totaltravelmode*100
publicmode = sum(df$Public_Transport)/totaltravelmode*100
privatemode = sum(df$Private_Transport)/totaltravelmode*100
round(other,digits = 0)   #6% use "other" mode of transport nationally
round(softmode,digits = 0) #17% use Soft mode of transport nationally
round(publicmode,digits = 0) #13% use public mode of transport nationally
round(privatemode,digits = 0) #63% use private mode of transport nationally

#Private mode of transport is the most popular with 63% 

#We can visualise the counties that use private transport the most from left to right on the barplot
df100 <- df %>% 
  group_by(County) %>% 
  summarise(Private_Transport = sum(Private_Transport)) %>% 
  arrange(desc(Private_Transport))

barplot(df100$Private_Transport, names.arg =df100$County, col =c("green","gold"), xlab = "County", ylab = "Private Transport", las = 2, cex.names = 0.7)
#we can see that Cork county has the highest population of commuters that use private transport, followed by Dublin City with Leitrim having the lowest population of private commuters

#A follow up question might be what type of transport is used the most by commuters that use Private transport.
Carpassenger = sum(df$Car_Passenger)/sum(df$Private_Transport)*100 
Cardriver = sum(df$Car_Driver)/sum(df$Private_Transport)*100 

round(Carpassenger + Cardriver ,digits = 0)# we can see that 93% of private commuters use a Car to get to work, college or school

#Further analysis could be performed if we had previous years data to see if the number of commuters using the car has increased or decreased.



####Q2 How does this (Answer to Q1) compare to the most popular mode of transport in your assigned county - Kerry####

#create a new dataframe for Kerry only
df1 <- df %>% 
  filter( County == "Kerry") %>% 
  select(Other,Soft_Modes,Public_Transport,Private_Transport,travel_Mode_Total)
df1

totaltravelmodekerry <- sum(df1$travel_Mode_Total) #Total population of travel mode variable for County Kerry


a= sum(df1$Other)/totaltravelmodekerry*100
b= sum(df1$Soft_Modes)/totaltravelmodekerry*100
c= sum(df1$Public_Transport)/totaltravelmodekerry*100
d= sum(df1$Private_Transport)/totaltravelmodekerry*100
round(a,digits = 0)
round(b,digits = 0)
round(c,digits = 0)
round(d,digits = 0)

#Private mode of transport is the most popular in Kerry with 70%


#It might be interesting to see what type of transport is the most popular in Kerry
df11 <- df %>% 
  filter( County == "Kerry") %>% 
  select(Car_Driver,Car_Passenger)
df11 #create a dataframe for car commuters for Kerry only

Carpassengerkerry = sum(df11$Car_Passenger)/sum(df1$Private_Transport)*100 
Cardriverkerry = sum(df11$Car_Driver)/sum(df1$Private_Transport)*100 

round(Carpassengerkerry + Cardriverkerry ,digits = 0) #91% of commuters use the car to get to work, school or college



#We can visualise the top 5 electoral divisions that use private transport in county Kerry if we need to get more insights into the data
df300 <- df %>% 
  filter( County == "Kerry") %>% 
  group_by(Electoral.Division.Name) %>% 
  summarise(Private_Transport = sum(Private_Transport)) %>%
  arrange(desc(Private_Transport)) %>% 
  top_n(5)

barplot(df300$Private_Transport, names.arg =df300$Electoral.Division.Name, col =c("green","gold"), xlab = "Electoral Division", ylab = "Private Transport", las = 1, cex.names = 0.8)

#The barplot shows us that Tralee Rural has the highest number of commuters that us private transport followed by Killarney Rural, Killarney Urban, Killorglin and Listowel



####Q3 What differences are evident between the choice of transportation in the cities compared to the other regions?####

#dataframe containing city data only
df2 <- df %>% 
  filter( grepl("City", County)) %>% 
  select(Other,Soft_Modes,Public_Transport,Private_Transport,travel_Mode_Total)
df2

totaltravelmodecity <- sum(df2$travel_Mode_Total) #total travel mode for cities only

e= sum(df2$Other)/totaltravelmodecity*100   #proportion of other travel mode as a percentage of total travel mode in the cities
f= sum(df2$Soft_Modes)/totaltravelmodecity*100
g= sum(df2$Public_Transport)/totaltravelmodecity*100
h= sum(df2$Private_Transport)/totaltravelmodecity*100
round(e,digits = 0) #6% use "other" mode of transport in the cities
round(f,digits = 0) #33% use Soft mode of transport in the cities
round(g,digits = 0) #18% use Public mode of transport in the cities
round(h,digits = 0) #43% use Private mode of transport in the cities


#dataframe not containing city data
df3 <- df %>% 
  filter(!grepl("City", County )) %>% 
  select(Other,Soft_Modes,Public_Transport,Private_Transport,travel_Mode_Total)
df3

totaltravelmodenotcity <- sum(df3$travel_Mode_Total) #total travel mode excluding cities 
totaltravelmodenotcity

i= sum(df3$Other)/totaltravelmodenotcity*100
j= sum(df3$Soft_Modes)/totaltravelmodenotcity*100
k= sum(df3$Public_Transport)/totaltravelmodenotcity*100
l= sum(df3$Private_Transport)/totaltravelmodenotcity*100
round(i,digits = 0) #7% use "other" mode of transport outside of the cities
round(j,digits = 0) #14% use Soft mode of transport outside of the cities
round(k,digits = 0) #12% use Public mode of transport outside of the cities
round(l,digits = 0) #67% use Private mode of transport outside of the cities

#We can see that in the cities more soft modes of transport are uses, 33% in cities v 14% outside of cities and 43% of people use private transport in the city v 67% outside the cities



####Q4 What proportion of commuters leave home outside of the 8-9am rush hour?####

#sum of the departure times except for the periods between 8-9am 
df4 <- df %>% 
  select(Departure_Time_Before_0630:Departure_Time_0731_8000,Departure_Time_0901_0930,Departure_Time_After_0930,Departure_Time_Not_Stated)
ans <- sum(df4)

#sum of the total departure time column
ans1 <- sum(df$Departure_Time_Total) 
ans1

#proprtion of commuter outside of 8-9am
ans2= (ans/ans1)*100
round(ans2,digits = 0) #55% of commuters leave home outside of the 8-9am rush hour




####Q5 Are commuters in your assigned county likely to travel for longer than 45 minutes each morning?####

#sum of time greater than 45 Minutes (Assuming that the not stated column is not greater than 45 minutes)
df6 <- df %>% 
  filter( County == "Kerry") %>% 
  select(Travel_Time_Three_Quarter_Hours_To_Under_One_Hour:Travel_Time_One_And_Half_Hours_And_Over)
ans3 <- sum(df6)
ans3


#sum of the total travel time column
df7 <-df %>%
  filter( County == "Kerry") %>% 
  select(Travel_Time_Total)

ans4 <-  sum(df7) 
ans4

#proportion of commutes greater than 45 minutes
ans5= (ans3/ans4)*100
round(ans5,digits = 0)

# 8% of commuters in Kerry are likely to travel for longer than 45 minutes each morning



####Q6 How does this (Answer to Q5) compare to other counties in the same NUTS III region?####

# Cork county is the only other county in my NUTS III region (assuming this meant excluding city data)
df8 <- df %>% 
  filter( County == "Cork County" ) %>% 
  select(Travel_Time_Three_Quarter_Hours_To_Under_One_Hour:Travel_Time_One_And_Half_Hours_And_Over)
ans6 <- sum(df8)
ans6



#sum of the total travel time column for Cork and Cork City
df9 <-df %>%
  filter( County == "Cork County") %>% 
  select(Travel_Time_Total)

ans7 <-  sum(df9) 
ans7

ans8= (ans6/ans7)*100 
round(ans8,digits = 0)#11% of commutes are greater than 45 minutes in Cork county

round(ans8-ans5)#difference between Cork and Kerry commuters travelling longer than 45 minutes

#11% of commuters from Cork travel longer than 45 minutes each morning, which is 3% more versus commuters in Kerry 



####Q7 The residents of which five counties experience the longest commute times?####

#create a dataframe for the counties that travel longer then one and a half hours
df10 <- df %>% 
    group_by(County) %>% 
  summarise(Travel_Time_One_And_Half_Hours_And_Over = sum(Travel_Time_One_And_Half_Hours_And_Over)) %>% 
  arrange(desc(Travel_Time_One_And_Half_Hours_And_Over)) %>% 
  top_n(5)

df10

#We can visualise the top 5 counties where residents experience the longest commute times
barplot(df10$Travel_Time_One_And_Half_Hours_And_Over, names.arg =df10$County, col =c("green","gold"), xlab = "County", ylab = "Number of Journeys with longest commute time", las = 1, cex.names = 0.6)

#Fingal, Meath, Kildare, Dublin City and Cork County are the counties that experience the longest commute times as represented in the bar plot



####Q8 What proportion of cars used in the morning commute contain only one person?####


ans9 <- sum(df[, 'Car_Driver'])
ans10 <- sum(df[, 'Car_Passenger'])
ans11 <- ans9-ans10 #Assuming that there was one car passenger in each car surveyed 
ans12 <- (ans11/ans9)*100

round(ans12,digits = 0)

#Assuming that there was one car passenger in each car surveyed, then 55% of cars contain only one person 



####Q9 Which Electoral Division within each Planning Region do you propose should be prioritised for investment in public transportation?####

df200 <- df %>% 
  group_by(Planning.Region,Electoral.Division.Name, ) %>%
      summarise(Private_Transport = sum(Private_Transport)) %>% 
    top_n(1)
df200
barplot(df200$Private_Transport, names.arg =df200$Electoral.Division.Name, col =c("green","yellow","blue"), xlab = "Electoral Division", ylab = "Number of Private Transport commuters", las = 1, cex.names = 0.7, )

#Based on the data the electoral divisions with the highest level of private Transport in each planning region are Blanchardstown-Blakestown, Bearna and Douglas. 
#I would priortise these electoral divisions for investment in public transportation as they have the highest number of commuters using private transport.

