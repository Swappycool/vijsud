#__________________________________UBER CASE STUDY_________________________________

# Read the Uber dataset
uber<-read.csv("Uber request data.csv")

# Call the lubridate, ggplot2 & plyr functions for further actions
library(plyr)
library(ggplot2)
library(lubridate)

# Plot the hourwise STACKED BARS of frequency of trips

Uber_plot_1<- ggplot(uber, aes(x=factor(hour(parse_date_time(uber$Request.time, "HMS"))),
                     fill=factor(uber$Pickup.point)))+geom_bar(position="dodge")+
               labs(title= "HOUR wise Plot of Trip requests based on pickup point", 
                    x= "Hours of day", y= "No.of trip requests", fill= "PICK-UP POINT")

# Create a new column of TIME SLOT in uber 

uber$Time_Slot<-c("")

# To bin the Request Time into different Time slots
# first extract the hours from the Request Time

hours<-hour(parse_date_time(uber$Request.time, "HMS"))

# Create the suggested Bins

for(i in 1:nrow(uber))
{if(hours[i]>=0 && hours[i]<=5)
{uber$Time_Slot[i]<-c("Pre_morning")}
if(hours[i]>=6 && hours[i]<=10)
{uber$Time_Slot[i]<-c("Morning_Rush")}
if(hours[i]>=11 && hours[i]<=15)
{uber$Time_Slot[i]<-c("Day_Time")}
if(hours[i]>=16 && hours[i]<=20)
{uber$Time_Slot[i]<-c("Evening_Rush")}
if(hours[i]>=21 && hours[i]<=23)
{uber$Time_Slot[i]<-c("Late_Night")}
}

# Plot a bar chart for number of trips made during different time-slots 
Uber_plot2<-ggplot(uber, aes(x=factor(Time_Slot)))+geom_bar()
+labs(title="plot of no.of trips vs TIME SLOTS", 
      x="TIME SLOT", y= "No.of trips")

# Make a stacked bar chart where each bar represents a time slot and 
# the y-axisshows the frequency of requests. 
# Different proportions of bars should represent 
# the completed, cancelled and no cars available out of the total customer requests. (Hint: ggplot)

count(uber$Time_Slot)

Uber_plot_3<-ggplot(uber, aes(x=factor(Time_Slot), fill=factor(Status)))
             +geom_bar()+labs(title="No.of trips vs Time slot representing STATUS 
                  proportions", x="TIME SLOT", y= "No.of Requests", 
                  fill= "Status of trip")

# Problem 1 Visualised as :There are more complaints of "no cars available" 
#                       during the Evening_Rush slot (i.e., from 4 pm to 8pm) 

# Analysis of problem 1

# Step 1: Create a subset of Evening Data
evening_data<-subset(uber,uber$Time_Slot=="Evening_Rush" & uber$Status=="No Cars Available")

# Step 2 : To analyze using the plot, that the probelm 1 is more in which 
#          Pickup Point

Uber_plot_5<-ggplot(evening_data, aes(x=1, fill=factor(Pickup.point)))+
             geom_bar(position="fill")+scale_x_continuous(breaks= NULL)+
                labs(x="Evening_time_slot",y="No.of Requests with status 
                          \n NO CARS AVAILABLE", fill="Pickup Point" )
# Step 3 : Plot for percentage breakup for the total number of 
#          issues in this time slot based on the pick-up point.

evening_issue<-subset(uber, uber$Time_Slot=="Evening_Rush")

Uber_plot_4<-ggplot(evening_issue,aes(x=1,fill=factor(Status)))+
                geom_bar(position="fill")+facet_wrap(~Pickup.point)
                 +scale_x_continuous(breaks=NULL)+labs(x="Evening_Rush",
                  y="% of different issues", title= "Share of issues based on Pickup Point", fill= "ISSUES")

# Step 4 : To calculate the % of issues in different Pickup.Points

eve_Pickupwise_count<-count(evening_issue$Pickup.point)

evening_issue_share<-table(evening_issue$Status, evening_issue$Pickup.point)
for(i in 1:3)
{per_airport[i]<-(evening_issue_share[i,1]/eve_Pickupwise_count$freq[1])*100
 per_city[i]<-(evening_issue_share[i,2]/eve_Pickupwise_count$freq[2])*100
}

# Problem 2 Visualised as :The request made to uber drivers are mostly getting cancelled 
#                  during the Morning_Rush Time slot (i.e. from 6 am to 10 am)

# Analysis of problem 2

# Step 1: Create a subset of Morning Data
evening_data<-subset(uber,uber$Time_Slot=="Morning_Rush" & uber$Status=="Cancelled")

# Step 2 : To analyze using the plot, that the probelm 1 is more in which 
#          Pickup Point

Uber_plot_6<-ggplot(morning_data, aes(x=1, fill=factor(Pickup.point)))+
             geom_bar(position="fill")+scale_x_continuous(breaks= NULL)+
             labs(x="Morning_time_slot",y="No.of Requests with status 
                  \n CANCELLED trips", fill="Pickup Point" )

# Step 3 : Plot for percentage breakup for the total number of 
#          issues in this time slot based on the pick-up point.

morning_issue<-subset(uber, uber$Time_Slot=="Morning_Rush")

Uber_plot_7<-ggplot(morning_issue,aes(x=1,fill=factor(Status)))+
              geom_bar(position="fill")+facet_wrap(~Pickup.point)
              +scale_x_continuous(breaks=NULL)+labs(x="Morning_Rush",
              y="% of different issues", title= "Share of issues based on Pickup Point", fill= "ISSUES")

# Step 4 : To calculate the % of issues in different Pickup.Points

mor_Pickupwise_count<-count(morning_issue$Pickup.point)

morning_issue_share<-table(morning_issue$Status, morning_issue$Pickup.point)
for(i in 1:3)
{per_airport[i]<-(morning_issue_share[i,1]/mor_Pickupwise_count$freq[1])*100
per_city[i]<-(morning_issue_share[i,2]/mor_Pickupwise_count$freq[2])*100
}

#______________________________********___________________________________________
