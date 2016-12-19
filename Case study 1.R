#Checkpoint1
#Load companies and rounds2 files into dataframes from working directory and save it as a variable 
companies <- read.csv ("companies.txt",sep="\t",stringsAsFactors = FALSE)
rounds2 <- read.csv ("rounds2.csv",stringsAsFactors = FALSE)
# convert the common field in both the dataframes to same casefold
companies$permalink <- toupper(companies$permalink)
rounds2$company_permalink <- toupper(rounds2$company_permalink)
#Change the name of column 'Company_Permalink'in rounds2 dataframe to 'permalink' 
names(rounds2)[1] <- "permalink"
#To find the unique Number of companies in the rounds2 and companies dataframe
unique_rounds2 <- length(unique(rounds2$name))
unique_companies <- length(unique(companies$name))
#To know the unique key for the companies dataframe. 
#If the number of rows is equal to unique number of elements in a column
#Then that column  is considered as the unique key
nrow(companies)
length(unique(permalink))
#Merge the dataframes
master_frame <- merge(rounds2,companies,by="permalink",all.x=F)
# If the number of unique companies in master_frame and companies dataframe are same after inner merger
#then there are no companies in rounds2 file which is not present in companies file
unique_rounds2 <- length(unique(master_frame$name))
unique_companies <- length(unique(companies$name))




#Check Point2
# use sum function to calcualte the number of NA values in raised_amount_usd
#store the result in a variable
miss <- sum(is.na(master_frame$raised_amount_usd)==TRUE)
# To visualise the distribution of raised_amount_usd plot graphs
# If there are few outliers, use of mean for NA values is suggesed
plot(master_frame$raised_amount_usd)
boxplot(master_frame$raised_amount_usd)
# Subset the file whose value for raised_amount_usd is not NA and store it in a dataframe
file_without_missing <- subset(master_frame,master_frame$raised_amount_usd!="")
# Calculate the mean of the subsetted dataframe whose raised_amount_usd is not NA
mean_investment <- mean (file_without_missing$raised_amount_usd)
#Replace the mean in place of NA in master_frame dataset
master_frame$raised_amount_usd[which(is.na(master_frame$raised_amount_usd==T))] <- mean_investment
#Check the mean for raised_amount_usd after replacement of NA values
mean(master_frame$raised_amount_usd)


#Check point 3
#Find the mean of raised_amoun_usd  for funding_round_type venture
avg_venture <- mean(master_frame$raised_amount_usd[which(master_frame$funding_round_type=="venture")])
#Find the mean of raised_amoun_usd  for funding_round_type angel
avg_angel <- mean(master_frame$raised_amount_usd[which(master_frame$funding_round_type=="angel")])
#Find the mean of raised_amoun_usd  for funding_round_type seed
avg_seed <- mean(master_frame$raised_amount_usd[which(master_frame$funding_round_type=="seed")])
#Find the mean of raised_amoun_usd  for funding_round_type private_equity
avg_private_equity <- mean(master_frame$raised_amount_usd[which(master_frame$funding_round_type=="private_equity")])


#check point 4
#convert country_code data type from character to factor
master_frame$country_code <- as.factor(master_frame$country_code)
#checked for conversion of datatype
str(master_frame)
#load library plyr
require(plyr)
# check the frquency of country code
count(master_frame$country_code)
#As 'usa' is the (most frequently occurred) value of categorical factor country_code
#'usa' is replaced with all blank values of country_code 
most_frequent_country <- c("USA")
master_frame$country_code[which(master_frame$country_code=="")] <- NA
master_frame$country_code[which(is.na(master_frame$country_code==T))] <- most_frequent_country
count(master_frame$country_code)
#subset the master_frame consisting of venture type funding only
venture_table <- subset(master_frame,master_frame$funding_round_type=="venture")
#check the aggregate value of fund amount raised based on country code in the venture_table
venture_aggregate <- aggregate(venture_table$raised_amount_usd,by=list(venture_table$country_code),FUN = sum)
#View the top 9 countries who recevied highest investment in venture type funding and save it as variable
venture_descend <- (venture_aggregate[order(venture_aggregate$x,decreasing = T),])[1:9,]
#subset the venure_table so that only top 9 countries who recieved highest venture funding remains
#name the dataframe top9
top9 <- subset(venture_table,venture_table$country_code==venture_descend$Group.1)
#change the name of column of venture_descend
names(venture_descend)[1] <- "country_code"
names(venture_descend)[2] <- "Total investment_countrywise"
#Merge the top9 with venture_descend to add the column total investment_countrywise
top9 <- merge(top9,venture_descend,by="country_code",all.x=T)


#checkpoint 5
#Remove the characters after the primary category list and save it as a vector
cat_splitter <- gsub("\\|.*","",master_frame$category_list)
#Bind the primary category column with master_frame
master_frame <- cbind(master_frame,cat_splitter)
#check the frequency of primary category and save it as a variable
cat_split_freq <- count(master_frame$cat_splitter)
#load the mapping file into R
mapping_file <- read.csv("mapping_file.csv",stringsAsFactors = FALSE)
#check number of unique main sectors
unique(mapping_file$main_sector)
#Fill the blank cells in the primary category column with NA
master_frame$cat_splitter[which(master_frame$cat_splitter=="")] <- NA
#As the  percentage of NA values is more,it is suggested to replace it randomly than using mode
# set seed to generate the same random values every time the code runs
set.seed(3)
# To replace 3410 NA values in primary category
#Generate a vector of 3410 random primary category values and save it as a vector
catna_replace <- (sample(cat_split_freq$x,3410,replace=TRUE))
#Replace the NA values in primary category column
master_frame$cat_splitter[which(is.na(master_frame$cat_splitter==T))] <- catna_replace
#Again check for any NA values
xx <- count(master_frame$cat_splitter)
#Random replacement method chooses NA also from the list of primary categories
#Replace these NA values with the mode of primary category value
master_frame$cat_splitter[which(master_frame$cat_splitter=="")] <- "Biotechnology"
#Change the name of column of mapping file for merger with master frame
names(mapping_file)[1] <- "cat_splitter"
#Fill the blank cells in primary category column with NA
mapping_file$cat_splitter[which(mapping_file$cat_splitter=="")]<- NA
#Delete the rows in mapping file with NA values and save it 
x <- mapping_file[complete.cases(mapping_file),]
#Merge the files to adding main sector column to master_frame
master_frame <- merge(master_frame,x,by="cat_splitter",all.x=T)
#Assign the NA values in main sector to others category
masterframe_mainsec_na <- c("Others")
master_frame$main_sector[which(is.na(master_frame$main_sector==T))] <- masterframe_mainsec_na


#checkpoint6
#subset the dataframe on the basis of country code,funding type and amount raised.
#Top three countries who received top investment count in venture funding with investment amount between
#5 and 15 million are subsetted to a dataframe
e <- subset(master_frame,(master_frame$country_code=="USA" & master_frame$funding_round_type=="venture" & master_frame$raised_amount_usd >= 5000000))
d1 <- subset(e,(e$raised_amount_usd <= 15000000))
f <- subset(master_frame,(master_frame$country_code=="GBR" & master_frame$funding_round_type=="venture" & master_frame$raised_amount_usd >= 5000000))
d2 <- subset(f,(f$raised_amount_usd <= 15000000))
g <- subset(master_frame,(master_frame$country_code=="IND" & master_frame$funding_round_type=="venture" & master_frame$raised_amount_usd >= 5000000))
d3 <- subset(g,(g$raised_amount_usd <= 15000000))

#Count the number of investments in 08 main sectors in the country USA and save it
mainsec_investcount <- count(d1$main_sector)
#change the name of main sector column for merging 
names(mainsec_investcount)[1] <- "main_sector"
#merge the count of investment column with d1(USA) dataframe using left merge
d1 <- merge(d1,mainsec_investcount,by="main_sector",all.x=T)
#change the name of merged column for better understanding
names(d1)[18] <- "mainsec_investcount"
#Aggregate the invsetment amount in d1 dataframe on the basis of main sectors and save it
d1_mainsec_invest <- aggregate(d1$raised_amount_usd,by=list(d1$main_sector),FUN=sum)
# Change the name of columns of aggregated dataframe
names(d1_mainsec_invest)[1] <- "main_sector"
names(d1_mainsec_invest)[2] <- "mainsec_Totalinvestment"
#Merge the aggregated dataframe with d1 dataframe
d1 <- merge(d1,d1_mainsec_invest,by="main_sector",all.x=T)

#Count the number of investments in 08 main sectors in the country GBR and save it
mainsec2_investcount <- count(d2$main_sector)
#change the name of main sector column for merging
names(mainsec2_investcount)[1] <- "main_sector"
#merge the count of investment column with d2(GBR) dataframe using left merge
d2 <- merge(d2,mainsec2_investcount,by="main_sector",all.x=T)
#change the name of merged column for better understanding
names(d2)[18] <- "mainsec_investcount"
#Aggregate the invsetment amount in d2 dataframe on the basis of main sectors and save it
d2_mainsec_invest <- aggregate(d2$raised_amount_usd,by=list(d2$main_sector),FUN=sum)
# Change the name of columns of aggregated dataframe
names(d2_mainsec_invest)[1] <- "main_sector"
names(d2_mainsec_invest)[2] <- "mainsec_Totalinvestment"
#Merge the aggregated dataframe with d2 dataframe
d2 <- merge(d2,d2_mainsec_invest,by="main_sector",all.x=T)

#Count the number of investments in 08 main sectors in the country IND and save it
mainsec3_investcount <- count(d3$main_sector)
#change the name of main sector column for merging
names(mainsec3_investcount)[1] <- "main_sector"
#merge the count of investment column with d3(IND) dataframe using left merge
d3 <- merge(d3,mainsec3_investcount,by="main_sector",all.x=T)
#change the name of merged column for better understanding
names(d3)[18] <- "mainsec_investcount"
#Aggregate the invsetment amount in d3 dataframe on the basis of main sectors and save it
d3_mainsec_invest <- aggregate(d3$raised_amount_usd,by=list(d3$main_sector),FUN=sum)
# Change the name of columns of aggregated dataframe
names(d3_mainsec_invest)[1] <- "main_sector"
names(d3_mainsec_invest)[2] <- "mainsec_Totalinvestment"
#Merge the aggregated dataframe with d3 dataframe
d3 <- merge(d3,d3_mainsec_invest,by="main_sector",all.x=T)


#------------------------------------------------------------------------------------------------------------------------------------------
#code for answering questions in excel worksheet
# Total number of Investments in d1
ques_6.1.1 <- sum(mainsec_investcount$freq)
ques_6.1.1

#Total number of Investments in d2
ques_6.1.2 <- sum(mainsec2_investcount$freq)
ques_6.1.2

#Total number of Investments in d3
ques_6.1.3 <- sum(mainsec3_investcount$freq)
ques_6.1.3

#Total amount of Investment in d1
ques_6.2.1 <- sum(d1_mainsec_invest$mainsec_Totalinvestment)
ques_6.2.1

#Total amount of Investment in d2
ques_6.2.2 <- sum(d2_mainsec_invest$mainsec_Totalinvestment)
ques_6.2.2

#Total amount of Investment in d3
ques_6.2.3 <- sum(d3_mainsec_invest$mainsec_Totalinvestment)
ques_6.2.3

#To subset the top sector in d1 
#To check the max raised amount usd
topsectorcompany_d1 <- subset(d1,d1$main_sector=="Others")
max(topsectorcompany_d1$raised_amount_usd)


#To subset the top sector in d2 
#To check the max raised amount usd
topsectorcompany_d2 <- subset(d2,d2$main_sector=="Others")
max(topsectorcompany_d2$raised_amount_usd)

#To subset the top sector in d3
#To check the max raised amount usd
topsectorcompany_d3 <- subset(d3,d3$main_sector=="Others")
max(topsectorcompany_d3$raised_amount_usd)

#To subset the second top sector in d1
#To check the max raised amount usd
secondtopsectorcompany_d1 <- subset(d1,d1$main_sector=="Social, Finance, Analytics, Advertising")
max(secondtopsectorcompany_d1$raised_amount_usd)


#To subset the second top sector in d2
#To check the max raised amount usd
secondtopsectorcompany_d2 <- subset(d2,d2$main_sector=="Social, Finance, Analytics, Advertising")
max(secondtopsectorcompany_d2$raised_amount_usd)

#To subset the second top sector in d3
#To check the max raised amount usd
secondtopsectorcompany_d3 <- subset(d3,d3$main_sector=="Social, Finance, Analytics, Advertising")
max(secondtopsectorcompany_d3$raised_amount_usd)
unique(is.na(secondtopsectorcompany_d3$cat_splitter))

#-----------------------------------------------------------------------------------------------------------



#checkpoint 7
#For question 7.1
#load the ggplot2 package
library(ggplot2)
require(ggplot2)

# Aggregate the investment amount raised globally based on funding type and save it as dataframe
pie1 <- aggregate (master_frame$raised_amount_usd,by=list(master_frame$funding_round_type),FUN=sum)
#Add the venture,seed,private_equity investment amount and subtract it from total 
sum(pie1$x)-sum(645034110791, 89154226845, 145564413503)
#create a dataframe with Different investment types and their total investment
pie1.df <- data.frame(Investment = c("venture","seed","private_equity","All other methods"), Amount=c(645034110791, 89154226845, 145564413503,318805451993),Average=c(11623493,2920791,63704339,""),Funding=c("645 Billion","89 Billion","145 Billion",""))
#create a vector of average investments of venture,seed,private_equity 
Avg_investments <- c("(AVG-11623493)","(AVG-2920791)","(AVG-63704339)","")
#combine Investment type with Avg investments for depicting it in pie chart
pie1.df$Investment <- paste(pie1.df$Investment,Avg_investments)
# Draw a pie chart of funding methods.Choose a plot which is more suitable between plot1,plot1.1,plot1.2
pie(pie1.df$Amount,label=(pie1.df$Investment), init.angle = 90,clockwise=TRUE,main="Total Investments by Different Funding Methods with Average",col = rainbow(length(pie1.df$Amount)))
pie_bar_chart <- ggplot(pie1.df,aes(x=Average,y=Amount,fill=Investment))+geom_bar(width = 1,stat="identity")
plot1.2 <- pie_bar_chart+coord_polar(theta="x",start=0,direction=-1)+labs(title="Total Investments by Different Funding Methods")

pie_bar_chart2 <- ggplot(pie1.df,aes(x="",y=Amount,fill=Investment))+geom_bar(width = 1,stat="identity")
plot1.1 <- pie_bar_chart2+coord_polar(theta="y",start=0,direction=-1)+labs(title="Total Investments by Different Funding Methods")

# Code for increasing the aesthetics of piechart like adding title,darkening the outline
#Removing outline from legend,removing axis ticks,titles etc and adding pie slice label
plot1 <- ggplot(pie1.df, aes(x=1, y=Amount, fill=Investment)) +
  ggtitle("Total Investments by Different Funding Methods") +
  # black border around pie slices
  geom_bar(stat="identity", color='black') +
  # remove black diagonal line from legend
  guides(fill=guide_legend(override.aes=list(colour=NA))) +
  # Plot a pie chart
  coord_polar(theta='y') +
  # label aesthetics
  theme(axis.ticks=element_blank(),  # the axis ticks
        axis.title=element_blank(),  # the axis labels
        axis.text.y=element_blank(), # the text labels
        axis.text.x=element_text(color='black')) +
  # pie slice labels
  scale_y_continuous(
    breaks=cumsum(pie1.df$Amount) - pie1.df$Amount/2,
    labels=pie1.df$Funding
  )


#For question 7.2
#code for creating a bar chart of total investments through venture funding received by top 9 countries
#Reorder for depicting the graph in descending order
names(venture_descend)[2] <- "total_investment"
top9_bar <- ggplot(venture_descend,aes(x=reorder(country_code,-total_investment),y=total_investment))+ggtitle("Top 9 countries by Total Investment (venture)")
plot2 <- top9_bar+geom_bar(stat="identity",fill="blue")

#For question 7.3
#subset the top 3 main sectors and their number of investments of top 3 countries to a dataframe
mainsec_investcount_descend <- (mainsec_investcount[order(mainsec_investcount$freq,decreasing = T),])[1:3,]
mainsec2_investcount_descend <- (mainsec2_investcount[order(mainsec2_investcount$freq,decreasing = T),])[1:3,]
mainsec3_investcount_descend <- (mainsec3_investcount[order(mainsec3_investcount$freq,decreasing = T),])[1:3,]

#create a merged file of Top 3 countries with count of investments in top 3 sectors
mainmerge_investcount_descend <- merge(mainsec_investcount_descend,mainsec2_investcount_descend,by="main_sector",all.x=T)
mainmerge1_investcount_descend <- merge(mainmerge_investcount_descend,mainsec3_investcount_descend,by="main_sector",all=T)
#change the column names of merged dataframe
names(mainmerge1_investcount_descend)[2]<-"d1"
names(mainmerge1_investcount_descend)[3]<-"d2"
names(mainmerge1_investcount_descend)[4]<-"d3"

#Grouped bar charts can't be created with wide type data frames
#For converting wide type to long type data frame install "tidyr" and "R6" packages
#load both packages
install.packages("tidyr")
install.packages("R6")
require("tidyr")
require("R6")
#convert wide type to long type dataframe using gather function
data_long <- gather(mainmerge1_investcount_descend,country,count,d1,d2,d3)
# For depicting the graph, order the main sectors
data_long <- within(data_long,main_sector<-factor(main_sector,levels=names(sort(table(main_sector),increase = TRUE))))
#Plot the grouped bar chart for Top 3 countries in Top 3 sectors
data_graph <- ggplot(data_long,aes(x=country, y=count, fill=factor(main_sector)))+ggtitle("Top 3 sectors in  Top3 countries")
plot3 <- data_graph+geom_bar(stat="identity",position="stack")




