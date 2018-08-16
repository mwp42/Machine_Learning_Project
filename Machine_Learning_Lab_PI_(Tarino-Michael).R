## Part I: Preprocessing and EDA
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

orders <- read.csv(file = "/Users/michaelwtarino/Desktop/Mike/Bootcamp/Week_7/Machine Learning Lab Part I/data/Orders.csv", stringsAsFactors =FALSE)
class(orders)
head(orders) 
dim(orders) #51290 rows by 24 columns
colnames(orders)

#Basic EDA
summary(orders)
sapply(orders, sd)
cor(orders)  #Error: 'x' must be numeric


### Problem 1: Dataset Import & Cleaning
class(orders$Profit)  ##factor and has $ dollar sign
class(orders$Sales)  ##factor and has $ dollar sign
orders$Profit <- gsub(",","",orders$Profit, fixed=TRUE)    #gets rid of commas
orders$Profit <- as.numeric(gsub("$","",orders$Profit, fixed=TRUE)) #gets rid of dollar sign and converts to numeric
orders$Sales <- gsub(",","",orders$Sales, fixed=TRUE)
orders$Sales <- as.numeric(gsub("$","",orders$Sales, fixed=TRUE))

#changes to numeric and gets rid of "," commas and $" dollar sign. 

orders$Order.Date
class(orders$Order.Date)
orders$Order.Date<-as.Date(orders$Order.Date, format = "%m/%d/%y") #lower case y
orders$Order.Date
### Problem 2: Inventory Management
#Part 2.1
#plot by time
orders_q<-orders%>%
  group_by(.,Order.Date)%>%
  summarise(.,Quantity=sum(Quantity))

ggplot(data = orders_q , aes(x = Order.Date, y=Quantity))+
  geom_point(col="red",pch = 17)+
  xlab("Order.Date")+
  ylab("Quantity")+
  ggtitle("Relationship between Order.Date and Quantity")

#plot by month
orders_q_monthly <- orders %>%
  group_by(.,Order.Date= strftime(Order.Date,"%y/%m")) %>%
  summarize(.,Quantity = sum(Quantity))

ggplot(data = orders_q_monthly , aes(x = Order.Date, y=Quantity))+
  geom_point(col="red",pch = 17)+
  xlab("Order.Date")+
  ylab("Quantity")+
  ggtitle("Relationship between Order.Date and Quantity")




#Part 2.2
#plot by time and facet for category
orders_c<-orders%>%
  group_by(.,Order.Date, Category)%>%
  summarise(.,Quantity=sum(Quantity))

ggplot(data = orders_c , aes(x = Order.Date, y=Quantity))+
  geom_point(col="red",pch = 17)+
  xlab("Order.Date")+
  ylab("Quantity")+
  facet_wrap(~Category)+
  ggtitle("Relationship between Month and Quantity")


#plot by month and facet for category
orders_c_monthly <- orders %>%
  group_by(.,Order.Date= strftime(Order.Date,"%y/%m"), Category) %>%
  summarize(.,Quantity = sum(Quantity))

ggplot(data = orders_c_monthly , aes(x = Order.Date, y=Quantity))+
  geom_point(col="red",pch = 17)+
  xlab("Order.Date")+
  ylab("Quantity")+
  facet_wrap(~Category)+
  ggtitle("Relationship between Month and Quantity")





### Problem 3: Why did customers make returns?
returns <- read.csv(file = "/Users/michaelwtarino/Desktop/Mike/Bootcamp/Week_7/Machine Learning Lab Part I/data/Returns.csv", stringsAsFactors =FALSE)

#Merge the **Returns** dataframe with the **Orders** dataframe using `Order.ID`.
ordersandreturns <- merge(orders,returns,by="Order.ID")
ordersandreturns[is.na(ordersandreturns)] <- "No"
colnames(ordersandreturns)
head(ordersandreturns)

#Part 3.1
ordersandreturns_loss <- ordersandreturns %>%
  group_by(year = strftime(Order.Date, "%y")) %>% 
  filter(Returned == "Yes",Profit <0) %>%
  summarize(.,Loss = sum(Profit))
ordersandreturns_loss

ggplot(data = ordersandreturns_loss , aes(x = year, y=Loss))+
  geom_point(col="red",pch = 17)+
  xlab("Order.Date")+
  ylab("Quantity")+
  ggtitle("Relationship between Year and Losses on Returns")

#Part 3.2
# For which customers returned things
ordersandreturns_customer <- ordersandreturns %>%
  group_by(Customer.Name) %>%
  filter(Returned == "Yes") %>%
  summarize(Returns = n())
ordersandreturns_customer

# How many customers returned 1,2,...etc times
returns_count <- ordersandreturns_customer %>%
  group_by(Returns) %>%
  summarize(Customers = n())
ret_count

ggplot(ret_count, aes(x=Returns, y=Customers)) + geom_bar(stat="identity")

#Part 3.3
ordersandreturns_region<-ordersandreturns %>%
  group_by(.,Region.x) %>%
  filter(.,Returned == "Yes") %>%
  summarise(.,Count = n())


ordersandreturns_region[order(ordersandreturns_region$Count, decreasing = TRUE),]

ggplot(ordersandreturns_region, aes(x=Region.x, y=Count)) + geom_bar(stat="identity")

#Part 3.4
ordersandreturns_category<-ordersandreturns %>%
  group_by(.,Sub.Category, Category) %>%
  filter(.,Returned == "Yes") %>%
  summarise(.,Count = n())
ordersandreturns_category

ordersandreturns_category[order(ordersandreturns_category$Count, decreasing = TRUE),]

ggplot(ordersandreturns_category, aes(x=Category, y=Count)) + geom_bar(stat="identity")
