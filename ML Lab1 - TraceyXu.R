library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)


Orders <- read.table("./Orders.csv",head = T, sep = ',')
Returns <- read.table("./Returns.csv",head = T, sep=',')
summary(Orders)


Orders$Profit<-gsub(",","",Orders$Profit, fixed = TRUE)
Orders$Profit<-as.numeric(gsub("$","",Orders$Profit,fixed = TRUE))

Orders$Sales<-gsub(",","",Orders$Sales, fixed = TRUE)
Orders$Sales<-as.numeric(gsub("$","",Orders$Sales,fixed = TRUE))

#1. Is there any seasonal trend of inventory in the company?
# Yes
#2. Is the seasonal trend the same for different categories?
# ***Hint:*** For each order, it has an attribute called `Quantity` that indicates the number of product in the order. If an order contains more than one product, there will be multiple observations of the same order.
# No. Technology and Funiture have larger volatility in avg month numbers, office supplies is different
Orders$Order.Date<-as.Date(Orders$Order.Date,"%m/%d/%y")
Orders$Ship.Date<-as.Date(Orders$Ship.Date,"%m/%d/%y")
Orders<-Orders[order(Orders$Order.Month),]
plot(Orders$Order.Date,Orders$Quantity)
Orders$Order.Year<-format(as.Date(Orders$Order.Date, format="%d/%m/%Y"),"%Y")
Orders$Ship.Year<-format(as.Date(Orders$Order.Date, format="%d/%m/%Y"),"%Y")
Orders$Order.Month<-format(as.Date(Orders$Order.Date, format="%d/%m/%Y"),"%Y/%m")
Orders$Ship.Month<-format(as.Date(Orders$Order.Date, format="%d/%m/%Y"),"%Y/%m")


Orders<-Orders[order(Orders$Order.Year,Orders$Order.Month),]
Orders_q = Orders %>%
    group_by(., Category,Order.Month) %>%
    summarise(., Quantity=sum(Quantity))

ggplot(data=Orders_q, aes(x=Order.Month, y=Quantity))+
  geom_point(col="red",pch=17)+
  xlab("Date")+
  ylab("Quantity")+
  facet_wrap(~Category)

# Orders_q$Growth <- with(Orders_q, ave(Category, Quantity, 
#                                       FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))

Orders_m = Orders %>%
  group_by(., Category,Order.Month) %>%
  summarise(., Quantity=mean(Quantity))

ggplot(data=Orders_m, aes(x=Order.Month, y=Quantity))+
  geom_point(col="blue",pch=17)+
  xlab("Date")+
  ylab("Avg Quantity")+
  facet_wrap(~Category)

#question3
Orders.Returns<-merge(Orders,Returns,by="Order.ID")

#Orders.addReturns<-merge(Orders,Returns,by="Order.ID",all=TRUE)

#3.1. How much profit did we lose due to returns each year?
Loss.table = Orders.Returns[ Orders.Returns$Profit>0,]
Loss.table = Loss.table %>%
  group_by(.,Order.Year) %>%
  summarise(Loss=sum(Profit))

#2012:23250.
#2013:16983.
#2014:24950.
#2015:33021.

#3.2. How many customer returned more than once? more than 5 times?
#Orders.Returns.Unique = Orders.Returns[!duplicated(Orders.Returns$Order.ID),]

Customer.table = Orders.Returns%>%
  select(Customer.Name, Order.Date) %>%
  group_by(.,Customer.Name) %>%
  summarise(Count=n()) 

nrow(Customer.table[Customer.table$Count>1,])#448 customer return more than once
nrow(Customer.table[Customer.table$Count>5,])#124 customer retrun more than 5 times

#3.3. Which regions are more likely to return orders?
Orders.addReturns<-merge(Orders,Returns,by="Order.ID",all=TRUE)
Region.table1 = Orders.addReturns%>%
  filter(Returned=="Yes") %>%
  group_by(.,Region.x) %>%
  summarise(Count=n()) 

Region.table2 = Orders.addReturns%>%
  group_by(.,Region.x) %>%
  summarise(Count=n()) 

Region.table = merge(Region.table1,Region.table2, by="Region.x")

Region.table$Rate = Region.table$Count.x/Region.table$Count.y

Region.table %>%
  top_n(n = 3) 

#Eastern Asia,Eastern Asia,Western US are mostly likely to return

#3.4. Which categories (sub-categories) of products are more likely to be returned?
#***Hint:*** Merge the **Returns** dataframe with the **Orders** dataframe using `Order.ID`.

Category.table11 = Orders.addReturns%>%
  filter(Returned=="Yes") %>%
  group_by(.,Category, Sub.Category) %>%
  summarise(Count = n()) 

Category.table22 = Orders.addReturns%>%
  group_by(.,Category, Sub.Category) %>%
  summarise(Count = n()) 

Category.table00 = merge(Category.table11,Category.table22, by="Sub.Category")

Category.table00$Rate = Category.table00$Count.x/Category.table00$Count.y

Category.table00 %>%
  arrange(desc(Category.table00$Rate)) %>%
  top_n(n = 3) 

# Using order numbers,
# Labels(Office Supplies), Tables(Furniture) and Accessories(Technology) are mostly likely to be returned

Category.table1 = Orders.addReturns%>%
  filter(Returned=="Yes") %>%
  group_by(.,Category, Sub.Category) %>%
  summarise(Number=sum(Quantity)) 

Category.table2 = Orders.addReturns%>%
  group_by(.,Category, Sub.Category) %>%
  summarise(Number=sum(Quantity)) 

Category.table = merge(Category.table1,Category.table2, by="Sub.Category")

Category.table$Rate = Category.table$Number.x/Category.table$Number.y

Category.table %>%
  arrange(desc(Category.table$Rate)) %>%
  top_n(n = 3) 

# Using total quantities,
# Labels(Office Supplies),Tables(Furniture) and Art(Office Supplies) are mostly likely to be returned