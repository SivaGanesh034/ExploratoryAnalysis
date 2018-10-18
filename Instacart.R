library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)

orders=read.csv('D:\\DataScience_Drive\\Kaggle\\Instacart\\orders.csv')
products=read.csv('D:\\DataScience_Drive\\Kaggle\\Instacart\\products.csv')
order_products=read.csv('D:\\DataScience_Drive\\Kaggle\\Instacart\\order_products__train.csv')
order_products_prior=read.csv('D:\\DataScience_Drive\\Kaggle\\Instacart\\order_products__prior.csv')
aisles=read.csv('D:\\DataScience_Drive\\Kaggle\\Instacart\\aisles.csv')
departments=read.csv('D:\\DataScience_Drive\\Kaggle\\Instacart\\departments.csv')

names(orders)
#Converting Character Variables to Factors
#Mutate helps in creating new variables
orders=orders %>% mutate(order_hour_of_day=as.numeric(order_hour_of_day), eval_set=as.factor(eval_set))
products$product_name=as.factor(products$product_name)
glimpse(orders)
aisles$aisle=as.factor(aisles$aisle)
departments$department=as.factor(departments$department)

#WHEN DO PEOPLE ORDER?
#Let's have a look when people buy groceries online.
#ORDER HOUR OF DAY
orders %>% ggplot(aes(x=order_hour_of_day)) + 
                    geom_histogram(stat="count",fill="steelblue")
##Clearing we can see that there is a clear effect of hour of day on volume.
##Most orders are between 8.00-18.00

#Now let's look at the day of the week
#DAY OF WEEK
orders %>% ggplot(aes(x=order_dow)) +
                  geom_histogram(stat = "count",fill="red")
##Clearing we can see that, in a week 0 and 1 are high on orders.
# 0 and 1 can be weekends

#When do they order again
orders %>% ggplot(aes(x=days_since_prior_order)) +
                    geom_histogram( stat="count", fill="red")
##Clearing it looks like people seem to order more often after exactly a week

#Let's look how many prior orders are there.
unique(orders$eval_set)
##Clearing we can see that we have three levels

unique(orders$order_number)

#Let's see how many prior orders are there?
orders %>% filter(eval_set=="prior") %>% count(order_number) %>% ggplot(aes(order_number,n)) +
geom_line(color="red",size=1) + geom_point(size=2,color="red")
#Let's check how many items do people buy i.e how many orders are in order?



head(order_products) 
head(order_products_prior)

##Let's see how many items do people buy?
##In train set
order_products %>% 
  group_by(order_id) %>%
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items)) +
  geom_histogram(stat = "count",fill="red") +
  geom_rug() +
  coord_cartesian(xlim = c(0,80))

##Let's see how many items do people buy?
##In prior set
#Prior set contains previous order contents of all customers.

order_products_prior %>%
  group_by(order_id) %>%
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items)) +
  geom_histogram(stat = "count",fill="red") +
  geom_rug() +
  coord_cartesian(xlim = c(0,80))
  
order_products %>%
  group_by(order_id) %>%
  summarise(n_items=last(add_to_cart_order))

#Bestsellers
#Let's have a look which products are sold most often 
tmp = order_products %>% 
  group_by(product_id) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  left_join(select(products,product_id,product_name),by="product_id") %>%
  arrange(desc(count)) 
kable(tmp)

tmp %>% 
  ggplot(aes(x=reorder(product_name,-count), y=count))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())

#How often do people order the same items again?
tmp = order_products %>% 
  group_by(reordered) %>% 
  summarize(count = n()) %>% 
  mutate(reordered = as.factor(reordered)) %>%
  mutate(proportion = count/sum(count))
kable(tmp)


tmp %>% 
  ggplot(aes(x=reordered,y=count,fill=reordered))+
  geom_bar(stat="identity")

