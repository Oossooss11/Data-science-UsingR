#Downloading needed packages
install.packages("readr")
library(readr)
install.packages("dplyr")
library(dplyr)
install.packages("arules")
library(arules)

#Importing Dataset to R
grc <- read_csv("grc.csv")
View(grc)

#Removing column rnd for data cleaning
grc$rnd = NULL

#Grouping Ages and Sum by Total Spending for #2
ages = group_by(grc,age)
ages = summarise(ages,totalSpending=sum(total))
View(ages)

#Grouping Cities and Sum by Total Spending for #3
cities = group_by(grc,city)
cities = summarise(cities,totalSpending=sum(total))
cities = arrange(cities, desc(totalSpending))
View(cities)

#Grouping Customers by their total spending and Gender for #5
customers = group_by(grc,customer)
customers = summarise(customers,totalSpending=sum(total))
gender = c("Male", "Male", "Female", "Female", "Female",
           "Female", "Male", "Male", "Male", "Female",
           "Male", "Male", "Male", "Female", "Female")
customers = cbind(customers, gender)
customers = arrange(customers, desc(totalSpending))
View(customers)

#Grouping genders by their total spending for #6
genders = group_by(customers,gender)
genders = summarise(genders,total=sum(totalSpending))
View(genders)

#Creating Data Visualization Dashboard #1
par(mfrow=c(2,2))
#1
pie(
  x = table(grc$paymentType),
  main = "Payment Methods Types"
)
#2
barplot(
  height =ages$totalSpending,
  name =ages$age,
  col = "skyblue",
  main = "Total Spending by Age",
  xlab = "Ages",
  ylab = "Spending"
)
#3
barplot(
  height =cities$totalSpending,
  name =cities$city,
  col = "skyblue",
  main = "Total Spending by City",
  xlab = "Cities",
  ylab = "Spending"
)
#4
boxplot(
  x= grc$total,
  col= "skyblue",
  main= "Distribution of Total Spending",
  xlab= "Spending"
)

#Creating Data Visualization Dashboard #2
par(mfrow=c(2,2))
#5
barplot(
  height =customers$totalSpending,
  name =customers$customer,
  col = "skyblue",
  main = "Total Spending by Customers",
  xlab = "Name",
  ylab = "Spending"
)
#6
pie(
  x = table(customers$gender),
  main = "Total Count by Gender"
)
#7
barplot(
  height =genders$total,
  name =genders$gender,
  col = "skyblue",
  main = "Total Spending by Gender",
  xlab = "Gender",
  ylab = "Spending"
)

#Kmeans Clustering
nclusters <- readline("Enter number of clusters: ")

if (nclusters>=2 & nclusters<=4){
  Kmeans<-kmeans(ages,center=nclusters)
  Kmeans
}else {print("Wrong input")}

#Apriori Algorithm
grc_1 <- read.transactions("C:\\Users\\muazs\\Documents\\grc_1.txt", sep=",")

min_support<-as.numeric(readline("Enter minimum suport: "))
min_confidence<-as.numeric(readline("Enter minimum confidence: "))

if ((min_support>=0.001 & min_support<=1) &
    (min_confidence>=0.01 & min_confidence<=1)){
apriorirules <- apriori(grc_1, 
parameter = list(supp = min_support, conf = min_confidence,minlen=2))
print(inspect(apriorirules))
}else {print("Wrong input")}

#Listing unique items and their count
items2 <- read.csv("C:\\Users\\muazs\\Documents\\grc.csv",
                   stringsAsFactors = FALSE)

transactions = strsplit(as.vector(items2$items), ',')

unique_items = unique(unlist(transactions))

get_occurrences <- function(item_names){
  names_matrix <- item_names
  if(!is.matrix(item_names))
    names_matrix <- matrix(item_names)
  result <- c()
  for (name_index in 1:nrow(names_matrix)) {
    name_row <- names_matrix[name_index,]
    result[name_index] <- 0
    for (tran_index in 1:length(transactions)) {
      transaction <- transactions[tran_index]
      if(length(intersect(name_row,transaction[[1]])) >= length(name_row))
      {
        result[name_index] <-result[name_index]+1
      }
    }
  }
  return(result)
}

single_items <- data.frame(item = unique_items, stringsAsFactors = FALSE )

single_items <- mutate(single_items,count = get_occurrences(item))
View(single_items)

#Data cleaning - Removing duplicates
#cream cheese
single_items$count[97] = 390
single_items = single_items[-c(10),]

#roll products
single_items$count[146] = 101
single_items = single_items[-c(141),]

single_items = arrange(single_items, desc(count))

barplot(
  height =single_items$count,
  name =single_items$item,
  col = "skyblue",
  main = "Most Selling Items",
  xlab = "Name",
  ylab = "Count"
)