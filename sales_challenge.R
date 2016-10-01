library(jsonlite)

# read file
file <- readLines('sample.txt', encoding = "UTF-8")

# fix file to be just one big string
full_json <- paste(file, collapse = '')

# parse json to a data.frame
sales_table <- fromJSON(full_json)

# 1635 lines
dim(sales_table)

#Fix date format to be possible to extract more info from there
dates <- sapply(sales_table$ide$dhEmi, strptime, tz = "UTC", "%Y-%m-%dT%H:%M:%OSZ")
dates <- dates$`$date`
sales_table$ide$dhEmi <- dates

# extract days from dates
days <- as.integer(format(dates, '%d'))

# extract hour from dates
hours <- as.integer(format(dates, '%H'))

# fix value to be a double
tot_value <- as.double(sales_table$complemento$valorTotal)

#generate aux data frames to easily work with data
amount_by_day <- data.frame(days, tot_value)
amount_by_hour <- data.frame(hours, tot_value)
amount_by_table <- data.frame(table = as.character(sales_table$infAdic$infCpl), tot_value)
sum_by_day <- data.frame()
sum_by_hour <- data.frame()
sum_by_table <- data.frame(table=character(), amount = double(), stringsAsFactors=FALSE)

######
# Exploratory analysis
#####

# the dates start at day 5 until 23
for(i in 5:23){
  day_amount <- sum(amount_by_day$tot_value[amount_by_day$days == i])
  print(day_amount)
  sum_by_day <- rbind(sum_by_day, c(i, day_amount))
}

# all available hours
for(i in 1:24){
  hour_amount <- sum(amount_by_hour$tot_value[amount_by_hour$hours == i])
  print(hour_amount)
  sum_by_hour <- rbind(sum_by_hour, c(i, hour_amount))
}

# all tables available
for(i in as.character(unique(sales_table$infAdic$infCpl))){
  table_amount <- sum(amount_by_table$tot_value[amount_by_table$table == i])
  table_number <- as.integer(gsub('[a-z A-Z]', '', i))
  tmp_table <- data.frame(table = table_number, amount = as.double(table_amount), stringsAsFactors = F)
  sum_by_table <- rbind(sum_by_table, tmp_table)
}

# hours when there is more clients
plot(sum_by_hour, type = 'l')
# restaurant works on 11h-15h and 18h-23h - and closes once a week

# sales by day
barplot(sum_by_day)

# average sales by day - 4.776,28
mean(sum_by_day$X6821.34)

# table proportion - tables with higher numbers have less value and less clients
barplot(table(as.integer(gsub('[a-z A-Z]', '', sales_table$infAdic$infCpl))))

# spend by table
plot(sum_by_table[order(sum_by_table$table),])

# products
# generate a data.frame with the products, their order, the hour, the day, the value, quantity and table the order was made
products <- data.frame()
for(i in 1:length(sales_table$dets)){
  day <- as.integer(format(sales_table$ide$dhEmi[i], '%d'))
  hour <- as.integer(format(sales_table$ide$dhEmi[i], '%H'))
  value <- as.double(sales_table$dets[[i]]$prod$vProd)
  quantity <- as.integer(sales_table$dets[[i]]$prod$qCom)
  restaurant_table <- as.integer(gsub('[a-z A-Z]', '', sales_table$infAdic$infCpl[i]))
  product <- data.frame(product = as.character(sales_table$dets[[i]]$prod$xProd), order = i, day, hour, value, quantity, restaurant_table, stringsAsFactors = F)
  print(product)
  products <- rbind(products, product)
}

# types of food and their frequency
table(products$product)
barplot(table(products$product))

# product and hour of consumption
table(products$product, products$hour)

# mean buffet value - 46.19
mean(products$value[products$product == 'BUFFET'])

# most consumed products are buffet and refrigerante. Create aux data frames to analyse that data
buffet_per_day <- cbind(products$value[products$product == 'BUFFET'], products$day[products$product == 'BUFFET'])
refri_per_day <- cbind(products$value[products$product == 'REFRIGERANTE'], products$day[products$product == 'REFRIGERANTE'])

# mean buffet value
aggregate(buffet_per_day[,1], by=list(buffet_per_day[,2]), FUN=mean)

# mean buffet amount
aggregate(buffet_per_day[,1], by=list(buffet_per_day[,2]), FUN=length)

# mean refri amount
aggregate(refri_per_day[,1], by=list(refri_per_day[,2]), FUN=length)

# mean refri value
aggregate(refri_per_day[,1], by=list(refri_per_day[,2]), FUN=mean)

#mean week 2 values
aggregate(products$value, by = list(products$day), FUN = mean)


######
# How much a customer will spend
#####

# how to predict how much a customer will spend without knowing the customer?
# hour of the day the customers goes + table he sits gives a nice average. (infered by the exploratory analysis)

#even if the valorTotal is not linear - it even looks like to have a pattern, let's check the linear regression.

# divide the data in one week + 5 days (train_data) and more 5 days (test_data)
# day 16 is the divisor between those weeks

train_data <- products[products$day <= 16,]
test_data <-  products[products$day > 16,]

# the model consists only on hour and table. Hour is a stronger variable according to it's p-value.
linear_regression <- lm(value ~ restaurant_table + hour, data = train_data)
summary(linear_regression)

# test the model against test_data and compares it with the model mean and the actual mean for the week.
mean(predict(linear_regression, test_data)) #26,07
mean(test_data$value) #26,09

# know what time the customer arrives and what table he sits gaves the amount a customer will spend.