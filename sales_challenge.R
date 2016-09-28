library(jsonlite)

#read file
file <- readLines('~/Desktop/sample.txt', encoding = "UTF-8")

#fix file to be just one big string
full_json <- paste(file, collapse = '')

#parse json to a data.frame
sales_table <- fromJSON(full_json)

# 1635 lines
dim(sales_table)

dates <- sapply(sales_table$ide$dhEmi, strptime, tz = "UTC", "%Y-%m-%dT%H:%M:%OSZ")
dates <- dates$`$date`

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


#the dates start at day 5 until 23
for(i in 5:23){
  day_amount <- sum(amount_by_day$tot_value[amount_by_day$days == i])
  print(day_amount)
  sum_by_day <- rbind(sum_by_day, c(i, day_amount))
}

#all available hours
for(i in 1:24){
  hour_amount <- sum(amount_by_hour$tot_value[amount_by_hour$hours == i])
  print(hour_amount)
  sum_by_hour <- rbind(sum_by_hour, c(i, hour_amount))
}

#all tables
for(i in as.character(unique(sales_table$infAdic$infCpl))){
  table_amount <- sum(amount_by_table$tot_value[amount_by_table$table == i])
  table_number <- as.integer(gsub('[a-z A-Z]', '', i))
  tmp_table <- data.frame(table = table_number, amount = as.double(table_amount), stringsAsFactors = F)
  sum_by_table <- rbind(sum_by_table, tmp_table)
}

# hours when there is more clients
plot(sum_by_hour, type = 'l')

# sales by day
plot(sum_by_day, type = 'l')

#table proportion
plot(table(as.integer(gsub('[a-z A-Z]', '', sales_table$infAdic$infCpl))))

#spend by table
plot(sum_by_table[order(sum_by_table$table),], type = 'l')


