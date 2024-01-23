library(readxl)
library(dplyr)
library(DataExplorer)
library(parallel)
library(ggplot2)
library(broom)
library(stringr)
## load data:
ab_data <- read_xlsx(path = './source_data/AB DS Data Challenge 1.xlsx')

## create some augmented features for further analysis
year_vec<-as.numeric(substr(ab_data$year_week,1,4))
week_vec<-as.numeric(substr(ab_data$year_week,5,6))
month_vec<-unlist(lapply(as.numeric(week_vec), function(x){min(ceiling(x/4),12)}))
ab_data[,'year']<-year_vec
ab_data[,'month']<-month_vec
ab_data[,'week']<-week_vec
ab_data[,'oz']<-unlist(lapply(ab_data$Package_Value,function(x){as.numeric(strsplit(word(x,2),'-')[[1]][1])})) #as.numeric(str_extract(ab_data$Package_Value, "(\\d)+(?= OZ)"))
ab_data[,'package']<-word(ab_data$Package_Value,1)
ab_data[,'package2'] <- unlist(lapply(ab_data$package, function(x){ifelse((x=='SINGLES' || x == 'KEG' || x=='BAG'),'1-PACK',x)}))
ab_data[,'pack_size']<-as.numeric(gsub('-PACK','',ab_data$package2))
ab_data[,'rev_per_oz']<-ab_data$dollar_sales/(ab_data$oz*ab_data$pack_size*ab_data$unit_sales)
# convert character to factor:
ab_data1 <- mutate_if(ab_data, is.character, as.factor)

## some light data filtering based on summary
ab_data2<-ab_data1%>%filter(unit_sales>0,
                       dollar_sales>0,
                       volume_sales>0,
                       Brewer!='null',
                       BRAND_VALUE!='null',
                       !is.na(rev_per_oz))%>%
  mutate(year = as.factor(year),
         month = as.factor(month),
         Product_Key=as.factor(Product_Key),
         Product_Key = as.factor(Product_Key),
         year = as.factor(year),
         wholesaler_id_value = as.factor(wholesaler_id_value),
         retailer_store_number = as.factor(retailer_store_number))
ab_data3<-na.omit(ab_data2)

brewer_brand_ref <- na.omit(unique(ab_data3[,c('BRAND_VALUE','Brewer')]))

city_ref<-unique(ab_data3[,c('city','state_code')])
## check with the US census data to gather some data about population and geolocation
us_cities <- read_excel('uscities.xlsx')
ny_cities_summary <-us_cities%>%group_by(city,state_id)%>%
  summarise(total_pop = sum(population),
            lat = median(lat),
            lng = median(lng),
            density = median(density))%>%mutate(city = toupper(city))%>%filter(state_id == 'NY')
city_ref2<-merge(city_ref,ny_cities_summary, by.x=c('city','state_code'), by.y=c('city','state_id'), all.x = TRUE)


## which brands are best sellers per year measured by total dollar sales from all stores?
## brand level info:
overall_brand_stats <- ab_data3%>%group_by(BRAND_VALUE)%>%
  summarise(total_sales=sum(dollar_sales),
            total_sales_per_year = total_sales/(length(unique(year))),
            sales_reliability = sd(dollar_sales),
            median_sales = median(dollar_sales),
            sales_count = n(),
            rev_per_10oz = median(10*rev_per_oz))%>%arrange(desc(total_sales))%>%
  mutate(ranking = rank(desc(total_sales_per_year), ties.method = 'first'))%>%merge(brewer_brand_ref,on='BRAND_VALUE')


overall_brand_retail_stats <- ab_data3%>%group_by(year,city,retailer_store_number,BRAND_VALUE)%>%
  summarise(total_sales=sum(dollar_sales),
            sales_reliability = sd(dollar_sales),
            median_sales = median(dollar_sales),
            sales_count = n(),
            rev_per_10oz = median(10*rev_per_oz))%>%arrange(desc(total_sales))%>%
  mutate(ranking = rank(desc(total_sales), ties.method = 'first'))%>%merge(brewer_brand_ref,on='BRAND_VALUE')



overall_retail_stats <- ab_data3%>%group_by(retailer_store_number,year)%>%
  summarise(total_sales=sum(dollar_sales),
            total_units = sum(unit_sales),## typical per year sales
            sales_reliability = sd(dollar_sales), # standard deviation of sales indicate consistency of business 
            median_sales = median(dollar_sales), # typical order size
            rev_per_10oz = median(10*rev_per_oz),
            sales_count = n(), # typical # of transaction
            num_loc = length(unique(city)), # num of store locations
            product_variety = length(unique(BRAND_VALUE)),# num of products carried by store 
            ab_ratio = sum(Brewer=="ANHEUSER-BUSCH INBEV")/n() ## ratio of AB products in the store
  )%>%arrange(desc(total_sales))


overall_retail_city_stats <- ab_data3%>%group_by(retailer_store_number,city)%>%
  summarise(total_sales=sum(dollar_sales),
            total_units = sum(unit_sales),## typical per year sales
            sales_reliability = sd(dollar_sales), # standard deviation of sales indicate consistency of business 
            median_sales = median(dollar_sales), # typical order size
            rev_per_10oz = median(10*rev_per_oz),
            sales_count = n(), # typical # of transaction
            year_in_business = length(unique(year)),
            avg_sales = total_sales/year_in_business,
            avg_units = total_units/year_in_business,
            avg_sales_count = sales_count/year_in_business,
            product_variety = length(unique(BRAND_VALUE)),# num of products carried by store 
            ab_ratio = sum(Brewer=="ANHEUSER-BUSCH INBEV")/n() ## ratio of AB products in the store
  )%>%arrange(desc(total_sales))%>%merge(ny_cities_summary, on = c('city','state_code'))
overall_retail_city_stats[,'city_retail_id']<-paste0(overall_retail_city_stats$retailer_store_number,overall_retail_city_stats$city)
## get area density and population for the stores:

overall_retail_stats2 <- overall_retail_city_stats%>%group_by(retailer_store_number)%>%
  summarise(total_pop = sum(total_pop),
            median_density = median(density))%>%merge(overall_retail_stats, on = c('city','state'))




### create a matrix summarizing a brand's 'populatiry' as ranked by overall sales and price using 'price per 10 oz' feature
pop_index_coef <- max(overall_brand_stats$ranking)
price_index_coef <- quantile(overall_brand_stats$rev_per_10oz,.90)

brand_pop_price <- overall_brand_stats%>%group_by(BRAND_VALUE,Brewer)%>%
  summarize(total_sales = sum(total_sales), 
            pop_index = (pop_index_coef - median(ranking))/pop_index_coef , ## normalizing - higher = better
            price_index = min(median(rev_per_10oz),3)) ## scale it to a similar scale as popularity index
            #price_index = min(median(rev_per_10oz)/price_index_coef,1))
summary(brand_pop_price)
### save data:
write.csv(brand_pop_price,'engineered_feature_tables/brand_pop_price.csv',row.names = FALSE)
write.csv(overall_brand_stats,'engineered_feature_tables/overal_brand_stats.csv',row.names = FALSE)
write.csv(overall_retail_stats ,'engineered_feature_tables/overal_retail_stats.csv',row.names = FALSE)
write.csv(overall_brand_retail_stats,'engineered_feature_tables/overall_retail_brand_stats.csv',row.names = FALSE)
write.csv(ab_data3,'./engineered_feature_tables/ab_data_clean.csv',row.names = FALSE)
write.csv(city_ref,'./engineered_feature_tables/city_ref.csv',row.names = FALSE)
write.csv(overall_retail_city_stats,'./engineered_feature_tables/overall_retail_city_stats.csv',row.names = FALSE)
