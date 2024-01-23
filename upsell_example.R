## upsell example 
library(dplyr)
## load in data
ab_data3 <- read.csv('./engineered_feature_tables/ab_data_clean.csv',header = T)
overall_brand_stats <- read.csv('./engineered_feature_tables/overal_brand_stats.csv', header = T)
overall_retail_stats <- read.csv('./engineered_feature_tables/overal_retail_stats.csv', header = T)
overall_retail_brand_stats<- read.csv('engineered_feature_tables/overall_retail_brand_stats.csv',header = T)
brand_pop_price <- read.csv('./engineered_feature_tables/brand_pop_price.csv', header = T)



# function to implement a simple 'upsell' idea where retailers are recommended 
## products with a higher popularity and price index based on data
retail_id = 39
num_prod = 3
ab_upsell_assignment <-function(retail_id = 39,replace = FALSE){
  
  current_data <- ab_data3%>%filter(year == 2023, 
                                    as.numeric(month) > 7, 
                                    retailer_store_number == as.factor(retail_id))
  
  current_products <- unique(current_data$BRAND_VALUE)
  ## find out which AB products are not performing well
  current_ab_products <- brand_pop_price%>%filter(BRAND_VALUE%in%current_products, 
                                                    Brewer=='ANHEUSER-BUSCH INBEV')
  
  retail_specific_stats <- overall_brand_retail_stats%>%
    filter(BRAND_VALUE %in% unique(current_ab_products$BRAND_VALUE),retailer_store_number==retail_id)%>%
    group_by(BRAND_VALUE,retailer_store_number)%>%
    summarise(median_total_sales = median(total_sales), median_rank = min(ranking))%>%arrange(median_total_sales)
  
  drop_list <- unique(retail_specific_stats[1:2,]$BRAND_VALUE) ## dropped 2 lowest ranked brands based on sales
  
  current_stats <- brand_pop_price%>%filter(BRAND_VALUE%in%current_products)
  ## selection criteria:
  
  pop_min<-quantile(brand_pop_price$pop_index,0.5)
  price_min<-quantile(brand_pop_price$price_index,0.5)
  sales_min<-quantile(current_stats$total_sales,0.3)
  
  candidate_brands <- brand_pop_price%>%filter(price_index>price_min,
                                                pop_index>pop_min,
                                               total_sales > sales_min,
                                                !BRAND_VALUE %in% current_products,
                                                Brewer=='ANHEUSER-BUSCH INBEV')%>%
    mutate(rec_score = 0.4*price_index+pop_index,
           recommendation = rank(desc(rec_score)))
  
  upsell_output0 <- candidate_brands[candidate_brands$recommendation<=3,]
  upsell_output1 <- brand_pop_price[brand_pop_price$BRAND_VALUE%in% current_products,]
  upsell_output1[,'rec_score'] <-NA
  upsell_output1[,'recommendation'] <-NA
  
  if(replace){
    upsell_output0 <- candidate_brands[candidate_brands$recommendation<=2,]
    upsell_output2 <-brand_pop_price[(brand_pop_price$BRAND_VALUE%in% drop_list),]
    upsell_output2[,'rec_score'] <-NA
    upsell_output2[,'recommendation'] <- -1
    
    upsell_output <-rbind(upsell_output0,upsell_output1[,colnames(upsell_output0)],upsell_output2[,colnames(upsell_output0)])
  }
  
  else{
    upsell_output <-rbind(upsell_output0,upsell_output1[,colnames(upsell_output0)])
  }
  upsell_output[,'retailer_store_number'] = as.factor(retail_id)
  #upsell_output <-data.frame(cbind( rbind(upsell_output0,upsell_output1),retailer_store_number=retail_id))
  
  return(upsell_output)
  
}



test<-ab_upsell_assignment(retail_id = 39)
test2<-ab_upsell_assignment(retail_id = 39,replace = TRUE)

View(test)
View(test2)

test_upsell<-data.frame()
test_upsell_replace<-data.frame()
## it looks like store 2431 has closed in the begining of 2023:
for(retail_id in unique(ab_data3[ab_data3$retailer_store_number!=2431,]$retailer_store_number)){
  print(retail_id)
  test_upsell<-rbind(test_upsell,ab_upsell_assignment(retail_id = retail_id))
  test_upsell_replace <-rbind(test_upsell_replace ,ab_upsell_assignment(retail_id =retail_id,replace = TRUE))
  
}

test_upsell_final<-test_upsell[!is.na(test_upsell$recommendation),]
test_upsell_replace_final<-test_upsell_replace[!is.na(test_upsell_replace$recommendation),]

write.csv(test_upsell_final,'output/upsell_output.csv',row.names = F)
write.csv(test_upsell_replace_final,'output/upsell_output_with_replace.csv',row.names = F)
