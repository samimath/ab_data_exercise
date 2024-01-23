## analysis of upsell example:
library(dplyr)
library(ggplot2)
library(ggrepel)
upsell_output1<-read.csv('output/upsell_output.csv',header = TRUE)
upsell_output_with_replace<-read.csv('output/upsell_output_with_replace.csv',header = TRUE)
overall_retail_brand_stats<- read.csv('engineered_feature_tables/overall_retail_brand_stats.csv',header = T)
overall_brand_stats <- read.csv('./engineered_feature_tables/overal_brand_stats.csv', header = T)
overall_retail_stats <- read.csv('./engineered_feature_tables/overal_retail_stats.csv', header = T)
brand_pop_price <- read.csv('./engineered_feature_tables/brand_pop_price.csv', header = T)


## estimate potential sales lift based on recommendation:
plot(overall_retail_stats$total_sales~overall_retail_stats$product_variety)

## estimate effect of addint a new product to total sales:

prod_var_model<-lm(total_sales~product_variety+num_loc, data = overall_retail_stats)
summary(prod_var_model)
#summary(overall_brand_retail_stats$median_sales) 

ggplot(upsell_output1) + geom_point(aes(x = pop_index, y = price_index , col= BRAND_VALUE), size = 3)+
  facet_wrap(.~retailer_store_number)+theme_bw()

ggplot(upsell_output_with_replace) + geom_point(aes(x = pop_index, 
                                                    y = price_index , 
                                                    pch = as.factor(recommendation),
                                                    col= BRAND_VALUE),size = 3)+
  facet_wrap(.~retailer_store_number)+theme_bw()