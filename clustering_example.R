## clustering example 

### 3.	Cluster/segment stores based on their characteristics such as volume level, price level, discounting pattern, geo info, 
## and product portfolio composition (not limited only to these). 
library(dplyr)
library(lme4)
library(psych)
library(skimr)
library(ggpubr)
library(ggrepel)
overall_retail_city_stats<-read.csv('./engineered_feature_tables/overall_retail_city_stats.csv')
drop_cols <- c('state_id','sales_reliability')
data_for_clustering <-na.omit(overall_retail_city_stats%>%select(-all_of(drop_cols)) )
pairs.panels(data_for_clustering[,c('avg_sales','avg_units','avg_sales_count',
                                                    'density','product_variety','total_pop','year_in_business','rev_per_10oz')])

row.names(data_for_clustering) <-data_for_clustering$city_retail_id
## We now havea 23 by 17 table for clustering 

## normalize data for clustering
mds <- select_if(data_for_clustering, is.numeric) %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")

## K-means clustering
clust <- kmeans(mds, 4)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)


mds_df <- data.frame(cbind(mds,rownames(data_for_clustering)))
colnames(mds_df)<-c('Dim1','Dim2','cluster','city_retail_id')

segmentation_output <- data_for_clustering%>%merge(mds_df, on = city_retail_id)



ggplot(segmentation_output )+geom_point(aes(x=Dim1,y=Dim2,col=cluster)) + 
  facet_wrap(.~cluster,nrow=2)+
  geom_label_repel(aes(x=Dim1,y=Dim2,
                       label = city_retail_id))+ggtitle('K-means clustering output')+theme_bw()


ggplot(segmentation_output )+geom_point(aes(x=product_variety,y=avg_sales,col=cluster)) + 
  facet_wrap(.~cluster,nrow=2)+
  geom_label_repel(aes(x=product_variety,y=avg_sales,
                       label = city_retail_id))+ggtitle('View from product variety vs avg yearly sales')+
  theme_bw()


## population density vs avg yearly sales

ggplot(segmentation_output )+geom_point(aes(x=density,y=avg_sales,col=cluster)) + 
  facet_wrap(.~cluster,nrow=2)+
  geom_label_repel(aes(x=density,y=avg_sales,
                       label = city_retail_id))+ ggtitle('View from pop density vs avg yearly sales')+
  theme_bw()
                                                                                   
## population density vs rev per 10 oz

ggplot(segmentation_output )+geom_point(aes(x=density,y=rev_per_10oz,col=cluster)) + 
  facet_wrap(.~cluster,nrow=2)+
  geom_label_repel(aes(x=density,y=rev_per_10oz,
                       label = city_retail_id))+ggtitle('View from pop density vs rev per 10 oz')+
  theme_bw()
  


ggplot(segmentation_output)+geom_point(aes(x=rev_per_10oz, y = avg_sales,group = cluster, col=cluster), size = 2)+
  facet_wrap(.~cluster,nrow=2)+theme_bw()


ggplot(segmentation_output )+geom_point(aes(x=rev_per_10oz,y=avg_sales,group=cluster,col=cluster)) + 
  facet_wrap(.~cluster,nrow=2)+
  geom_label_repel(aes(x=rev_per_10oz,y=avg_sales,
                       label = city_retail_id))+ggtitle('View from rev per 10 oz vs avg yearly sales')+
  theme_bw()


### hypothesis 1: average sales is largely explained by rev per 10 oz (price)?

price_model<-lm(avg_sales~rev_per_10oz+ab_ratio+as.factor(cluster)*rev_per_10oz+product_variety, data = segmentation_output)
