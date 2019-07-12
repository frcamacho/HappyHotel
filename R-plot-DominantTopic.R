# load libraries 
require(tidyverse)
require(ggsci)

df <- read_csv("reviews_df_dominant-topics.csv", col_names = T)

# what are the dominant topic distribution for each hotel and happy and unhappy responses 
df_counts <- df %>% group_by(Is_Response, hotel_ID, Dominant_Topic) %>% count() %>% ungroup() %>% 
  group_by(hotel_ID, Dominant_Topic) %>% mutate(sumN = sum(n)) %>% ungroup() %>% 
  spread(., Is_Response, n, fill = 0) %>% mutate(nothappyProp = (`not happy`/sumN)*100) %>% 
  arrange(desc(nothappyProp))
# plot bargraph facet by hotel id 
df_counts %>% ggplot(., aes(x=as.factor(Dominant_Topic),  y = nothappyProp, fill=as.factor( hotel_ID))) + 
  geom_bar( stat="identity") +    
  facet_wrap(~hotel_ID) + theme_minimal() + coord_flip() + scale_fill_futurama(name = "Hotel ID") + 
  ggtitle("Proportion of 'not happy' Reviews by Dominant Topic") + 
  ylab("not_happy proportion") + xlab("Dominant Topic") 