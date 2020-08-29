data(heights)
heights
str(heights)
row_query = heights.row == 777
row_query
heights[777,2]
heights[776:778,0:2]
min(heights$height)
max(heights$height)

min(heights$height)

match(min(heights$height),heights$height)
heights[1030:1033,0:2]

mean(heights$height)
median(heights$height)

heights %>% 
  group_by(sex) %>% 
  summarise(amount = n()) %>% 
  mutate(proportion = amount/sum(amount)*100)

unique(heights$sex)

heights %>% 
  filter(height>78, sex == 'Female') %>% 
  nrow()
