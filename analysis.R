# Text Minning of Vol I Capital
# 
# Autor: @gustavoovital
# Author: @gustavoovital

# Packages ----

library(pdftools)
library(tidyverse)
library(tidytext)
library(extrafont)

pdf_text('Capital-Volume-I.pdf')

Part_I_Commodities_and_Money <- pdf_text(pdf_subset('Capital-Volume-I.pdf', pages = 26:102, output = "parti.pdf"))
Part_II_Transformation_of_Money_into_Capital <- pdf_text(pdf_subset('Capital-Volume-I.pdf', pages = 103:125, output = "partii.pdf"))
Part_III_The_Production_of_Absolute_Surplus_Value <- pdf_text(pdf_subset('Capital-Volume-I.pdf', pages = 126:218, output = "partiii.pdf"))
Part_IV_Production_of_Relative_Surplus_Value <- pdf_text(pdf_subset('Capital-Volume-I.pdf', pages = 219:357, output = "partiv.pdf"))
Part_V_Production_of_Absolute_and_Relative_Surplus_Value <- pdf_text(pdf_subset('Capital-Volume-I.pdf', pages = 358:377, output = "partv.pdf"))
Part_VI_Wages <- pdf_text(pdf_subset('Capital-Volume-I.pdf', pages = 378:399, output = "partvi.pdf"))
Part_VII_The_Accumulation_of_Capital <- pdf_text(pdf_subset('Capital-Volume-I.pdf', pages = 400:505, output = "partvii.pdf"))
Part_VIII_Primitive_Accumulation <- pdf_text(pdf_subset('Capital-Volume-I.pdf', pages = 506:549, output = "partviii.pdf"))

# function to split and remove \n ----

split_all <- function(data){
  splt <- str_c(unlist(data %>%
                         str_split('\n')), collapse = ' ')
  return(splt)
}

# creating a corpus --- 

corpus <- 
  tibble(part = c('Commodities_and_Money', 
                  'Transformation_of_Money_into_Capital',
                  'The_Production_of_Absolute_Surplus_Value',
                  'Production_of_Relative_Surplus_Value',
                  'Production_of_Absolute_and_Relative_Surplus_Value',
                  'Wages',
                  'The_Accumulation_of_Capital',
                  'Primitive_Accumulation'),
         text = c(split_all(Part_I_Commodities_and_Money), 
                  split_all(Part_II_Transformation_of_Money_into_Capital),
                  split_all(Part_III_The_Production_of_Absolute_Surplus_Value),
                  split_all(Part_IV_Production_of_Relative_Surplus_Value),
                  split_all(Part_V_Production_of_Absolute_and_Relative_Surplus_Value),
                  split_all(Part_VI_Wages),
                  split_all(Part_VII_The_Accumulation_of_Capital),
                  split_all(Part_VIII_Primitive_Accumulation)))

# stop_words ----

StopWords <- c(stopwords::stopwords(), letters, 'chapter', 'us', 'let', 1:100, 'l.c')

# with stop words ----

# Commodities_and_Money ----

corpus %>% 
  filter(part == 'Commodities_and_Money') %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c('term1', 'term2'), sep = ' ') %>% 
  
  mutate(term1 = if_else(term1 %in% c("value", "values"), "value", term1)) %>% 
  mutate(term2 = if_else(term2 %in% c("value", "values"), "value", term2)) %>% 
  
  filter(!term1 %in% StopWords) %>% 
  filter(!term2 %in% StopWords) %>% 
  arrange(desc(n)) %>% 
  mutate(term = paste(term1, term2)) %>% 
  head(20) -> Commodities_and_Money

# 'Transformation_of_Money_into_Capital' ----

corpus %>% 
  filter(part == 'Transformation_of_Money_into_Capital') %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c('term1', 'term2'), sep = ' ') %>% 
  
  mutate(term1 = if_else(term1 %in% c("value", "values"), "value", term1)) %>% 
  mutate(term2 = if_else(term2 %in% c("value", "values"), "value", term2)) %>% 
  
  filter(!term1 %in% StopWords) %>% 
  filter(!term2 %in% StopWords) %>% 
  arrange(desc(n)) %>% 
  mutate(term = paste(term1, term2)) %>% 
  head(20) -> Transformation_of_Money_into_Capital

# 'The_Production_of_Absolute_Surplus_Value' ----

corpus %>% 
  filter(part == 'The_Production_of_Absolute_Surplus_Value') %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c('term1', 'term2'), sep = ' ') %>% 
  
  mutate(term1 = if_else(term1 %in% c("value", "values"), "value", term1)) %>% 
  mutate(term2 = if_else(term2 %in% c("value", "values"), "value", term2)) %>% 
  
  filter(!term1 %in% StopWords) %>% 
  filter(!term2 %in% StopWords) %>% 
  arrange(desc(n)) %>% 
  mutate(term = paste(term1, term2)) %>% 
  head(20) -> The_Production_of_Absolute_Surplus_Value

# 'Production_of_Relative_Surplus_Value' ----

corpus %>% 
  filter(part == 'Production_of_Relative_Surplus_Value') %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c('term1', 'term2'), sep = ' ') %>% 
  
  mutate(term1 = if_else(term1 %in% c("value", "values"), "value", term1)) %>% 
  mutate(term2 = if_else(term2 %in% c("value", "values"), "value", term2)) %>% 
  
  filter(!term1 %in% StopWords) %>% 
  filter(!term2 %in% StopWords) %>% 
  arrange(desc(n)) %>% 
  mutate(term = paste(term1, term2)) %>% 
  head(20) -> Production_of_Relative_Surplus_Value

# 'Production_of_Absolute_and_Relative_Surplus_Value' ----

corpus %>% 
  filter(part == 'Production_of_Absolute_and_Relative_Surplus_Value') %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c('term1', 'term2'), sep = ' ') %>% 
  
  mutate(term1 = if_else(term1 %in% c("value", "values"), "value", term1)) %>% 
  mutate(term2 = if_else(term2 %in% c("value", "values"), "value", term2)) %>% 
  
  filter(!term1 %in% StopWords) %>% 
  filter(!term2 %in% StopWords) %>% 
  arrange(desc(n)) %>% 
  mutate(term = paste(term1, term2)) %>% 
  head(20) -> Production_of_Absolute_and_Relative_Surplus_Value

# 'Wages' ----

corpus %>% 
  filter(part == 'Wages') %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c('term1', 'term2'), sep = ' ') %>% 
  
  mutate(term1 = if_else(term1 %in% c("value", "values"), "value", term1)) %>% 
  mutate(term2 = if_else(term2 %in% c("value", "values"), "value", term2)) %>% 
  
  filter(!term1 %in% StopWords) %>% 
  filter(!term2 %in% StopWords) %>% 
  arrange(desc(n)) %>% 
  mutate(term = paste(term1, term2)) %>% 
  head(20) -> Wages

# 'The_Accumulation_of_Capital' ----

corpus %>% 
  filter(part == 'The_Accumulation_of_Capital') %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c('term1', 'term2'), sep = ' ') %>% 
  
  mutate(term1 = if_else(term1 %in% c("value", "values"), "value", term1)) %>% 
  mutate(term2 = if_else(term2 %in% c("value", "values"), "value", term2)) %>% 
  
  filter(!term1 %in% StopWords) %>% 
  filter(!term2 %in% StopWords) %>% 
  arrange(desc(n)) %>% 
  mutate(term = paste(term1, term2)) %>% 
  head(20) -> The_Accumulation_of_Capital

# 'Primitive_Accumulation' ----

corpus %>% 
  filter(part == 'Primitive_Accumulation') %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE) %>% 
  separate(bigram, c('term1', 'term2'), sep = ' ') %>% 
  
  mutate(term1 = if_else(term1 %in% c("value", "values"), "value", term1)) %>% 
  mutate(term2 = if_else(term2 %in% c("value", "values"), "value", term2)) %>% 
  
  filter(!term1 %in% StopWords) %>% 
  filter(!term2 %in% StopWords) %>% 
  arrange(desc(n)) %>% 
  mutate(term = paste(term1, term2)) %>% 
  head(20) -> Primitive_Accumulation

# Into the graphs ----

Commodities_and_Money$Part <- 'Part I - Commodities and Money' 
Transformation_of_Money_into_Capital$Part <- 'Part II - Transformation of Money into Capital'
The_Production_of_Absolute_Surplus_Value$Part <- 'Part III - The Production of Absolute Surplus Value'
Production_of_Relative_Surplus_Value$Part <- 'Part IV - Production of Relative Surplus Value'
Production_of_Absolute_and_Relative_Surplus_Value$Part <- 'Part V - Production of Absolute and Relative Surplus Value'  
Wages$Part <- 'Part VI - Wages' 
The_Accumulation_of_Capital$Part <- 'Part VII - The Accumulation of Capital' 
Primitive_Accumulation$Part <- 'Part VIII - Primitive Accumulation'  


data <- rbind(
  Commodities_and_Money,
  Transformation_of_Money_into_Capital,
  The_Production_of_Absolute_Surplus_Value,
  Production_of_Relative_Surplus_Value,
  Production_of_Absolute_and_Relative_Surplus_Value,
  Wages,
  The_Accumulation_of_Capital,
  Primitive_Accumulation
) 


data %>% 
  group_by(Part) %>% 
  top_n(15) %>% 
  ungroup %>% 
  mutate(Part = as.factor(Part),
         term = reorder_within(term, n, Part)) %>% 
  
  ggplot(aes(term, n, fill = Part)) +
  geom_col(alpha = .7, show.legend = FALSE) +
  scale_fill_manual(values = viridis::inferno(12)) +
  labs(title = 'Occurrences of words',
       subtitle = 'Capital - Volume I, Karl Marx', x = NULL, y = 'Occurrences',
       caption = 'Source: Capital - Volume I, English edition first published in 1887\n @gustavoovital') +
  facet_wrap(~Part, scales = 'free') +
  scale_x_reordered() +
  coord_flip() +
  # scale_y_continuous(expand = c(0,0)) +
  
  theme_minimal() +
  theme(legend.position = 'none',
        plot.title.position = 'plot',
        text = element_text(family = 'Comic Sans MS'),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 20),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 13),
        plot.caption = element_text(size = 15))
