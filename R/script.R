### DataCamp on Google Trends

# Load pckgs
library(gtrendsR)
library(dplyr)
library(forcats)
library(ggplot2)

# Launch search query
dc_trends <- gtrends(keyword = "Datacamp",
                     time = paste("2014-01-01", Sys.Date()))



# Plot interest over time
dc_trends$interest_over_time %>%
  mutate(date = as.Date(date),
         hits = as.numeric(ifelse(hits == "<1", 0, hits))) %>%  # hit count on 2014-03-01 is <1, which might be 0...
  ggplot(aes(x = date, y = hits)) +
  geom_line() +
  scale_x_date(breaks = "26 weeks") +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.58))

# Plot interest by country
dc_trends$interest_by_country %>% filter(!is.na(hits)) %>%
  mutate(location_2 = fct_lump_min(location, 10, w = hits),
         location_2 = fct_reorder(location_2, hits)) %>% 
  filter(location_2 != "Other") %>% 
  ggplot(aes(x = location_2, y = hits)) + geom_col() +
  coord_flip()
