### Query results on Google Trends

# Load pckgs
library(gtrendsR)
library(dplyr)
library(forcats)
library(ggplot2)

# Define search keyword
search_keyword <- "Oda"

# Launch search query
dc_trends <- gtrends(keyword = search_keyword,
                     time = paste("2014-01-01", Sys.Date()))

# Plot interest over time
dc_trends$interest_over_time %>%
  mutate(date = as.Date(date),
         hits = as.numeric(ifelse(hits == "<1", 0, hits))) %>%  # hit count on 2014-03-01 is <1, which might be 0...
  ggplot(aes(x = date, y = hits)) +
  geom_line() +
  scale_x_date(breaks = "26 weeks") +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.58)) +
  labs(title = "Interest over time",
       subtitle = paste0("search keyword: '", search_keyword, "'"),
       caption = paste0("Google Trends query results, ", Sys.Date()))

ggsave(filename = "Output/interest_over_time.png", dpi = 72)

# Plot interest by country
dc_trends$interest_by_country %>%
  mutate(hits = as.numeric(hits)) %>%
  filter(!is.na(hits)) %>%
  mutate(country = fct_lump_min(location, 10, w = hits),
         country = fct_reorder(country, hits)) %>% 
  filter(country != "Other") %>% 
  ggplot(aes(x = country, y = hits)) +
  geom_col() +
  coord_flip() +
  labs(title = "Interest by country",
       subtitle = paste0("search keyword: '", search_keyword, "'"),
       caption = paste0("Google Trends query results, ", Sys.Date()))

ggsave("Output/interest_by_country.png", dpi = 72)
