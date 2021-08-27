### Query results on Google Trends

# Load pckgs
library(gtrendsR)
library(dplyr)
library(forcats)
library(ggplot2)

# Define search keyword
# Comparing supermarkets
keyword_1 <- "Oda" # Norwegian grocery store start-up
keyword_2 <- "Lidl" # Traditional supermarket founded in Germany, now operating worldwide
keyword_3 <- "Glovo" # Spanish quick-commerce start-up

# Launch search query
trends <- gtrends(keyword = c(keyword_1,keyword_2, keyword_3),
                     time = paste("2014-01-01", Sys.Date()))


# Plot interest over time
trends$interest_over_time %>%
  mutate(date = as.Date(date),
         hits = as.numeric(ifelse(hits == "<1", 0, hits))) %>%  # hit count on 2014-03-01 is <1, which might be 0...
  ggplot(aes(x = date, y = hits, color = keyword)) +
  geom_line(size = 1) +
  scale_y_log10() +
  scale_x_date(breaks = "26 weeks") +
  theme(axis.text.x = element_text(angle = 75, vjust = 0.58)) +
  labs(title = "Interest over time",
       caption = paste0("Google Trends query results, ", Sys.Date()),
       color = 'Search Keywords')

ggsave(filename = "Output/1_interest_over_time.jpg")

# Plot interest by country
trends$interest_by_country %>%
  mutate(hits = as.numeric(hits)) %>%
  filter(!is.na(hits)) %>%
  group_by(keyword) %>%
  mutate(country = fct_lump(f = location, n = 5, w = hits)) %>%
  ungroup() %>% 
  filter(country != "Other") %>% 
  ggplot(aes(x = country, y = hits, fill = keyword)) +
  geom_col() +
  coord_flip() +
  facet_grid(keyword ~ ., scales = "free") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Interest by country",
       subtitle = "Top n countries per keyword",
       caption = paste0("Google Trends query results, ", Sys.Date())) +
  guides(fill = FALSE)

ggsave("Output/2_interest_by_country.jpg")
